module rec Unicorn.Syntax.Parser

open FParsec
open Unicorn.Models.AST

let private skipSpaces parser = spaces >>. parser .>> spaces

let private parseId =
    spaces >>. many1Chars (satisfy isAsciiLetter) |>> Id

let private parseCharacter =
    spaces
    >>. skipChar '\''
    >>. anyChar
    .>> skipChar '\''
    |>> Character

let private parseInteger = spaces >>. pint32 |>> Integer
let private parseString =
    spaces
    >>. skipChar '\"'
    >>. manyChars anyChar
    .>> skipChar '\"'
    |>> String

let private parseTrue = pstring "true" >>% Boolean true
let private parseFalse = pstring "false" >>% Boolean false
let private parseBoolean =
    spaces >>. choice [
        parseTrue
        parseFalse
    ]

let private parsePrimary =
    spaces >>. choice [
        parseId         |>> Primary.Id
        parseCharacter
        parseInteger
        parseString     |>> Primary.String
        parseTrue
        parseFalse
        parseBoolean
    ]

let private parseDeps =
    let parseStringRaw =
        spaces >>. (between (skipChar '"') (skipChar '"') (manyChars anyChar))
    
    parse {
        do! spaces
        do! skipString "deps"
        do! spaces
        do! skipChar '{'
        do! spaces
        let! deps = sepBy parseStringRaw (spaces >>. pchar ',' >>. spaces)
        do! spaces
        do! skipChar '}'
        return Deps deps
    }

let private parseParameter =
    parse {
        let! name = parseId
        do! spaces
        do! skipChar ':'
        do! spaces
        let! type' = parseId
        return (name, type')
    }
    
let private parseParameters =
    between
        (pchar '(')
        (pchar ')')
        (spaces >>. (many parseParameter) .>> spaces)

let private parseAssignmentRest =
    parse {
        do! spaces
        do! skipChar '='
        do! spaces
        return! parseExpressionStatement
    }

let private parseVariableDeclaration =
    parse {
        do! spaces
        do! skipString "let"
        do! spaces
        let! name = parseId
        do! spaces
        do! skipChar ':'
        do! spaces
        let! type' = parseId
        let! value = opt parseAssignmentRest
        return VariableDeclaration (name, type', value)
    }

let private parseAssignment =
    parse {
        do! spaces
        let! variable = parseId
        do! spaces
        do! skipChar '='
        do! spaces
        let! value = parseAssignmentRest
        return Assignment (variable, value)
    }

let operatorParser = new OperatorPrecedenceParser<Expression,unit,unit>()
let exprParser = operatorParser.ExpressionParser
let termParser =
    choice [
        spaces >>. (parsePrimary |>> Expression.Primary) .>> spaces
        between (spaces >>. pchar '(' .>> spaces) (spaces >>. pchar ')' .>> spaces) exprParser
    ]

operatorParser.TermParser <- termParser
operatorParser.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> Binary(x, Add, y)))
operatorParser.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> Binary(x, Subtract, y)))
operatorParser.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> Binary(x, Multiply, y)))
operatorParser.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> Binary(x, Divide, y)))
operatorParser.AddOperator(PrefixOperator("-", spaces, 2, true, fun x -> Unary(Subtract, x)))

let private parseExpression =
    exprParser |>> ExpressionStatement.Expression

let private parseIfStatement =
    parse {
        do! skipString "if" |> skipSpaces
        do! skipChar '(' |> skipSpaces
        let! expr = parseExpressionStatement |> skipSpaces
        do! skipChar ')' |> skipSpaces
        let! body = parseCompoundStatement |> skipSpaces
        return If (expr, body)
    }

let private parseExpressionStatement =
    choice [
        parseVariableDeclaration
        parseAssignment
        parseExpression
        parseIfStatement
        // TODO: Add parseForStatement
        // TODO: Add parseWhileStatement
        // TODO: Add parseDoWhileStatement
    ] |> skipSpaces

let private parseReturn =
    skipString "return" |> skipSpaces >>. opt parseExpressionStatement

let private parseStatement =
    spaces >>. choice [
        parseReturn                 |>> Statement.Return
        parseExpressionStatement    |>> Statement.ExpressionStatement
    ]

let private parseManyStatements =
    spaces >>. many parseStatement  |>> CompoundStatement.StatementList

let private parseSimpleCompoundStatement =
    parseStatement                  |>> CompoundStatement.Statement

let private parseCompoundStatement =
    choice [
        parseManyStatements
        parseSimpleCompoundStatement
    ]

let private parseFunctionBody =
    skipChar '{' >>. parseManyStatements |> skipSpaces .>> skipChar '}'

let private parseFunction =
    parse {
        do! spaces
        do! skipString "fun"
        do! spaces
        let! name = parseId
        do! spaces
        let! parameters = parseParameters
        do! spaces
        let! returnType = skipChar ':' >>. spaces >>. parseId
        do! spaces
        do! skipChar '{'
        let! body = parseFunctionBody
        do! spaces
        do! skipChar '}'
        return Function (name, parameters, returnType, body)
    }

let private parseTopLevelStatement =
    spaces >>. choice [
        parseDeps
        parseFunction
    ]

let private parseProgram sources =
    match run (many parseTopLevelStatement) sources with
    | Success(result, _, _)   -> result 
    | Failure(errorMsg, _, _) -> failwith errorMsg