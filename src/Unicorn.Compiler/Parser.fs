module rec Unicorn.Parser

open FParsec

open Unicorn.Compiler.Ast
open Unicorn.Compiler.ParserExtensions

// Skip whitespaces and newline, but not next line (it is covered by indentation)
let private skipSpaces : Parser<unit> =
    skipMany (choice [pchar ' '; pchar '\t'])
    .>> skipSatisfy ((=) '\n') // Skip ending newline

let private skipSpaces1 : Parser<unit> =
    skipMany1 (choice [pchar ' '; pchar '\t'])
    .>> skipSatisfy ((=) '\n') // Skip ending newline

let private parseId : Parser<Id>  =
    many1Chars (satisfy isAsciiLetter)
    .>> skipSpaces
    |>> Id.Id

let private parseIdPath =
    sepBy parseId (pchar '.')
    .>> skipSpaces
    |>> IdPath

let private parseTypedId =
    parseId
    .>>. parseSignatureExpression
    .>> skipSpaces
    |>> TypedId

let private parseCharacter =
    skipChar '\''
    >>. anyChar
    .>> skipChar '\''
    .>> skipSpaces
    |>> Character

let private parseInteger = spaces >>. pint32 |>> Integer
let private parseString =
    skipChar '\"'
    >>. manyChars anyChar
    .>> skipChar '\"'
    .>> skipSpaces
    |>> String.String

let private parseTrue = pstring "true" >>% Boolean true
let private parseFalse = pstring "false" >>% Boolean false
let private parseBoolean =
    choice [
        parseTrue
        parseFalse
    ] .>> skipSpaces

let private parsePrimary =
    choice [
        parseId         |>> Primary.Id
        parseCharacter
        parseInteger
        parseString     |>> Primary.String
        parseTrue
        parseFalse
        parseBoolean
    ] .>> skipSpaces

let operatorParser = OperatorPrecedenceParser<Expression, unit>()
let exprParser = operatorParser.ExpressionParser
let termParser =
    choice [
        spaces >>. (parsePrimary |>> Expression.Primary) .>> spaces
        between (spaces >>. pchar '(' .>> spaces) (spaces >>. pchar ')' .>> spaces) exprParser
    ]

operatorParser.TermParser <- termParser
operatorParser.AddOperator(InfixOperator("|>", spaces, 1, Associativity.Left, fun x y -> Binary(x, Pipe, y)))
operatorParser.AddOperator(InfixOperator(">>", spaces, 2, Associativity.Left, fun x y -> Binary(x, Compose, y)))
operatorParser.AddOperator(InfixOperator("+", spaces, 3, Associativity.Left, fun x y -> Binary(x, Add, y)))
operatorParser.AddOperator(InfixOperator("-", spaces, 3, Associativity.Left, fun x y -> Binary(x, Subtract, y)))
operatorParser.AddOperator(InfixOperator("*", spaces, 4, Associativity.Left, fun x y -> Binary(x, Multiply, y)))
operatorParser.AddOperator(InfixOperator("/", spaces, 5, Associativity.Left, fun x y -> Binary(x, Divide, y)))
operatorParser.AddOperator(PrefixOperator("-", spaces, 2, true, fun x -> Unary(Subtract, x)))

let private parseExpression =
    exprParser |>> ExpressionStatement.Expression

let private parseTupleSignature =
    parse {
        do! skipChar '('
        do! spaces
        let! inners = sepBy parseSignatureExpression (pchar '*' .>> spaces)
        do! spaces
        do! skipChar ')'
        return SignatureExpression.Tuple inners
    } .>> skipSpaces

let private parseArrowSignature =
    parse {
        let! signatureA = parseSignatureExpression
        do! spaces1
        do! skipString "->"
        let! signatureB = parseSignatureExpression
        return SignatureExpression.Arrow (signatureA, signatureB)
    } .>> skipSpaces

let private parseSignatureExpression =
    choice [
        parseTupleSignature
        parseArrowSignature
        parseIdPath |>> SignatureExpression.Type
    ] .>> skipSpaces

let private parseAssignmentRest =
    parse {
        do! skipChar '='
        do! skipSpaces
        return! parseBindingBody
    } .>> skipSpaces

let private parseValueBinding =
    parse {
        do! skipString "let"
        do! spaces
        let! mutable' = pstring "mutable" |> opt
        do! spaces
        let! typedId = parseTypedId
        do! spaces
        let! value = parseAssignmentRest |> opt
        return LetBinding.Value (mutable' |> Option.isSome, typedId, value)
    }

let private parseParameter =
    parse {
        do! skipChar '('
        do! spaces
        let! typedId = parseTypedId
        do! spaces
        do! skipChar ')'
        return typedId
    } .>> skipSpaces

let private parseFunctionBinding =
    parse {
        do! skipString "let"
        do! spaces
        let! name = parseId
        do! spaces
        let! params' = sepBy parseParameter spaces
        do! spaces
        let! results = parseSignatureExpression
        do! skipSpaces
        let! body = parseAssignmentRest
        return LetBinding.Function (name, params', results, body)
    }

let private parseLetBinding =
    choice [
        parseValueBinding
        parseFunctionBinding
    ] .>> skipSpaces

let private parseOpen =
    pstring "open" .>> spaces >>. parseIdPath
    .>> skipSpaces
    |>> InModuleDecl.Open

let private parseBindingBody =
    (indentedMany1 parseExpressionStatement "binding body") // Multiline
    <|> many parseExpressionStatement // Oneline

let private parseAssignment =
    parse {
        do! spaces
        let! id = parseIdPath
        do! spaces
        do! skipChar '='
        do! spaces
        let! value = parseAssignmentRest
        return Assignment (id, value)
    }

let private parseExpressionStatement =
    choice [
        parseLetBinding |>> ExpressionStatement.LetBinding
        // TODO: parsePatternMatch
        parseAssignment
        parseExpression
    ] .>> skipSpaces

let private parseTypeDecl : Parser<TypeDecl> =
    choice [
        // TODO: parseData
        // TODO: parseUnion
        // TODO: parseSingleUnion
        // TODO: parseAlias
    ] .>> skipSpaces

let private parseModuleBody =
    many <|
        choice [
            parseOpen
            parseTypeDecl |>> InModuleDecl.TypeDecl
            parseLetBinding |>> InModuleDecl.LetBinding
        ] .>> skipSpaces

let private parseModuleDef =
    parse {
        do! skipString "module" .>> spaces
        let! name = parseIdPath .>> spaces
        let! decl = parseModuleBody .>> spaces
        return ModuleDef(name, decl)
    } .>> skipSpaces

let private parseChunk' =
    indentedMany1 parseModuleDef "module" .>> spaces

let parseChunk chunkName source =
    let parserInitialState = TabState.Create()
    match runParserOnString parseChunk' parserInitialState chunkName source with
    | Success (result, _, _) ->
        Result.Ok result
    | Failure (errorMsg, _, _) ->
        Result.Error errorMsg