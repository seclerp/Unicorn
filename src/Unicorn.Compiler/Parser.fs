module rec Unicorn.Parser

open FParsec

open Unicorn.Compiler
open Unicorn.Compiler.Ast

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let private tabOrSpace =
    pchar Terminals.space <|> pchar Terminals.tab

let private tabOrSpaces =
    many1 (pchar Terminals.space <|> pchar Terminals.tab)

let private skipManySpacesAndNewline =
    skipMany tabOrSpace .>> skipNewline .>> skipMany tabOrSpace

// Skip whitespaces and newline, but not next line (it is covered by indentation)
let private skipSpaces : Parser<unit, unit> =
    skipMany tabOrSpace
    .>> skipSatisfy ((=) Terminals.newline) // Skip ending newline

let private skipSpaces1 : Parser<unit, unit> =
    skipMany1 tabOrSpace
    .>> skipSatisfy ((=) Terminals.newline) // Skip ending newline

let private expectNewline : Parser<unit, unit> =
    skipMany (choice [pchar Terminals.space; pchar Terminals.tab])
    .>> pchar Terminals.newline

let idParser : Parser<Id, unit> =
    parse {
        let! firstSymbol = choice [asciiLetter; pchar '_']
        let! rest = manyChars <| choice [asciiLetter; digit; pchar '_']
        return sprintf "%c%s" firstSymbol rest
    } .>> spaces
    |>> Id.Id

let idPathParser =
    sepBy idParser (pstring Terminals.dotOperator)
    |>> IdPath.IdPath

let typedIdParser =
    parse {
        let! id = idParser <!> "parseId" .>> spaces
        do! skipChar Terminals.colon .>> spaces
        let! type' = signatureExprParser <!> "parseSignatureExpression"
        return TypedId(id, type')
    }

let characterParser =
    skipChar Terminals.singleQuote
    >>. anyChar
    .>> skipChar Terminals.singleQuote
    |>> Character

let integerParser = pint32 |>> Integer
let stringParser =
    skipChar Terminals.doubleQuote
    >>. manyChars anyChar
    .>> skipChar Terminals.doubleQuote
    |>> String.String

let trueParser = pstring Terminals.trueKeyword >>% Boolean true
let falseParser = pstring Terminals.falseKeyword >>% Boolean false
let parseBoolean : Parser<Primary, unit> = trueParser <|> falseParser

let primaryParser =
    choice [
        idParser         |>> Primary.Id
        characterParser
        integerParser
        stringParser     |>> Primary.String
        trueParser
        falseParser
        parseBoolean
    ]

let private operatorParser = OperatorPrecedenceParser<Expression, unit, unit>()
let private exprParser = operatorParser.ExpressionParser
let private parseTerm =
    choice [
        (primaryParser |>> Expression.Primary) .>> spaces
        between (pchar Terminals.leftBrace .>> spaces) (pchar Terminals.rightBrace .>> spaces) exprParser
    ]
let private populateOperatorParser =
    operatorParser.TermParser <- parseTerm <!> "term"
    operatorParser.AddOperator(InfixOperator(Terminals.pipeOperator, spaces, 1, Associativity.Left, fun x y -> Binary(x, Pipe, y)))
    operatorParser.AddOperator(InfixOperator(Terminals.composeOperator, spaces, 2, Associativity.Left, fun x y -> Binary(x, Compose, y)))
    operatorParser.AddOperator(InfixOperator(Terminals.plusOperator, spaces, 3, Associativity.Left, fun x y -> Binary(x, Add, y)))
    operatorParser.AddOperator(InfixOperator(Terminals.minusOperator, spaces, 3, Associativity.Left, fun x y -> Binary(x, Subtract, y)))
    operatorParser.AddOperator(InfixOperator(Terminals.starOperator, spaces, 4, Associativity.Left, fun x y -> Binary(x, Multiply, y)))
    operatorParser.AddOperator(InfixOperator(Terminals.slashOperator, spaces, 5, Associativity.Left, fun x y -> Binary(x, Divide, y)))
    operatorParser.AddOperator(PrefixOperator(Terminals.minusOperator, spaces, 2, true, fun x -> Unary(Subtract, x)))

let private block blockParser =
    between
        (pchar Terminals.leftSquareBrace)
        (pchar Terminals.rightSquareBrace)
        (spaces >>. blockParser)

let expressionParser =
    exprParser |>> ExpressionStatement.Expression

let private signatureOperatorParser = OperatorPrecedenceParser<SignatureExpression, unit, unit>()
let signatureExprParser : Parser<SignatureExpression, unit> =
   signatureOperatorParser.ExpressionParser
let private signatureTermParser =
    choice [
        idPathParser |>> Type
        between (pchar Terminals.leftBrace) (pchar Terminals.rightBrace) signatureExprParser
    ]
let private populateSignatureParser =
    signatureOperatorParser.TermParser <- signatureTermParser
    signatureOperatorParser.AddOperator(InfixOperator(Terminals.arrowOperator, spaces, 1, Associativity.Left, fun x y -> SignatureExpression.Arrow(x, y)))
    signatureOperatorParser.AddOperator(InfixOperator(Terminals.starOperator, spaces, 2, Associativity.Left, fun x y -> SignatureExpression.Tuple(x, y)))

let assignmentRestParser =
    parse {
        do! skipString Terminals.equalOperator .>> spaces
        return! bindingBodyParser .>> spaces
    }

let valueBindingParser =
    parse {
        do! skipString Terminals.letKeyword .>> spaces1
        let! mutable' = opt (skipString Terminals.mutableKeyword .>> spaces1)
        let! typedId = typedIdParser <!> "parseTypedId" .>> spaces
        let! value = (assignmentRestParser <!> "parseAssignmentRest" .>> spaces) |> opt
        return LetBinding.Value (false, typedId, value)
    }

let parameterParser =
    parse {
        do! skipChar Terminals.leftBrace
        do! spaces
        let! typedId = typedIdParser
        do! spaces
        do! skipChar Terminals.rightBrace
        return typedId
    }

let functionBindingParser =
    parse {
        do! skipString Terminals.letKeyword
        do! spaces
        let! name = idParser
        do! spaces
        let! params' = sepBy parameterParser spaces
        do! spaces
        let! results = signatureExprParser
        do! skipSpaces
        let! body = assignmentRestParser
        return LetBinding.Function (name, params', results, body)
    }

let letBindingParser =
    choice [
        valueBindingParser <!> "let"
        functionBindingParser
    ]

let openParser =
    skipString Terminals.openKeyword .>> spaces >>. idPathParser
    |>> InModuleDecl.Open

let private bindingBodyParser =
    choice [
        //block parseExpressionStatement
        expressionStatementParser
    ]

let assignmentParser =
    parse {
        let! id = idPathParser
        do! spaces
        do! skipString Terminals.equalOperator
        do! spaces
        let! value = assignmentRestParser
        return Assignment (id, value)
    }

//let private parseUnionDeconstructValue =
//    choice [
//        pchar Terminals.leftBrace
//    ]
//
//let private parseUnionDeconstruct =
//    parse {
//        do! skipChar
//    }
//
//let private parsePatternExpression =
//    ()

let expressionStatementParser : Parser<ExpressionStatement, unit> =
    choice [
        letBindingParser |>> ExpressionStatement.LetBinding
        // TODO: parsePatternMatch
        assignmentParser
        expressionParser
    ] .>> skipSpaces

let private sepBySemicolonOrNewline parser =
    sepBy parser <|
        choice [
            skipMany (tabOrSpaces >>. skipChar Terminals.newline .>> tabOrSpaces)
            tabOrSpaces >>. skipChar Terminals.semicolon .>> spaces
        ]

let dataDeclParser =
    parse {
        do! skipString Terminals.dataKeyword .>> spaces
        let! name = idParser .>> spaces
        let! fields = block (sepBySemicolonOrNewline typedIdParser)
        return Data(name, fields)
    }

let unionDeclParser =
    let parseUnionCase =
        choice [
            idParser .>>. signatureExprParser |>> TypedId |>> UnionCase.Typed
            idParser |>> UnionCase.Untyped
        ] .>> expectNewline

    parse {
        do! skipString Terminals.unionKeyword .>> spaces
        let! name = idParser .>> spaces
        let! fields = block (sepBySemicolonOrNewline parseUnionCase)
        return Union(name, fields)
    }

let singleUnionDeclParser =
    skipString Terminals.unionKeyword .>> spaces >>. typedIdParser
    |>> TypeDecl.SingleUnion

let aliasDeclParser =
    parse {
        do! skipString Terminals.aliasKeyword .>> spaces
        let! alias = idParser .>> spaces
        do! skipString Terminals.equalOperator .>> spaces
        let! for' = idPathParser .>> spaces
        return Alias(alias, for')
    }

let typeDeclParser : Parser<TypeDecl, unit> =
    choice [
        dataDeclParser
        unionDeclParser
        singleUnionDeclParser
        aliasDeclParser
    ] .>> skipSpaces

let private moduleBodyParser =
    choice [
        openParser
        typeDeclParser |>> InModuleDecl.TypeDecl
        letBindingParser |>> InModuleDecl.LetBinding
    ]

let moduleDefParser =
    parse {
        do! skipString Terminals.moduleKeyword .>> spaces
        let! name = idPathParser .>> spaces
        let! decl = block (many moduleBodyParser) .>> spaces
        return ModuleDef(name, decl)
    }

let chunkParser =
    spaces >>. many (moduleDefParser .>> spaces)

let toResult chunkLength =
    function
    | Success (result, _, _) ->
        Result.Ok result
    | Failure (errorMsg, _, _) ->
        Result.Error errorMsg

let runParser parser chunkName (sources: string) =
    let chunkLength = int64 sources.Length
    let state = ()
    runParserOnString (parser .>> eof) state chunkName sources
    |> toResult chunkLength

let parseChunk chunkName (sources: string) =
    runParser chunkParser chunkName sources