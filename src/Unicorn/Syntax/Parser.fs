module rec Unicorn.Syntax.Parser

open FParsec
open Unicorn.Models.AST

let private parsePlus = pchar '+' >>% Plus
let private parseMinus = pchar '-' >>% Minus
let private parseMult = pchar '*' >>% Mult
let private parseDivide = pchar '/' >>% Divide

let private pop =
    spaces >>. choice [
        parsePlus
        parseMinus
        parseMult
        parseDivide
    ]

let private parseId =
    spaces >>. many1Chars (satisfy isAsciiLetter) |>> Id
let private parsePrimaryId =
    parseId |>> Primary.Id

let private parseCharacter =
    spaces
    >>. skipChar '\''
    >>. anyChar
    .>> skipChar '\''
    |>> Character

let private parseInteger = spaces >>. pint32 |>> Integer
let private parseString =
    spaces
    >>. pchar '\"'
    >>. manyChars anyChar
    .>> pchar '\"'
    |>> String
let private parsePrimaryString =
    parseString |>> Primary.String

let private parseTrue = pstring "true" >>% Boolean true
let private parseFalse = pstring "false" >>% Boolean false
let private parseBoolean =
    spaces >>. choice [
        parseTrue
        parseFalse
    ]

let private parsePrimary =
    spaces >>. choice [
        parsePrimaryId
        parseCharacter
        parseInteger
        parsePrimaryString
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
        return VariableDeclaration (name, type')
    }

let private parseExpressionStatement =
    spaces >>. choice [
        parseVariableDeclaration
        parseAssignment
        parseExpression
    ]

let private parseReturn =
    spaces >>. skipString "return" >>. spaces >>. parseExpressionStatement

let private parseStatement =
    spaces >>. choice [
        parseExpressionStatement
        parseReturn
    ]

let private parseFunctionBody =
    spaces >>. many parseStatement

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

let private parseProgram =
    spaces
    >>. many parseTopLevelStatement
    >>. eof