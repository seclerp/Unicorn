[<RequireQualifiedAccess>]
module Unicorn.Compiler.Terminals

// Characters
let [<Literal>] newline = '\n'
let [<Literal>] tab = '\t'
let [<Literal>] space = ' '
let [<Literal>] leftBrace = '('
let [<Literal>] rightBrace = ')'
let [<Literal>] singleQuote = '\''
let [<Literal>] doubleQuote = '\"'

// Keywords
let [<Literal>] moduleKeyword = "module"
let [<Literal>] letKeyword = "let"
let [<Literal>] mutableKeyword = "mutable"
let [<Literal>] openKeyword = "open"
let [<Literal>] matchKeyword = "match"
let [<Literal>] withKeyword = "with"
let [<Literal>] dataKeyword = "data"
let [<Literal>] unionKeyword = "union"
let [<Literal>] aliasKeyword = "alias"
let [<Literal>] trueKeyword = "true"
let [<Literal>] falseKeyword = "false"


// Operators
let [<Literal>] pipeOperator = "|>"
let [<Literal>] composeOperator = ">>"
let [<Literal>] plusOperator = "+"
let [<Literal>] minusOperator = "-"
let [<Literal>] starOperator = "*"
let [<Literal>] slashOperator = "/"
let [<Literal>] equalOperator = "="
let [<Literal>] arrowOperator = "->"
let [<Literal>] dotOperator = "."