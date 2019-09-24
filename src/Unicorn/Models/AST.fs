module Unicorn.Models.AST

type AST =
    | AST of TopLevelStatement list
and TopLevelStatement =
    | Deps of string list
    | Function of name: string * parameters: (string * string) list * returnType: string * body : Statement list
and Statement =
    | ExpressionStatement of ExpressionStatement
    | Return of ExpressionStatement option
and ExpressionStatement =
    | VariableDeclaration of string * string * ExpressionStatement option
    | Assignment of l: string * r: ExpressionStatement
    | Expression of Expression
and Expression =
    | Binary of Expression * Op * Expression
    | Unary of Op * Expression
    | Primary of Primary
and Primary =
    | Id of string
    | Character of char
    | Integer of int
    | String of string
    | Boolean of bool
and Op =
    | Plus
    | Minus
    | Mult
    | Divide