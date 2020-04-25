module Unicorn.Ast

type Ast =
    | Ast of TopLevelStatement list

and TopLevelStatement =
    | Deps of string list
    | Function of name: Id * parameters: (Id * Id) list * returnType: Id * body : CompoundStatement

and CompoundStatement =
    | Statement of Statement
    | StatementList of Statement list

and Statement =
    | ExpressionStatement of ExpressionStatement
    | Return of ExpressionStatement option

and ExpressionStatement =
    | VariableDeclaration of Id * Id * ExpressionStatement option
    | If of condition: ExpressionStatement * body: CompoundStatement
    | Assignment of l: Id * r: ExpressionStatement
    | Expression of Expression

and Expression =
    | Binary of Expression * Op * Expression
    | Unary of Op * Expression
    | Primary of Primary

and Primary =
    | Id of Id
    | Character of char
    | Integer of int
    | String of String
    | Boolean of bool

and Id = Id of string

and String = String of string

and Op =
    | Add
    | Subtract
    | Multiply
    | Divide