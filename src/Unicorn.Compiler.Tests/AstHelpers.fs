module Unicorn.Compiler.Tests.AstHelpers

open Unicorn.Compiler
open Unicorn.Compiler.Ast

let id = Id.Id
let idPath (path: string) = path.Split(".") |> List.ofArray |> List.map Id.Id |> IdPath.IdPath
let typedId idString typeSignature = TypedId.TypedId (id idString, typeSignature)

let add = Op.Add
let sub = Op.Subtract
let mul = Op.Multiply
let div = Op.Divide
let deg = Op.Degree
let pipe = Op.Pipe
let compose = Op.Compose

let primaryId = id >> Primary.Id
let primaryIdPath = idPath >> Primary.IdPath
let primaryChar = Primary.Character
let primaryInt = Primary.Integer
let primaryString = Ast.String >> Primary.String
let primaryBool = Primary.Boolean
let primaryList = Primary.List

let exprBinary = Expression.Binary
let exprUnary = Expression.Unary
let exprPrimary = Expression.Primary
let exprFCall path parameters = Expression.FunctionCall(idPath path, parameters)

let bindingVal id typeSignature value = LetBinding.Value(false, typedId id typeSignature, Some value)
let bindingMutable id typeSignature value = LetBinding.Value(true, typedId id typeSignature, Some value)
let bindingFunc id' params' returns body =
    let id = id id'
    let params' = params' |> List.map (fun id, type' -> typedId id type')
    LetBinding.Function(id, params', returns, body)

let stmtBindingVal id typeSignature value = bindingVal id typeSignature value |> ExpressionStatement.LetBinding
let stmtBindingMutableVal id typeSignature value = bindingMutable id typeSignature value |> ExpressionStatement.LetBinding
let stmtBindingFunc id' params' returns body =
    bindingFunc id' params' returns body |> ExpressionStatement.LetBinding
// TODO: Complete list of functions for AST
