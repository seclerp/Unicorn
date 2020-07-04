namespace Unicorn

module Optimizer =
    open System
    open Unicorn
    open Unicorn.IL
    open Unicorn.Semantic

    let mapAstType =
        function
        | Ast.NoneType  -> typeof<Void>
        | Ast.Bool      -> typeof<bool>
        | Ast.Int       -> typeof<int>
        | Ast.Double    -> typeof<float>
        | Ast.String    -> typeof<string>

    let private mapMethodToAstId (method: ILMethod) : Ast.Identifier =
        method.Name

    let private removeUnusedMethods (ir: ILClass) (semantic: SemanticAnalysisResult) : ILClass =
        { ir with Methods = ir.Methods
              |> List.filter (fun method -> semantic.FunctionCalls.ContainsKey(method.Name)
                                            && mapAstType semantic.FunctionCalls.[method.Name].ReturnType = method.ReturnType
                                            || method.Name = "main") }

    let optimize (ir: ILClass) (semantic: SemanticAnalysisResult) =
        removeUnusedMethods ir semantic