[<RequireQualifiedAccess>]
module Unicorn.Cli.Commands

open System.IO
open System.Reflection
open Argu

open Unicorn
open ResultBuilder
open Unicorn.IL

let version () =
    Assembly.GetExecutingAssembly().GetName().Version.ToString(3)
    |> printfn "%s"
    |> Ok

let private saveResultIfNeeded (isNeeded: bool) (result: 'a) (file: string) =
    if isNeeded then
        File.WriteAllText(file, sprintf "%A" result)

let private executeCompile inputFile includeAst includeSemantic outputFileAst outputFileSemantic =
    result {
        let sourceCode = File.ReadAllText(inputFile)
        let! parserResult = Parser.parse sourceCode |> Result.mapError(fun error -> error.ToString())
        saveResultIfNeeded includeAst parserResult outputFileAst

        let! semanticResult = Semantic.analyze parserResult |> Result.mapError(fun error -> error.ToString())
        saveResultIfNeeded includeSemantic semanticResult outputFileSemantic

        let irBuilder = ILBuilder(semanticResult)
        let irResult = irBuilder.BuildClass(parserResult)
        let optimizedIrResult = Optimizer.optimize irResult semanticResult

        return optimizedIrResult
    }

let private saveResult outputFile ir =
    Compiler.compileIrToFile ir outputFile
    Compiler.publishRuntimeConfig outputFile
    outputFile

let private informCompleted file =
    printfn "Successfully compiled. Result saved to %s" file

let compile (argResults: ParseResults<CompileArgument>) : Result<unit, string> =
    let inputFile = argResults.GetResult(Input_File)
    let defaultOutputFile = inputFile |> Path.GetFileNameWithoutExtension |> sprintf "%s.dll"
    let outputFile = argResults.GetResult(Output_File, defaultOutputFile)
    let includeAst = argResults.Contains(Include_Ast)
    let includeSemantic = argResults.Contains(Include_Semantic)
    let includeNonOptimizedIr = argResults.Contains(Include_IR)
    let includeOptimizedIr = argResults.Contains(Include_IR_Optimized)

    let outputFileWithExtension = outputFile |> sprintf "%s.dll"
    let outputFileAst = outputFile |> sprintf "%s.ast.json"
    let outputFileSemantic = outputFile |> sprintf "%s.semantic.json"
    let outputFileNonOptimizedIr = outputFile |> sprintf "%s.ir-raw.json"
    let outputFileOptimizedIr = outputFile |> sprintf "%s.ir.json"

    if File.Exists inputFile |> not then
        Error "Specified input file not found"
    else
        let fullInputFilePath = Path.GetFullPath(inputFile)
        executeCompile fullInputFilePath includeAst includeSemantic outputFileAst outputFileSemantic
        |> Result.map (saveResult outputFileWithExtension)
        |> Result.map informCompleted

let invalid (command: string) =
    sprintf "Command '%s' is invalid" command |> Error

let processResult =
    function
    | Ok () -> ()
    | Error error -> printfn "%s" error