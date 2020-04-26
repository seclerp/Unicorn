[<RequireQualifiedAccess>]
module Unicorn.Cli.Commands

open System.IO
open System.Reflection
open System.Text.Json
open Argu
open Unicorn

let version () =
    Assembly.GetExecutingAssembly().GetName().Version.ToString(3)
    |> printfn "%s"
    |> Ok

let private jsonSerializationOptions = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = null)

let parse (argResults: ParseResults<ParseArgument>) : Result<unit, string> =
    let inputFile = argResults.GetResult(Input_File)
    let defaultOutputFile = inputFile |> Path.GetFileNameWithoutExtension |> sprintf "%s.ast"
    let outputFile = argResults.GetResult(Output_File, defaultOutputFile)
    let outputFormat = argResults.GetResult(Output_Format, ParseOutputFormat.Json)

    let formatResult result =
        match outputFormat with
        | ParseOutputFormat.Json -> JsonSerializer.Serialize(result, jsonSerializationOptions) |> Ok
        | _ -> "Incorrect output format" |> Error

    if File.Exists inputFile |> not then
        Error "Specified input file not found"
    else
        File.ReadAllText(inputFile)
        |> Parser.parseChunk inputFile
        |> Result.mapError(fun error -> error.ToString())
        |> Result.bind formatResult
        |> Result.map (fun result -> File.WriteAllText(outputFile, result))

let invalid (command: string) =
    sprintf "Command '%s' is invalid" command |> Error

let processResult =
    function
    | Ok () -> ()
    | Error error -> printfn "%s" error