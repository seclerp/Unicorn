namespace Unicorn.Cli

open Argu

type ParseOutputFormat =
    | Json = 0

type ParseArgument =
    | [<AltCommandLine("-i")>] [<Mandatory>] Input_File of string
    | [<AltCommandLine("-o")>] Output_File of string
    | Output_Format of ParseOutputFormat
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Input_File _ -> "Specify input .uni file to parse."
            | Output_File _ -> "Specify output file that will be used to store serialized AST."
            | Output_Format _ -> "Specify which format will be used to serialize resulting AST."

type Argument =
    | Version
    | [<CliPrefix(CliPrefix.None)>] Parse of ParseResults<ParseArgument>
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Version _ -> "Get version on Unicorn CLI."
            | Parse _ -> "Parse specified .uni file and save AST."