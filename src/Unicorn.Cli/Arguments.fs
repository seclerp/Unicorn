namespace Unicorn.Cli

open Argu

type ParseOutputFormat =
    | Json = 0

type CompileArgument =
    | [<AltCommandLine("-i")>] [<Mandatory>] Input_File of string
    | [<AltCommandLine("-o")>] Output_File of string
    | Include_Ast
    | Include_Semantic
    | Include_IR
    | Include_IR_Optimized
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Input_File _ -> "Specify input .uni file to parse."
            | Output_File _ -> "Specify output file (without extension) that will be used to output compiled file."
            | Include_Ast _ -> "Include parser analyzer result as <output-file>.ast.json file."
            | Include_Semantic _ -> "Include semantic analyzer result as <output-file>.semantic.json file."
            | Include_IR _ -> "Include non-optimized intermediate representation result as <output-file>.ir-raw.json file."
            | Include_IR_Optimized _ -> "Include optimized intermediate representation result as <output-file>.ir.json file."

type Argument =
    | Version
    | [<CliPrefix(CliPrefix.None)>] Compile of ParseResults<CompileArgument>
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Version _ -> "Get version on Unicorn CLI."
            | Compile _ -> "Compile specified .uni file."