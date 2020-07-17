open Argu

open Unicorn.Cli

[<EntryPoint>]
let main args =
    try
        let argumentsParser = ArgumentParser.Create<Argument>("unicorn")
        let results = argumentsParser.ParseCommandLine(inputs = args, raiseOnUsage = true)
        if results.Contains Version then
            Commands.version ()
        else
            match results.GetSubCommand() with
            | Compile compileArgs ->
                Commands.compile compileArgs
            | other ->
                Commands.invalid (string other)
        |> Commands.processResult
    with ex ->
        printfn "%A" ex
    0