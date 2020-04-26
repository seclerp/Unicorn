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
            | Parse parseArgs ->
                Commands.parse parseArgs
            | other ->
                Commands.invalid (string other)
        |> Commands.processResult
    with ex ->
        printfn "%s" ex.Message
    0