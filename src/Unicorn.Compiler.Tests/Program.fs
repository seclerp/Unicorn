module Program

open Expecto
open Unicorn.Tests

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args Parser.tests