module Program

open Expecto

[<EntryPoint>]
let main args =
    let config = defaultConfig
    runTestsInAssembly config args