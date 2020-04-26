module Unicorn.Compiler.Errors

type CompilerError =
    | SyntaxError of int * string
    | SemanticError of int * string
    | LinkingError of int * string
    | EmitterError of int * string
    override error.ToString() =
        match error with
        | SyntaxError (code, msg)   -> sprintf "PR%i: %s" code msg
        | SemanticError (code, msg) -> sprintf "SC%i: %s" code msg
        | LinkingError (code, msg)  -> sprintf "LK%i: %s" code msg
        | EmitterError (code, msg)  -> sprintf "EM%i: %s" code msg