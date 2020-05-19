module Unicorn.Tests.Parser

open Expecto
open Unicorn
open Unicorn.Compiler
open FParsec
open Unicorn.Compiler.

let private expectSuccessfulParsing (parser: Parser<'a, unit>) (sources: string) =
    let result = sources |> Parser.runParser parser "test"
    Expect.isOk result "Sources successfully parsed"

let private expectAst (parser: Parser<'a, unit>) (expectedAst: 'a) (sources: string) =
    let result = sources |> Parser.runParser parser "test"
    Expect.isOk result "Sources successfully parsed"
    match result with
    | Result.Error _ -> ()
    | Result.Ok ast -> Expect.equal ast expectedAst "AST are equal"

let private expectFailedParsing parser (sources: string) =
    let result = sources |> Parser.runParser parser "test"
    Expect.isError result "Parsing failed as expected"

[<Tests>]
let tests =
    testList "Parser tests" [
        testList "Identifier" [
            test "Parse 'abc213' identifier, should succeed" {
                let sources = "abc213"
                let expected = Ast.Id "abc213"
                expectAst Parser.idParser expected sources
            }
            test "Parse 'abc_123__' identifier, should succeed" {
                let sources = "abc_123__"
                let expected = Ast.Id "abc_123__"
                expectAst Parser.idParser expected sources
            }
            test "Parse '__' identifier, should succeed" {
                let sources = "__"
                let expected = Ast.Id "__"
                expectAst Parser.idParser expected sources
            }

            test "Parse 'abc_&$' identifier, should fail" {
                "abc_&$"
                |> expectFailedParsing Parser.idParser
            }
            test "Parse 123 identifier, should fail" {
                "123"
                |> expectFailedParsing Parser.idParser
            }
            test "Parse empty identifier, should fail" {
                ""
                |> expectFailedParsing Parser.idParser
            }
            test "Parse abc.213 identifier, should fail" {
                "abc.213"
                |> expectFailedParsing Parser.idParser
            }
            // TODO: Add UTF8 support
            test "Parse UTF8 identifier, should fail" {
                "фыв"
                |> expectFailedParsing Parser.idParser
            }
        ]

        testList "Dotted identifier" [
            test "Parse 'a.b.c' identifier, should succeed" {
                let sources = "a.b.c"
                let expected = Ast.IdPath [Ast.Id "a"; Ast.Id "b"; Ast.Id "c"]
                expectAst Parser.idPathParser expected sources
            }
            test "Parse 'a' identifier, should succeed" {
                let sources = "a"
                let expected = Ast.IdPath [Ast.Id "a"]
                expectAst Parser.idPathParser expected sources
            }

            test "Parse '.' identifier, should fail" {
                "."
                |> expectFailedParsing Parser.idPathParser
            }
            test "Parse 'a.' identifier, should fail" {
                "a."
                |> expectFailedParsing Parser.idPathParser
            }
        ]
        testList "Signature" [
            test "Parse 'simple' signature, should succeed" {
                let sources = "simple"
                let expected = Ast.IdPath  [ Ast.Id "simple" ] |> Ast.Type
                expectAst Parser.signatureExprParser expected sources
            }
            test "Parse 'simple -> simple' signature, should succeed" {
                let sources = "simple -> simple"
                let expected =
                    (Ast.IdPath [ Ast.Id "simple" ] |> Ast.Type, Ast.IdPath [ Ast.Id "simple" ] |> Ast.Type)
                    |> Ast.Arrow

                expectAst Parser.signatureExprParser expected sources
            }
            test "Parse '(simple * simple)' signature, should succeed" {
                let sources = "(simple -> simple)"
                let expected =
                    (Ast.IdPath [ Ast.Id "simple" ] |> Ast.Type, Ast.IdPath [ Ast.Id "simple" ] |> Ast.Type)
                    |> Ast.Arrow

                expectAst Parser.signatureExprParser expected sources
            }
            test "Parse 'simple * simple -> simple' signature, should succeed" {
                let sources = "simple * simple -> simple"
                let expected =
                    Ast.Arrow (Ast.Tuple
                        (Ast.Type
                             (Ast.IdPath [Ast.Id ("simple")]),
                         Ast.Type
                             (Ast.IdPath [Ast.Id ("simple")])
                        ), Ast.Type (Ast.IdPath [Ast.Id ("simple")])
                    )

                expectAst Parser.signatureExprParser expected sources
            }
            test "Parse '(simple * (simple -> simple))' signature, should succeed" {
                "(simple * (simple -> simple))"
                |> expectSuccessfulParsing Parser.signatureExprParser
            }
            test "Parse 'simple -> simple * simple -> simple' signature, should succeed" {
                "simple -> simple * simple -> simple"
                |> expectSuccessfulParsing Parser.signatureExprParser
            }

//            test "Parse 'simple -> simple * simple -> simple' signature, should succeed" {
//                "simple -> simple * simple -> simple"
//                |> expectSuccessfulParsing Parser.signatureExprParser
//            }
        ]

//        testList "General" [
////            test "Parse empty source, should succeed" {
////                let sources = ""
////                expectSuccessParsing sources
////            }
//        ]
//        testList "Module declaration" [
//            test "Parse empty module, should fail" {
//                let sources = @"module Example ="
//                expectFailedParsing Parser.moduleDefParser sources
//            }
//            test "Parse module with int value, should succeed" {
//                @"module Example {
//                    let value : int = 5
//                }"
//                |> expectSuccessParsing Parser.chunkParser
//            }
//        ]
    ]
