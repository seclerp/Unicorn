// Borrowed from
// https://github.com/stephan-tolksdorf/fparsec/wiki/Parsing-indentation-based-syntax-with-FParsec
module Unicorn.Compiler.ParserExtensions

open FParsec

let tabStopDistance = 8 // must be a power of 2

type TabState =
    { Indentation: int }
    with
       static member Create() = {Indentation = -1}

type CharStream = CharStream<TabState>
type Parser<'t> = Parser<'t, TabState>
type OperatorPrecedenceParser<'expression, 'afterString> = OperatorPrecedenceParser<'expression, 'afterString, TabState>

let skipIndentation (stream: CharStream) =
    let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
//    while stream.Peek() = '#' do
//        stream.SkipRestOfLine(false) // skip comment
//        indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)
    indentation

let indented p : Parser<_> =
    fun stream ->
        let state = stream.State
        let indentation = skipIndentation stream
        let expectedIndentation = stream.UserState.Indentation
        if indentation < expectedIndentation || stream.IsEndOfStream then
            stream.BacktrackTo(state)
            Reply(Error, NoErrorMessages)
        elif indentation = expectedIndentation then
            p stream
        else // indentation > expectedIndentation
            Reply(Error, messageError "wrong indentation")

let indentedBlock p =
    Inline.Many(stateFromFirstElement = (fun x -> [x]),
                foldState = (fun xs x -> x::xs),
                resultFromState = List.rev,
                firstElementParser = p,
                elementParser = indented p)

let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
    let indentedBlock = indentedBlock p
    fun stream ->
        let oldIndentation = stream.UserState.Indentation
        let indentation = skipIndentation stream
        if indentation <= oldIndentation then
            Reply(Error, expected (if indentation < 0 then "newline" else "indented " + label))
        else
            stream.UserState <- {stream.UserState with Indentation = indentation}
            let reply = indentedBlock stream
            if reply.Status = Ok then
                stream.UserState <- {stream.UserState with Indentation = oldIndentation}
            reply