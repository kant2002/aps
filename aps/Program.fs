open Parser
open FParsec

let test p str =
    match run p str with
    | Success(result, _, position) when position.Index = str.Length
        -> printfn "Success: %A" result
    | Success(result, _, position) when position.Index <> str.Length
        -> printfn "Partial Success: %A, unconsumed input: %s" result (str.Substring(int32 position.Index))
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let testProgram p str =
    let mutable initialPosition = 0
    let mutable haveToExit = false
    while (not haveToExit) do
        match runParserOnSubstring p () "test-stream" str initialPosition (str.Length - initialPosition) with
        | Success(result, _, position) when position.Index = str.Length
            ->
                printfn "Success: %A" result
                haveToExit <- true
        | Success(result, _, position) when position.Index <> str.Length
            ->
                if int position.Index <= initialPosition then
                    haveToExit <- true
                initialPosition <- initialPosition + int position.Index
                printfn "Partial Success: %A" result
        | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg
            haveToExit <- true
        | failure ->
            printfn "Generic Failure: %A" failure
            haveToExit <- true

// test floatBetweenBrackets "[1.25]"
// test arity "4"
// test arity "UNDEF"
// test markDescriptionElement "X(3)"
// test markDescriptionElement "X(UNDEF)"
// test markDescriptionElement "comma( 2,  7, \",\")"
// test markDescriptionElement "comma( 2,  7, \"-->\")"
// test markDescriptionElement "comma( 2,  7, \"mod\")"
// test markDescriptionElement "comma( 2,  7, \"else\")"

// test algebraicExpression "R:=rs()x"
// test algebraicExpression "F(0) = 1,F(1) = 1"
// test algebraicExpression
//     """R:=rs(n)(
//     F(0) = 1,
//     F(1) = 1,
//     F(n) = F(n-1)+F(n-2)
// )
//     """
// test statement "x := 1;"
// test statement "NAME xx;"
// test statement "MARKS comma(2,3,\"mod\");"

// TODO: Incorrectly parse as list of algebraic expressions
testProgram statement "x := 1; NAME xx; MARKS comma(2,3,\"mod\");"
