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

test int64Number "1"
test floatNumber "1.25"
test floatBetweenBrackets "[1.25]"
test namesDeclaration "NAMES S"
test namesDeclaration "NAME S"
test namesDeclaration "NAMES S1,S2"
test namesDeclaration "NAMES S1, S2"
test arity "4"
test arity "UNDEF"
test markDescriptionElement "X(3)"
test markDescriptionElement "X(UNDEF)"
test markDescriptionElement "comma( 2,  7, \",\")"
test markDescriptionElement "comma( 2,  7, \"-->\")"
test markDescriptionElement "comma( 2,  7, \"mod\")"
test markDescriptionElement "comma( 2,  7, \"else\")"
test markDescription "MARK X(3), X(UNDEF), comma( 2,  7, \",\")"
test markDescription "MARK X(3), /*some comment*/ X(UNDEF), comma( 2,  7, \",\")"
test algebraicExpression "x"
test algebraicExpression "2"
test algebraicExpression "2.3"
test algebraicExpression "()"
test algebraicExpression "\"some string\""
test algebraicExpression "VAL z"
test algebraicExpression "(x)"
test algebraicExpression "(2)"
test algebraicExpression "( 2 )"
test algebraicExpression "F(2)"
test algebraicExpression "F ( 2)"
test algebraicExpression "F ( 2 )"
test algebraicExpression "x + y"
test algebraicExpression "(x + y)"
test algebraicExpression "(x + y) * z"
test algebraicExpression "(x + y) * z + w"
test algebraicExpression "proc()loc(Term)"
test algebraicExpression "a[5]"
test algebraicExpression "a[5] := 10"
test algebraicExpression "R:=rs()x"
test algebraicExpression "F(0) = 1,F(1) = 1"
test algebraicExpression
    """R:=rs(n)(
    F(0) = 1,
    F(1) = 1,
    F(n) = F(n-1)+F(n-2)
)
    """
test statement "x := 1;"
test statement "NAME xx;"
test statement "MARKS comma(2,3,\"mod\");"

// TODO: Incorrectly parse as list of algebraic expressions
testProgram statement "x := 1; NAME xx; MARKS comma(2,3,\"mod\");"
