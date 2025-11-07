open Parser
open FParsec
open System

let test p str =
    match run p str with
    | Success(result, _, position) when position.Index = str.Length
        -> printfn "Success: %A" result
    | Success(result, _, position) when position.Index <> str.Length
        -> printfn "Partial Success: %A, unconsumed input: %s" result (str.Substring(int32 position.Index))
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    | failure -> printfn "Generic Failure: %A" failure


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


type AlgebraicValue =
    | VEmpty
    | VInt64 of int64
    | VFloat of float
    | VString of string
    | VArray of AlgebraicValue list

type ApsEnvironment = {
    names: Map<string, AlgebraicValue>
}


let mutable globalEnv = {
    names = new Map<string, AlgebraicValue>([])
}

let evaluateExpression env aExpr =
    match aExpr with
    | AInt64 value -> VInt64 value
    | AFloat value -> VFloat value
    | AString value -> VString value
    | AEmpty -> VEmpty
    | _ -> raise (NotImplementedException(sprintf "Cannot evaluate %A" aExpr))

let interpret env statement =
    match statement with
    | SNamesDeclaration declarations ->
        //printfn "Declarations: %A" declarations
        let mutable names = env.names
        for (decl, arity) in declarations do
            match arity with
            | None ->
                names <- names.Add(decl, VEmpty)
            | Some arity ->
                names <- names.Add(decl, VArray (List.init arity (fun (_) -> VEmpty)))
        { env with names = names }
    | SAssignment (ident, index, expr) ->
        match env.names |> Map.tryFind ident with
        | Some value ->
            //printfn "Assign: %s = %A" ident expr
            match index with
            | Some index -> raise (NotImplementedException("not implemented"))
            | None ->
                let evaluatedValue =
                    evaluateExpression env expr
                { env with names = env.names |> Map.add ident evaluatedValue }
        | None ->
            printfn "Identifier %s not found" ident
            env
    | SAtomDeclaration atoms ->
        printfn "Atoms: %A" atoms
        env
    | SExpression expr ->
        printfn "Expression: %A" expr
        env
    | SMarkDescription marks ->
        printfn "Marks: %A" marks
        env
    | SEmpty -> env

let interpretProgram streamName str =
    let mutable initialPosition = 0
    let mutable haveToExit = false
    let str = str + ";"
    while (not haveToExit) do
        match runParserOnSubstring statement () streamName str initialPosition (str.Length - initialPosition) with
        | Success(result, _, position) when position.Index = str.Length
            ->
                haveToExit <- true
        | Success(result, _, position) when position.Index <> str.Length
            ->
                if int position.Index <= initialPosition then
                    haveToExit <- true
                initialPosition <- initialPosition + int position.Index
                //printfn "Partial Success: %A" result
                globalEnv <- interpret globalEnv result
                //printfn "%A" globalEnv
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
//testProgram statement "x := 1; NAME xx; MARKS comma(2,3,\"mod\");"
interpretProgram "test" "NAME task; task:=1;"
interpretProgram "test" ""
