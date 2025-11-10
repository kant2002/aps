open Parser
open FParsec
open System
open System.IO

let test p str =
    match run p str with
    | Success(result, _, position) when position.Index = str.Length -> printfn "Success: %A" result
    | Success(result, _, position) when position.Index <> str.Length ->
        printfn "Partial Success: %A, unconsumed input: %s" result (str.Substring(int32 position.Index))
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    | failure -> printfn "Generic Failure: %A" failure


let testProgram p str =
    let mutable initialPosition = 0
    let mutable haveToExit = false

    while (not haveToExit) do
        match runParserOnSubstring p () "test-stream" str initialPosition (str.Length - initialPosition) with
        | Success(result, _, position) when position.Index = str.Length ->
            printfn "Success: %A" result
            haveToExit <- true
        | Success(result, _, position) when position.Index <> str.Length ->
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
    | VRewriteSystem of string list * AlgebraicExpression

type ApsEnvironment = { names: Map<string, AlgebraicValue> }


let mutable globalEnv = { names = new Map<string, AlgebraicValue>([]) }

let evaluateExpression env aExpr =
    match aExpr with
    | AInt64 value -> VInt64 value
    | AFloat value -> VFloat value
    | AString value -> VString value
    | AEmpty -> VEmpty
    | ARewriteSystemExpression(vars, rules) ->
        let rec collectVars (initState: string list) vars =
            match vars with
            | AInfixExpression(left, op, right) ->
                if (op <> ",") then
                    raise (InvalidOperationException("Identifiers should be comma separated"))
                else
                    collectVars (collectVars initState left) right
            | AAtom ident -> initState |> List.append [ ident ]
            | _ -> raise (InvalidOperationException("Rewrite system variables should be always identifiers"))

        let rewriteSystemVars = collectVars [] (vars |> List.head)
        VRewriteSystem(rewriteSystemVars, rules)
    | _ -> raise (NotImplementedException(sprintf "Cannot evaluate %A" aExpr))

type SourceContext = { source: string }

let resolveIncludePath (sourceContext: SourceContext) (path: string) =
    if Path.IsPathRooted(path) then
        path
    else
        Path.Combine(Path.GetDirectoryName(sourceContext.source), path)

let rec interpret context env statement =
    match statement with
    | SNamesDeclaration declarations ->
        //printfn "Declarations: %A" declarations
        let mutable names = env.names

        for (decl, arity) in declarations do
            match arity with
            | None -> names <- names.Add(decl, VEmpty)
            | Some arity -> names <- names.Add(decl, VArray(List.init arity (fun (_) -> VEmpty)))

        { env with names = names }
    | SAssignment(ident, index, expr) ->
        match env.names |> Map.tryFind ident with
        | Some value ->
            //printfn "Assign: %s = %A" ident expr
            match index with
            | Some index -> raise (NotImplementedException("not implemented"))
            | None ->
                let evaluatedValue = evaluateExpression env expr

                { env with
                    names = env.names |> Map.add ident evaluatedValue }
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
        match marks with
        | MarkDescription marks ->
            for mark in marks do
                match mark with
                | BinaryMark(name, arity, priority, symbol) ->
                    if arity <> 2u then
                        printfn "Invalid arity for binary mark %s" name
                    else
                        printfn "Add binary mark code = %s with priority %d and symbol %s" name priority symbol
                        setBinaryMark name (int priority) symbol
                | UnaryMark(name, arity, priority) ->
                    printfn "UnaryMark: %A. This looks like function with priority %d" mark priority
                    setUnaryMark name (int priority) name
                | _ -> printfn "Mark: %A" mark

        env
    | SEmpty -> env
    | SInclude path ->
        printfn "Include file from: %A" path
        let fileName = resolveIncludePath context path
        let programCode = File.ReadAllText(fileName)
        interpretProgram { context with source = fileName } programCode
        printfn "Done processing include file from: %A" path
        env

and interpretProgram context str =
    let mutable initialPosition = 0
    let mutable haveToExit = false
    let str = str + ";"

    //while (not haveToExit) do
    //match runParserOnSubstring statement () context.source str initialPosition (str.Length - initialPosition) with
    let statementInterpreter statement =
        interpret context globalEnv statement |> ignore

    match run (program statementInterpreter) str with
    //match run (program (fun x -> ())) str with
    | Success(result, _, position) when position.Index = str.Length -> haveToExit <- true
    | Success(result, _, position) when position.Index <> str.Length ->
        //printfn "Initial: %d current %d - %A" initialPosition position.Index result
        if (int position.Index <= 0) then
            // if result <> SEmpty then
            //     printfn "Don't parse whole file: %A" result

            haveToExit <- true

        initialPosition <- initialPosition + int position.Index

        printfn
            ">>> Partial Success: %A, position: %A, index = %d, initialPosition = %d"
            result
            position
            position.Index
            initialPosition
    //globalEnv <- interpret context globalEnv result
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
//
// interpretProgram "test" "NAME task; task:=1;"
// interpretProgram "test" ""
[<EntryPoint>]
let main (args) =
    match args with
    | [| fileName |] ->
        let programCode = File.ReadAllText(fileName)
        interpretProgram { source = fileName } programCode
    | [||] ->
        let programCode = Console.In.ReadToEnd()
        interpretProgram { source = "<stdin>" } programCode
    | _ -> printfn "Invalid arguments. aps [input-file]"

    0
