// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open FParsec

type AlgebraicExpression =
    | ANumber of float


let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
let number = pfloat |>> ANumber

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test number "1.25"
test floatBetweenBrackets "[1.25]"
