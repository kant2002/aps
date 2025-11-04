// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open FParsec

type AlgebraicExpression =
    | ANumber of float

type MarkArity =
    | KnownArity of uint32
    | UndefinedArity

type MarkDescriptionElement =
    | GenericMark of string * MarkArity
    | BinaryMark of string * uint32 * uint32 * string

type MarkDescription =
    | MarkDescription of MarkDescriptionElement list

type Statement =
    | SNamesDeclaration of string list

let str s = pstring s
let ws = spaces

let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
let aplanIdentifierOptions =
    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        normalization = System.Text.NormalizationForm.FormKC,
        normalizeBeforeValidation = true,
        allowAllNonAsciiCharsInPreCheck = true)

let aplanIdentifier : Parser<string, unit> = identifier aplanIdentifierOptions

let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
let quotedString = skipChar '"' |> anyStringBetween <| skipChar '"'
let number = pfloat |>> ANumber

let identifiersList = sepBy aplanIdentifier (spaces >>. pstring "," .>> spaces)
let namesDeclaration = str "NAMES" >>. pstring " " >>. ws >>. identifiersList |>> SNamesDeclaration
let arity: Parser<MarkArity, unit> =
    ((puint32 |>> KnownArity)
    <|> (skipString "UNDEF" |>> (fun () -> UndefinedArity)))

// Mark descriptions
let genericMarkDescriptionElement =
    (aplanIdentifier .>> spaces .>>
        pstring "(" .>> spaces .>>. arity .>> spaces .>> pstring ")" |>> GenericMark)
let binaryMarkDescriptionElement =
    tuple4
        (aplanIdentifier .>> spaces)
        (pstring "(" .>> spaces >>. puint32 .>> spaces)
        (str "," .>> spaces >>. puint32 .>> spaces)
        (str "," .>> spaces >>. quotedString .>> spaces .>> pstring ")")
        |>> BinaryMark
let markDescriptionElement =
    attempt genericMarkDescriptionElement <|> binaryMarkDescriptionElement
let markDescriptionElementList = sepBy markDescriptionElement (spaces >>. pstring "," .>> spaces)
let markDescription =
    pstring "MARK" >>. spaces >>. markDescriptionElementList |>> MarkDescription

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test number "1.25"
test floatBetweenBrackets "[1.25]"
test namesDeclaration "NAMES S"
test namesDeclaration "NAMES S1,S2"
test namesDeclaration "NAMES S1, S2"
test arity "4"
test arity "UNDEF"
test markDescriptionElement "X(3)"
test markDescriptionElement "X(UNDEF)"
test markDescriptionElement "comma( 2,  7, \",\")"
test markDescription "MARK X(3), X(UNDEF), comma( 2,  7, \",\")"
