// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open FParsec

type AlgebraicExpression =
    | AInt64 of int64
    | AFloat of float
    | AString of string
    | AEmpty
    //| AAtom of string ;;Atoms are identifiers which occur in program and were not describedas names or marks or infix notations.
    // Would be introduced in the CST later on probably. By default we parse all identifiers as identifiers.s
    | AIdentifier of string
    | AVal of string
    | APrefixExpression of (string * (AlgebraicExpression list))
    | AInfixExpression of AlgebraicExpression * string * AlgebraicExpression
    | AApplicationExpression of AlgebraicExpression * AlgebraicExpression
    | AArrayIndexingExpression of string * AlgebraicExpression

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

let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_' || c = '`'
let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '`'
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
let isAplanSign =
    (fun x ->
        (List.contains x ['+';'-';'*';'/';'%';'|';',';';';':';'=';'>';'<';'&';'$';'^'])
        || (isAsciiLetter x))
let signChar: Parser<char, unit> =
    satisfy isAplanSign
let infixNotation popen pclose = manyCharsBetween popen pclose signChar
let quotedInfixNotation = skipChar '"' |> infixNotation <| skipChar '"'
let multiLineComment = skipString "/*" |> anyStringBetween <| skipString "*/"
let ws = spaces .>> opt (multiLineComment .>> spaces)
let int64Number = pint64 |>> AInt64
let floatNumber = pfloat |>> AFloat

let identifiersList = sepBy aplanIdentifier (ws >>. pstring "," .>> ws)
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
        (str "(" .>> ws >>. puint32 .>> ws)
        (str "," .>> ws >>. puint32 .>> ws)
        (str "," .>> ws >>. quotedInfixNotation .>> ws .>> str ")")
        |>> BinaryMark
let markDescriptionElement =
    attempt genericMarkDescriptionElement <|> binaryMarkDescriptionElement
let markDescriptionElementList = sepBy markDescriptionElement (ws >>. pstring "," .>> ws)
let markDescription =
    pstring "MARK" >>. spaces >>. markDescriptionElementList |>> MarkDescription

// Expressions
let algebraicExpression, algebraicExpressionRef = createParserForwardedToRef<AlgebraicExpression, unit>()
let primaryExpression =
    (int64Number)
    <|> (floatNumber)
    <|> (quotedString |>> AString)
    <|> (aplanIdentifier |>> AIdentifier)
    <|> (str "VAL" .>> spaces >>. aplanIdentifier |>> AVal)
    <|> attempt ((str "(" >>. spaces >>. str ")") |>> (fun (_) -> AEmpty))
    <|> (str "(" >>. algebraicExpression .>> str ")")

let algebraicExpressionList = sepBy algebraicExpression (ws >>. pstring "," .>> ws)

let prefixExpression =
    attempt (aplanIdentifier .>> spaces .>>
        str "(" .>> spaces .>>. algebraicExpressionList .>> spaces .>> str ")"
        |>> APrefixExpression)
    <|> attempt (aplanIdentifier .>> spaces .>>
        str "[" .>> spaces .>>. algebraicExpression .>> spaces .>> str "]"
        |>> AArrayIndexingExpression)
    <|> primaryExpression

let application =
    attempt (tuple2
        (prefixExpression .>> spaces)
        (algebraicExpression .>> spaces)
        |>> AApplicationExpression)
    <|> prefixExpression

let infixExpression =
    tuple3
        (application .>> spaces)
        (manySatisfy isAplanSign .>> spaces)
        (algebraicExpression .>> spaces)
        |>> AInfixExpression

algebraicExpressionRef := choice [ attempt infixExpression; application ]

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test int64Number "1"
test floatNumber "1.25"
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
test markDescription "MARK X(3), /*some comment*/ X(UNDEF), comma( 2,  7, \",\")"
test algebraicExpression "x"
test algebraicExpression "2"
test algebraicExpression "2.3"
test algebraicExpression "()"
test algebraicExpression "\"some string\""
test algebraicExpression "VAL z"
test algebraicExpression "(x)"
test algebraicExpression "(2)"
test algebraicExpression "F(2)"
test algebraicExpression "F ( 2)"
//test algebraicExpression "F ( 2 )"
test infixExpression "x + y"
test algebraicExpression "(x + y)"
test algebraicExpression "(x + y) * z"
test algebraicExpression "(x + y) * z + w"
test algebraicExpression "proc()loc(Term)"
test algebraicExpression "a[5]"
