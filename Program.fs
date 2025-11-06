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
    | ARewriteSystemExpression of (AlgebraicExpression list * AlgebraicExpression list)
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
    | SExpression of AlgebraicExpression
    | SMarkDescription of MarkDescription
    | SEmpty

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
        (List.contains x ['+';'-';'*';'/';'%';'|';',';';';':';'=';'>';'<';'&';'$';'^']))
let signChar: Parser<char, unit> =
    satisfy isAplanSign
let infixNotationInternal popen pclose = manyCharsBetween popen pclose signChar
let infixNotation =
    choice [
        pstring "mod"
        pstring "else"
        manySatisfy isAplanSign
    ]
let quotedInfixNotation = pchar '"' >>. infixNotation .>> pchar '"'
let multiLineComment = skipString "/*" |> anyStringBetween <| skipString "*/"
let ws = spaces .>> opt (multiLineComment .>> spaces)
let int64Number = pint64 |>> AInt64
let floatNumber = pfloat |>> AFloat

let identifiersList = sepBy aplanIdentifier (ws >>. pstring "," .>> ws)
let namesDeclaration =
    ((attempt (str "NAMES")) <|> str "NAME")
        >>. pstring " " >>. ws >>. identifiersList |>> SNamesDeclaration
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
    (pstring "MARKS" <|> pstring "MARK") >>. spaces >>. markDescriptionElementList |>> MarkDescription

// Expressions
let algebraicExpression, algebraicExpressionRef = createParserForwardedToRef<AlgebraicExpression, unit>()
let primaryExpression =
    attempt (int64Number .>> notFollowedBy (pchar '.'))
    <|> (floatNumber)
    <|> (quotedString |>> AString)
    <|> (aplanIdentifier |>> AIdentifier)
    <|> (str "VAL" .>> spaces >>. aplanIdentifier |>> AVal)
    <|> attempt ((str "(" >>. spaces >>. str ")") |>> (fun (_) -> AEmpty))
    <|> (str "(" >>. algebraicExpression .>> str ")")

let algebraicExpressionList = sepBy algebraicExpression (ws >>. pstring "," .>> ws)

let prefixExpression =
    attempt (pstring "rs" .>> spaces .>>
        str "(" .>> spaces >>. algebraicExpressionList .>> spaces .>> str ")" .>> spaces .>>
        str "(" .>> spaces .>>. algebraicExpressionList .>> spaces .>> str ")"
        |>> ARewriteSystemExpression)
    <|> attempt (aplanIdentifier .>> spaces .>>
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
        (infixNotation .>> spaces)
        (algebraicExpression .>> spaces)
        |>> AInfixExpression

let infixOperator op priority    =
    InfixOperator(op, spaces,
        priority, Associativity.Right,
        fun left right -> AInfixExpression(left, op, right))

let opp = new OperatorPrecedenceParser<_,_,_>()
opp.TermParser <- application .>> spaces
opp.AddOperator(infixOperator "+" 54)
opp.AddOperator(infixOperator "-" 55)
opp.AddOperator(infixOperator "*" 58)
//opp.AddOperator(infixOperator ";" 5)
opp.AddOperator(infixOperator "," 7)
opp.AddOperator(infixOperator "=" 11)
opp.AddOperator(infixOperator ":=" 20)

//algebraicExpressionRef := choice [ attempt infixExpression; application ]
algebraicExpressionRef := choice [ opp.ExpressionParser ]

let statement =
    spaces >>.
        opt (choice [
            markDescription .>> spaces .>> str ";" |>> SMarkDescription
            namesDeclaration .>> spaces .>> str ";"
            algebraicExpression .>> spaces .>> str ";" |>> SExpression
            str ";" |>> fun (_) -> SEmpty
            //spaces |>> fun () -> SEmpty
        ])
    |>> fun (optStatement) ->
        match optStatement with
        | Some statement -> statement
        | None -> SEmpty

let program = sepBy statement (ws >>. pstring ";" .>> ws)


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
test infixExpression "x + y"
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
