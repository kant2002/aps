module Parser

open FParsec

type AlgebraicExpression =
    | AInt64 of int64
    | AFloat of float
    | AString of string
    | AEmpty
    // Atoms are identifiers which occur in program and were not describedas names or marks or infix notations.
    // Would be introduced in the CST later on probably. By default we parse all identifiers as identifiers.
    | AAtom of string
    | AIdentifier of string
    | AVal of string
    | APrefixExpression of (string * (AlgebraicExpression list))
    | AInfixExpression of AlgebraicExpression * string * AlgebraicExpression
    | AApplicationExpression of AlgebraicExpression * AlgebraicExpression
    | AArrayIndexingExpression of string * AlgebraicExpression
    | ARewriteSystemExpression of AlgebraicExpression list * AlgebraicExpression
    | AProcExpression of AlgebraicExpression list * (AlgebraicExpression list option) * AlgebraicExpression list

type MarkArity =
    | KnownArity of uint32
    | UndefinedArity

type MarkDescriptionElement =
    | GenericMark of string * MarkArity
    | BinaryMark of string * uint32 * uint32 * string
    | UnaryMark of string * uint32 * uint32

type MarkDescription = MarkDescription of MarkDescriptionElement list

type Statement =
    | SNamesDeclaration of (string * int option) list
    | SAtomDeclaration of string list
    | SMarkDescription of MarkDescription
    | SExpression of AlgebraicExpression
    | SAssignment of string * (int option) * AlgebraicExpression
    | SEmpty
    | SInclude of string

let private str s = pstring s
let mutable debugMode = false
let mutable ident = 0

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        let identString = String.init ident (fun _ -> " ")

        if debugMode then
            printfn "%A: %s Entering %s(%d)" stream.Position identString label ident
            ident <- ident + 2

        try
            let reply = p stream

            if debugMode then
                ident <- ident - 2

                printfn
                    "%A: %s Leaving %s(%d) (%A) (%O)"
                    stream.Position
                    identString
                    label
                    ident
                    reply.Status
                    reply.Result

            reply
        with ex ->
            if debugMode then
                ident <- ident - 2
                printfn "%A: %s Leaving %s(%d) (Unexpected Error)" stream.Position identString label ident

            Reply(Error, unexpected ex.Message)

let private isAsciiIdStart =
    fun c -> isAsciiLetter c || c = '_' || c = '`' || c = '~'

let private isAsciiIdContinue =
    fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '`' || c = '~'

let private aplanIdentifierOptions =
    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        normalization = System.Text.NormalizationForm.FormKC,
        normalizeBeforeValidation = true,
        allowAllNonAsciiCharsInPreCheck = true
    )

let private aplanIdentifier: Parser<string, unit> =
    identifier aplanIdentifierOptions

let floatBetweenBrackets: Parser<float, unit> = str "[" >>. pfloat .>> str "]"
let private manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
let private anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
let private quotedString = skipChar '"' |> anyStringBetween <| skipChar '"'

let private isAplanSign =
    (fun x ->
        not (isAsciiLetter x)
        && not (isDigit x)
        && x <> ' '
        && x <> '"'
        && x <> ')'
        && x <> '(')

let private isAplanSignAtom =
    (fun x -> isAplanSign x && x <> ',' && x <> ';' && x <> ')' && x <> '(')

let private signChar: Parser<char, unit> = satisfy isAplanSign
let private infixNotationInternal popen pclose = manyCharsBetween popen pclose signChar

let private infixNotation =
    choice [ pstring "mod"; pstring "else"; manySatisfy isAplanSign ]

let private infixNotationAtom = manySatisfy isAplanSignAtom

let private quotedInfixNotation = pchar '"' >>. infixNotation .>> pchar '"'

let private multiLineComment =
    skipString "/*" |> anyStringBetween <| skipString "*/"

let private ws = spaces .>> many (multiLineComment .>> spaces)
let private ws1 = spaces1 .>> many (multiLineComment .>> spaces)
let int64Number = pint64 |>> AInt64
let floatNumber = pfloat |>> AFloat

let private anglePath = pchar '<' >>. manySatisfy (fun x -> x <> '>') .>> pchar '>'

// Names declaration
let private identifiersList =
    sepBy (aplanIdentifier .>>. opt (str "[" >>. pint32 .>> str "]")) (ws >>. pstring "," .>> ws)

let namesDeclaration =
    ((attempt (str "NAMES")) <|> str "NAME") >>. ws1 >>. identifiersList
    |>> SNamesDeclaration

// Atom declarations
let private signsList = sepBy (infixNotationAtom) (ws >>. pstring "," .>> ws)

let atomsDeclaration =
    ((attempt (str "ATOMS")) <|> str "ATOM") >>. pstring " " >>. ws >>. signsList
    |>> SAtomDeclaration

// Mark descriptions
let arity: Parser<MarkArity, unit> =
    ((puint32 |>> KnownArity) <|> (skipString "UNDEF" |>> (fun () -> UndefinedArity)))

let private genericMarkDescriptionElement =
    (aplanIdentifier .>> spaces .>> pstring "(" .>> spaces .>>. arity
     .>> spaces
     .>> pstring ")"
     |>> GenericMark)

let private binaryMarkDescriptionElement =
    tuple4
        (aplanIdentifier .>> spaces)
        (str "(" .>> ws >>. puint32 .>> ws)
        (str "," .>> ws >>. puint32 .>> ws)
        (str "," .>> ws >>. quotedInfixNotation .>> ws .>> str ")")
    |>> BinaryMark

let private unaryMarkDescriptionElement =
    tuple3
        (aplanIdentifier .>> spaces)
        (str "(" .>> ws >>. puint32 .>> ws)
        (str "," .>> ws >>. puint32 .>> ws .>> ws .>> str ")")
    |>> UnaryMark

let markDescriptionElement =
    attempt genericMarkDescriptionElement
    <|> attempt binaryMarkDescriptionElement
    <|> unaryMarkDescriptionElement

let markDescriptionElementList =
    sepBy markDescriptionElement (ws >>. pstring "," .>> ws)

let markDescription =
    (pstring "MARKS" <|> pstring "MARK") >>. ws1 >>. markDescriptionElementList
    |>> MarkDescription

// Expressions

let private infixOperator op priority =
    InfixOperator(op, spaces, priority, Associativity.Left, fun left right -> AInfixExpression(left, op, right))

let private prefixOperator op priority =
    PrefixOperator(op, spaces, priority, false, fun right -> APrefixExpression(op, [ right ]))

let createProgramParser() =
    let algebraicExpression, algebraicExpressionRef =
        createParserForwardedToRef<AlgebraicExpression, unit>()

    let algebraicList =
        //algebraicExpressionListSemicolon
        let rec preprocess xs =
            match xs with
            | [ x ] -> x
            | head :: tail -> AInfixExpression(head, ";", preprocess tail)
            | _ -> raise (invalidOp (sprintf "Invalid grammar for the application. Cannot have 0 arguments. %A" xs))

        sepBy1 algebraicExpression (ws >>. pstring ";" .>> ws) |>> preprocess

    let primaryExpression =
        attempt (int64Number .>> notFollowedBy (pchar '.'))
        <|> (floatNumber)
        <|> (quotedString |>> AString)
        <|> attempt (str "VAL" .>> spaces >>. aplanIdentifier |>> AVal)
        <|> (aplanIdentifier |>> AAtom <!> "atom primary expression")
        <|> (attempt ((str "(" >>. ws >>. str ")") |>> (fun (_) -> AEmpty))
             <!> "empty primary expression")
        <|> ((str "(" >>. ws >>. algebraicList .>> ws .>> str ")")
             <!> "nested primary expression")

    let algebraicExpressionList =
        sepBy algebraicExpression (ws >>. pstring "," .>> ws)

    let algebraicExpressionListSemicolon =
        sepBy algebraicExpression (ws >>. pstring ";" .>> ws)

    let rewriteExpression =
        tuple2
            (pstring "rs" .>> ws .>> str "(" .>> ws >>. algebraicExpressionList
             .>> ws
             .>> str ")"
             .>> ws
             <!> "rewrite paramters")
            algebraicExpression
        <!> "rewrite body"
        |>> ARewriteSystemExpression

    let getList (value: AlgebraicExpression) =
        let rec getListCore sep value = 
            match value with
            | AInfixExpression(left, newsep, right) when sep = newsep -> (getListCore sep left) @ (getListCore sep right)
            | AEmpty -> []
            | _ -> [value]

        match value with
            | AInfixExpression(left, ",", right) -> getListCore "," value
            | AInfixExpression(left, ";", right) -> getListCore ";" value
            | AEmpty -> []
            | _ -> raise (invalidOp (sprintf "Invalid grammar for the application. Cannot have 0 arguments. %A" value))
    let opp = new OperatorPrecedenceParser<AlgebraicExpression, unit, unit>()

    let setBinaryMark name priority symbol =
        //if symbol <> ";" || false then
            opp.RemoveInfixOperator(symbol) |> ignore
            opp.AddOperator(infixOperator symbol priority)

    let setUnaryMark name priority symbol =
        opp.RemovePrefixOperator(symbol) |> ignore
        opp.AddOperator(prefixOperator symbol priority)

    let setDefaultMarks() =
        // Predicates
        setBinaryMark "POW"  60 "^"
        setBinaryMark "MOD"  59 "mod"  
        setBinaryMark "Mult" 58 "*" 
        setBinaryMark "DIV"  57 "/"  
        setBinaryMark "MLT"  56 "$"  
        setBinaryMark "SUB"  55 "-"  
        setBinaryMark "ADD"  54 "+"  

        // Predicates
        setBinaryMark "LE"  40 "<="
        setBinaryMark "LT"  40 "<"  // LS in the APS 
        setBinaryMark "GE"  40 ">=" // ME in the APS
        setBinaryMark "GT"  40 ">"  // MR in the APS
        setBinaryMark "EQ"  11 "=="
        setBinaryMark "EQU" 11 "="

        // Logical connections
        setUnaryMark "~" 30 "~"
        setBinaryMark "AND" 29 "&"
        setBinaryMark "OR"  28 "|/"
        setBinaryMark "IFF" 27 "<=>"

        // Imperative APLAN
        setBinaryMark "SET" 20 "-->"
        setBinaryMark "ASS" 20 ":="
        setBinaryMark "ELSE" 19 "else"
        setBinaryMark "IF"  18 "->"

        // Separators
        setBinaryMark "COMMA" 7 ","
        //setBinaryMark "LL" 5 ";"

        // gen_obj addition for smoke testing, remove after implementation wouldbe more mature
        setBinaryMark "COLON" 8 ":"
        setBinaryMark "DOT"   62 "."

        setBinaryMark "itr"   60 "^*"
        setBinaryMark "caten" 50 "!"
        setBinaryMark "parc"  28 "||"
        setBinaryMark "cpar"  28 "|^"
        setBinaryMark "comb"  59 "><"
        setBinaryMark "ccomb" 59 ">^"



    let mutable semicolonsRequest = 0
    let injectSemicolon state = 
        semicolonsRequest <- semicolonsRequest + 1
        if semicolonsRequest = 1 then
            setBinaryMark "LL" 5 ";"
        state

    let suppressSemicolon state = 
        semicolonsRequest <- semicolonsRequest - 1
        if semicolonsRequest = 0 then
            opp.RemoveInfixOperator(";") |> ignore
        state

    let algebraicExpressionX = 
        choice [
            str "(" >>. ws |>> injectSemicolon >>. algebraicExpression .>> ws |>> suppressSemicolon .>> str ")" <!> "nested expression with ; enabled"
            algebraicExpression <!> "simple expression"
        ]

    let procExpression =
        tuple3
            (pstring "proc" .>> ws .>> str "(" .>> ws >>. algebraicExpressionList
             .>> ws
             .>> str ")"
             .>> ws
             <!> "proc parameters")
            (opt (
                str "loc(" .>> ws >>. algebraicExpressionList .>> ws .>> str ")" .>> ws
                <!> "proc local parameters"
            ))
            (algebraicExpressionX
             <!> "proc local statements")
        <!> "proc body"
        |>> fun (parameters, localParameters, statements) -> AProcExpression(parameters, localParameters, [statements])

    let prefixExpression =
        // attempt rewriteExpression
        // <|>
        //attempt (
        //    aplanIdentifier .>> ws .>> str "(" .>> ws .>>. (algebraicExpressionList)
        //    .>> ws
        //    .>> str ")"
        //    |>> APrefixExpression
        //    <!> "prefix () expression"
        //)
        // <|>
        attempt (
            aplanIdentifier .>> ws .>> str "[" .>> ws .>>. algebraicExpression
            .>> ws
            .>> str "]"
            |>> AArrayIndexingExpression
            <!> "prefix [] expression"
        )
        <|> (primaryExpression <!> "prefix trivial")

    let application =
        attempt (
            tuple2 (prefixExpression .>> ws <!> "application base") (algebraicExpression <!> "application argument")
            |>> AApplicationExpression
        )
        <|> (prefixExpression <!> "application trivial")
        // let rec preprocess xs =
        //     match xs with
        //     | [ x ] -> x
        //     | head :: tail -> AApplicationExpression( head, preprocess tail )
        //     | _ -> raise (invalidOp (sprintf "Invalid grammar for the application. Cannot have 0 arguments. %A" xs))
        // sepBy1 prefixExpression ws |>> preprocess

    let infixExpression =
        tuple3
            (application .>> ws <!> "infix application")
            (infixNotation .>> ws)
            (algebraicExpression <!> "infix application right")
        |>> AInfixExpression
    opp.TermParser <- ws >>. application .>> ws <!> "infix term"

    //algebraicExpressionRef := choice [ attempt infixExpression; application ]
    //algebraicExpressionRef := choice [ opp.ExpressionParser ]
    algebraicExpressionRef
    := choice [ rewriteExpression; procExpression; opp.ExpressionParser ]

    let assignmentStatement =
        tuple3
            (aplanIdentifier .>> ws <!> "assignment identifier")
            (opt (str "[" .>> ws >>. pint32 .>> ws .>> str "]") .>> ws <!> "assignment index")
            (str ":=" >>. ws >>. (algebraicExpressionX) .>> ws .>> str ";"
             <!> "assignment expression")
        |>> SAssignment

    let statement =
        ws
        >>. choice
                [
                    markDescription .>> spaces .>> str ";" |>> SMarkDescription
                    namesDeclaration .>> ws .>> str ";"
                    atomsDeclaration .>> spaces .>> str ";"
                    str "INCLUDE" >>. ws >>. anglePath |>> SInclude
                    assignmentStatement
                    str ";" |>> fun (_) -> SEmpty
                    ws1 |>> fun (_) -> SEmpty
                ]
    {| statement = statement; setBinaryMark = setBinaryMark; setUnaryMark = setUnaryMark; setDefaultMarks = setDefaultMarks; algebraicExpression = algebraicExpression |}
// (fun stream ->
//     let reply1 = statement stream
//     if reply1.Status = Ok then
//         interpret reply1.Result
//         let p2 = reply1.Result
//         let stateTag = stream.StateTag
//         let mutable reply2 = (preturn p2) stream
//         if stateTag = stream.StateTag then
//             reply2.Error <- mergeErrors reply1.Error reply2.Error
//         reply2
//     else
//         Reply(reply1.Status, reply1.Error))
