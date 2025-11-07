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
    | UnaryMark of string * uint32 * uint32

type MarkDescription =
    | MarkDescription of MarkDescriptionElement list

type Statement =
    | SNamesDeclaration of (string * int option) list
    | SAtomDeclaration of string list
    | SMarkDescription of MarkDescription
    | SExpression of AlgebraicExpression
    | SAssignment of string * (int option) * AlgebraicExpression
    | SEmpty

let private str s = pstring s

let private isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_' || c = '`' || c = '~'
let private isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '`' || c = '~'
let private aplanIdentifierOptions =
    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        normalization = System.Text.NormalizationForm.FormKC,
        normalizeBeforeValidation = true,
        allowAllNonAsciiCharsInPreCheck = true)

let private aplanIdentifier : Parser<string, unit> = identifier aplanIdentifierOptions

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
        //(List.contains x ['+';'-';'*';'/';'%';'|';',';';';':';'=';'>';'<';'&';'$';'^'])
        )
let private isAplanSignAtom =
    (fun x ->
        isAplanSign x
        && x <> ','
        && x <> ';'
        //(List.contains x ['+';'-';'*';'/';'%';'|';',';';';':';'=';'>';'<';'&';'$';'^'])
        )
let private signChar: Parser<char, unit> =
    satisfy isAplanSign
let private infixNotationInternal popen pclose = manyCharsBetween popen pclose signChar
let private infixNotation =
    choice [
        pstring "mod"
        pstring "else"
        manySatisfy isAplanSign
    ]
let private infixNotationAtom =
    manySatisfy isAplanSignAtom

let private quotedInfixNotation = pchar '"' >>. infixNotation .>> pchar '"'
let private multiLineComment = skipString "/*" |> anyStringBetween <| skipString "*/"
let private ws = spaces .>> many (multiLineComment .>> spaces)
let private ws1 = spaces1 .>> many (multiLineComment .>> spaces)
let int64Number = pint64 |>> AInt64
let floatNumber = pfloat |>> AFloat

// Names declaration
let private identifiersList =
    sepBy
        (aplanIdentifier .>>. opt (str "[" >>. pint32 .>> str "]"))
        (ws >>. pstring "," .>> ws)
let namesDeclaration =
    ((attempt (str "NAMES")) <|> str "NAME")
        >>. pstring " " >>. ws >>. identifiersList
        |>> SNamesDeclaration

// Atom declarations
let private signsList =
    sepBy
        (infixNotationAtom)
        (ws >>. pstring "," .>> ws)
let atomsDeclaration =
    ((attempt (str "ATOMS")) <|> str "ATOM")
        >>. pstring " " >>. ws >>. signsList
        |>> SAtomDeclaration

// Mark descriptions
let arity: Parser<MarkArity, unit> =
    ((puint32 |>> KnownArity)
    <|> (skipString "UNDEF" |>> (fun () -> UndefinedArity)))

let private genericMarkDescriptionElement =
    (aplanIdentifier .>> spaces .>>
        pstring "(" .>> spaces .>>. arity .>> spaces .>> pstring ")" |>> GenericMark)
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
    attempt genericMarkDescriptionElement <|> attempt binaryMarkDescriptionElement <|> unaryMarkDescriptionElement
let markDescriptionElementList = sepBy markDescriptionElement (ws >>. pstring "," .>> ws)
let markDescription =
    (pstring "MARKS" <|> pstring "MARK") >>. ws1 >>. markDescriptionElementList |>> MarkDescription

// Expressions
let algebraicExpression, algebraicExpressionRef = createParserForwardedToRef<AlgebraicExpression, unit>()
let private primaryExpression =
    attempt (int64Number .>> notFollowedBy (pchar '.'))
    <|> (floatNumber)
    <|> (quotedString |>> AString)
    <|> (aplanIdentifier |>> AAtom)
    <|> (str "VAL" .>> spaces >>. aplanIdentifier |>> AVal)
    <|> attempt ((str "(" >>. spaces >>. str ")") |>> (fun (_) -> AEmpty))
    <|> (str "(" >>. spaces >>. algebraicExpression .>> spaces .>> str ")")

let private algebraicExpressionList = sepBy algebraicExpression (ws >>. pstring "," .>> ws)

let private prefixExpression =
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

let private application =
    attempt (tuple2
        (prefixExpression .>> spaces)
        (algebraicExpression .>> spaces)
        |>> AApplicationExpression)
    <|> prefixExpression

let private infixExpression =
    tuple3
        (application .>> spaces)
        (infixNotation .>> spaces)
        (algebraicExpression .>> spaces)
        |>> AInfixExpression

let private infixOperator op priority    =
    InfixOperator(op, spaces,
        priority, Associativity.Right,
        fun left right -> AInfixExpression(left, op, right))

let private opp = new OperatorPrecedenceParser<_,_,_>()
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
            atomsDeclaration .>> spaces .>> str ";"
            tuple3
                (aplanIdentifier .>> ws)
                (opt (str "[" .>> ws >>. pint32 .>> ws .>> str "]"))
                (str ":=" >>. ws >>. algebraicExpression .>> spaces .>> str ";")
                |>> SAssignment
            algebraicExpression .>> spaces .>> str ";" |>> SExpression
            str ";" |>> fun (_) -> SEmpty
            //spaces |>> fun () -> SEmpty
        ])
    |>> Option.defaultValue SEmpty

let program = sepBy statement (ws >>. pstring ";" .>> ws)
