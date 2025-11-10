module Tests

open System
open System.Diagnostics
open Xunit
open Parser
open FParsec

let runParser p str =
    match run p str with
    | Success(result, _, position) when position.Index = str.Length -> result
    | Success(result, _, position) when position.Index <> str.Length ->
        Assert.Fail(sprintf "Partial Success: %A, unconsumed input: %s" result (str.Substring(int32 position.Index)))
        raise (new UnreachableException())
    | Failure(errorMsg, _, _) ->
        Assert.Fail(errorMsg)
        raise (new UnreachableException())
    | _ -> raise (new UnreachableException())

[<Fact>]
let ``Trivial expressions`` () =
    Assert.Equal(AInt64 1L, runParser int64Number "1")
    Assert.Equal(AFloat 1.25, runParser floatNumber "1.25")
    Assert.Equal(AAtom "x", runParser algebraicExpression "x")

[<Fact>]
let ``Names declaration`` () =
    Assert.Equal(SNamesDeclaration [ ("S", None) ], runParser namesDeclaration "NAMES S")
    Assert.Equal(SNamesDeclaration [ ("S", None) ], runParser namesDeclaration "NAME S")
    Assert.Equal(SNamesDeclaration [ ("arr", Some 4) ], runParser namesDeclaration "NAME arr[4]")
    Assert.Equal(SNamesDeclaration [ ("S1", None); ("S2", None) ], runParser namesDeclaration "NAME S1,S2")
    Assert.Equal(SNamesDeclaration [ ("S1", None); ("S2", None) ], runParser namesDeclaration "NAME S1, S2")
// Assert.Equal(
//     [SNamesDeclaration [("S1", None);("S2", None)]],
//     runParser (program ignore) "  /**/\n\n\n\n NAME S1, S2;")

[<Fact>]
let ``Mark description`` () =
    Assert.Equal(
        MarkDescription
            [ GenericMark("X", KnownArity 3u)
              GenericMark("X", UndefinedArity)
              BinaryMark("comma", 2u, 7u, ",")
              UnaryMark("~", 1u, 20u) ],
        runParser markDescription "MARK X(3), X(UNDEF), comma( 2,  7, \",\"), ~(1, 20)"
    )

    Assert.Equal(
        MarkDescription
            [ GenericMark("X", KnownArity 3u)
              GenericMark("X", UndefinedArity)
              BinaryMark("comma", 2u, 7u, ",") ],
        runParser markDescription "MARK X(3), /*some comment*/ X(UNDEF), comma( 2,  7, \",\")"
    )

    Assert.Equal(
        MarkDescription
            [ GenericMark("X", KnownArity 3u)
              GenericMark("X", UndefinedArity)
              BinaryMark("comma", 2u, 7u, ",") ],
        runParser markDescription "MARK /*some comment*/ X(3), X(UNDEF), comma( 2,  7, \",\")"
    )

    Assert.Equal(
        MarkDescription
            [ GenericMark("X", KnownArity 3u)
              GenericMark("X", UndefinedArity)
              BinaryMark("comma", 2u, 7u, ",") ],
        runParser markDescription "MARK\n/*some comment*/ X(3), X(UNDEF), comma( 2,  7, \",\")"
    )

    Assert.Equal(
        MarkDescription
            [ GenericMark("X", KnownArity 3u)
              GenericMark("X", UndefinedArity)
              BinaryMark("comma", 2u, 7u, ",") ],
        runParser markDescription "MARK\n/*some comment*/ /*othercomment*/ X(3), X(UNDEF), comma( 2,  7, \",\")"
    )

    setBinaryMark "COMMA" 7 ","
    setBinaryMark "SEM" 5 ";"
    setBinaryMark "EQU" 11 "="

[<Fact>]
let ``Primitive expression`` () =
    Assert.Equal(AAtom "x", runParser algebraicExpression "x")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "2")
    Assert.Equal(AFloat 2.3, runParser algebraicExpression "2.3")
    Assert.Equal(AEmpty, runParser algebraicExpression "()")
    Assert.Equal(AString "some string", runParser algebraicExpression "\"some string\"")
    Assert.Equal(AVal "z", runParser algebraicExpression "VAL z")
    Assert.Equal(AAtom "x", runParser algebraicExpression "(x)")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "(2)")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "((2))")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "( 2 )")

[<Fact>]
let ``Prefix expression`` () =
    Assert.Equal(APrefixExpression("F", [ AInt64 2L ]), runParser algebraicExpression "F(2)")
    Assert.Equal(APrefixExpression("F", [ AInt64 2L ]), runParser algebraicExpression "F( 2)")
    Assert.Equal(APrefixExpression("F", [ AInt64 2L ]), runParser algebraicExpression "F( 2 )")
    Assert.Equal(AArrayIndexingExpression("a", AInt64 5L), runParser algebraicExpression "a[5]")

[<Fact>]
let ``Infix expression`` () =
    setBinaryMark "ADD" 54 "+"
    setBinaryMark "MUL" 58 "*"

    Assert.Equal(AInfixExpression(AAtom "x", "+", AAtom "y"), runParser algebraicExpression "x + y")
    Assert.Equal(AInfixExpression(AAtom "x", "+", AAtom "y"), runParser algebraicExpression "x +  y")
    Assert.Equal(AInfixExpression(AAtom "x", "+", AAtom "y"), runParser algebraicExpression "(x + y)")

    Assert.Equal(
        AInfixExpression(AInfixExpression(AAtom "x", "+", AAtom "y"), "*", AAtom "z"),
        runParser algebraicExpression "(x + y) * z"
    )

    Assert.Equal(
        AInfixExpression(AInfixExpression(AInfixExpression(AAtom "x", "+", AAtom "y"), "*", AAtom "z"), "+", AAtom "w"),
        runParser algebraicExpression "(x + y) * z + w"
    )

[<Fact>]
let ``Asignment`` () =
    Assert.Equal(SAssignment("b", None, AInt64 11L), runParser statement "b := 11;")
    Assert.Equal(SAssignment("a", Some 5, AInt64 10L), runParser statement "a[5] := 10;")

[<Fact>]
let ``Application expression`` () =
    Assert.Equal(
        AApplicationExpression(APrefixExpression("test", []), APrefixExpression("loc1", [ AAtom "Term" ])),
        runParser algebraicExpression "test()loc1(Term)"
    )

    Assert.Equal(
        AApplicationExpression(
            APrefixExpression("test", []),
            AApplicationExpression(APrefixExpression("loc1", [ AAtom "Term" ]), AEmpty)
        ),
        runParser algebraicExpression "test()loc1(Term)()"
    )

    Assert.Equal(
        AApplicationExpression(
            APrefixExpression("test", []),
            AApplicationExpression(APrefixExpression("loc1", [ AAtom "Term" ]), AAtom "x")
        ),
        runParser algebraicExpression "test()loc1(Term)x"
    )

    Assert.Equal(
        AApplicationExpression(APrefixExpression("test", []), AAtom "x"),
        runParser algebraicExpression "test()x"
    )

    Assert.Equal(AApplicationExpression(AAtom "a", AAtom "x"), runParser algebraicExpression "a x")


[<Fact>]
let ``Rewrite system`` () =
    setBinaryMark "MR" 58 ">" // More (strange names in hindsight)
    setBinaryMark "SEM" 5 ";"

    Assert.Equal(ARewriteSystemExpression([], AAtom "x"), runParser algebraicExpression "rs()x")
    Assert.Equal(SAssignment("R", None, ARewriteSystemExpression([], AAtom "x")), runParser statement "R:=rs()x;")
    Assert.Equal(SAssignment("R", None, ARewriteSystemExpression([], AAtom "x")), runParser statement "R:=rs()\nx;")
    Assert.Equal(SAssignment("R", None, ARewriteSystemExpression([], AAtom "x")), runParser statement "R:=rs()(\nx);")

    Assert.Equal(
        SAssignment("R", None, ARewriteSystemExpression([], AInfixExpression(AAtom "x", ">", AAtom "x"))),
        runParser statement "R:=rs()((x>x));"
    )

    setBinaryMark "COMMA" 7 ","
    setBinaryMark "EQU" 11 "="

[<Fact>]
let ``Atom description`` () =
    Assert.Equal(SAtomDeclaration [ "?"; "@"; "#" ], runParser statement "ATOMS ?,  @, /* {,  }, */ #;")

[<Fact>]
let ``Include statements`` () =
    Assert.Equal(SInclude "test.ap", runParser statement "INCLUDE <test.ap>")

[<Fact>]
let ``Proc definition statements`` () =
    setBinaryMark "SET" 20 "-->"
    setBinaryMark "COMMA" 7 ","
    setBinaryMark "SEM" 5

    Assert.Equal(
        SAssignment(
            "p",
            None,
            AProcExpression(
                [ AAtom "x" ],
                Some [ AAtom "y" ],
                [ AInfixExpression(
                      AInfixExpression(
                          AInfixExpression(
                              AInfixExpression(AAtom "x", "-->", APrefixExpression("copy", [ AAtom "x" ])),
                              ",",
                              APrefixExpression("ntb", [ AInfixExpression(AAtom "x", ",", AAtom "R") ])
                          ),
                          ",",
                          APrefixExpression(
                              "can_ord",
                              [ AInfixExpression(AInfixExpression(AAtom "x", ",", AAtom "R1"), ",", AAtom "Q1") ]
                          )
                      ),
                      ",",
                      APrefixExpression("return", [ AAtom "x" ])
                  ) ]
            )
        ),
        runParser
            statement
            """p:=proc(x)loc(y)(
      x-->copy(x),
      ntb(x,R),
      can_ord(x,R1,Q1),
      return(x)
    );"""
    )

    Assert.Equal(
        SAssignment(
            "p",
            None,
            AProcExpression(
                [ AAtom "x" ],
                None,
                [ AInfixExpression(
                      AInfixExpression(
                          AInfixExpression(
                              AInfixExpression(AAtom "x", "-->", APrefixExpression("copy", [ AAtom "x" ])),
                              ",",
                              APrefixExpression("ntb", [ AInfixExpression(AAtom "x", ",", AAtom "R") ])
                          ),
                          ",",
                          APrefixExpression(
                              "can_ord",
                              [ AInfixExpression(AInfixExpression(AAtom "x", ",", AAtom "R1"), ",", AAtom "Q1") ]
                          )
                      ),
                      ",",
                      APrefixExpression("return", [ AAtom "x" ])
                  ) ]
            )
        ),
        runParser
            statement
            """p:=proc(x)(
      x-->copy(x),
      ntb(x,R),
      can_ord(x,R1,Q1),
      return(x)
    );"""
    )

    Assert.Equal(
        SAssignment(
            "p",
            None,
            AProcExpression(
                [ AAtom "x" ],
                None,
                [ AInfixExpression(AAtom "x", "-->", APrefixExpression("copy", [ AAtom "x" ]))
                  APrefixExpression("ntb", [ AInfixExpression(AAtom "x", ",", AAtom "R") ])
                  APrefixExpression(
                      "can_ord",
                      [ AInfixExpression(AInfixExpression(AAtom "x", ",", AAtom "R1"), ",", AAtom "Q1") ]
                  )
                  APrefixExpression("return", [ AAtom "x" ]) ]
            )
        ),
        runParser
            statement
            """p:=proc(x)(
      x-->copy(x);
      ntb(x,R);
      can_ord(x,R1,Q1);
      return(x)
    );"""
    )
