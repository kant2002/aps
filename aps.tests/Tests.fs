module Tests

open System
open System.Diagnostics
open Xunit
open Parser
open FParsec

let runParser p str =
    match run p str with
    | Success(result, _, position) when position.Index = str.Length ->
        result
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

[<Fact>]
let ``Names declaration`` () =
    Assert.Equal(
        SNamesDeclaration [("S", None)],
        runParser namesDeclaration "NAMES S")
    Assert.Equal(
        SNamesDeclaration [("S", None)],
        runParser namesDeclaration "NAME S")
    Assert.Equal(
        SNamesDeclaration [("arr", Some 4)],
        runParser namesDeclaration "NAME arr[4]")
    Assert.Equal(
        SNamesDeclaration [("S1", None);("S2", None)],
        runParser namesDeclaration "NAME S1,S2")
    Assert.Equal(
        SNamesDeclaration [("S1", None);("S2", None)],
        runParser namesDeclaration "NAME S1, S2")

[<Fact>]
let ``Mark description`` () =
    Assert.Equal(
        MarkDescription [
            GenericMark ("X", KnownArity 3u)
            GenericMark ("X", UndefinedArity)
            BinaryMark ("comma", 2u, 7u, ",")],
        runParser markDescription "MARK X(3), X(UNDEF), comma( 2,  7, \",\")")
    Assert.Equal(
        MarkDescription [
            GenericMark ("X", KnownArity 3u)
            GenericMark ("X", UndefinedArity)
            BinaryMark ("comma", 2u, 7u, ",")],
        runParser markDescription "MARK X(3), /*some comment*/ X(UNDEF), comma( 2,  7, \",\")")

[<Fact>]
let ``Primitive expression`` () =
    Assert.Equal(AIdentifier "x", runParser algebraicExpression "x")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "2")
    Assert.Equal(AFloat 2.3, runParser algebraicExpression "2.3")
    Assert.Equal(AEmpty, runParser algebraicExpression "()")
    Assert.Equal(AString "some string", runParser algebraicExpression "\"some string\"")
    //Assert.Equal(AVal "z", runParser algebraicExpression "VAL z")
    Assert.Equal(AIdentifier "x", runParser algebraicExpression "(x)")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "(2)")
    Assert.Equal(AInt64 2L, runParser algebraicExpression "( 2 )")

[<Fact>]
let ``Prefix expression`` () =
    Assert.Equal(
        APrefixExpression ("F", [AInt64 2L]),
        runParser algebraicExpression "F(2)")
    Assert.Equal(
        APrefixExpression ("F", [AInt64 2L]),
        runParser algebraicExpression "F( 2)")
    Assert.Equal(
        APrefixExpression ("F", [AInt64 2L]),
        runParser algebraicExpression "F( 2 )")
    Assert.Equal(
        AArrayIndexingExpression ("a", AInt64 5L),
        runParser algebraicExpression "a[5]")

[<Fact>]
let ``Infix expression`` () =
    Assert.Equal(
        AInfixExpression (AIdentifier "x", "+", AIdentifier "y"),
        runParser algebraicExpression "x + y")
    Assert.Equal(
        AInfixExpression (AIdentifier "x", "+", AIdentifier "y"),
        runParser algebraicExpression "x +  y")
    Assert.Equal(
        AInfixExpression (AIdentifier "x", "+", AIdentifier "y"),
        runParser algebraicExpression "(x + y)")
    Assert.Equal(
        AInfixExpression
            (AInfixExpression (AIdentifier "x", "+", AIdentifier "y"), "*",
            AIdentifier "z"),
        runParser algebraicExpression "(x + y) * z")
    Assert.Equal(
        AInfixExpression
            (AInfixExpression
                (AInfixExpression (AIdentifier "x", "+", AIdentifier "y"),
                "*",
                AIdentifier "z"),
            "+",
            AIdentifier "w"),
        runParser algebraicExpression "(x + y) * z + w")
    Assert.Equal(
        AInfixExpression
            (AArrayIndexingExpression ("a", AInt64 5L), ":=", AInt64 10L),
        runParser algebraicExpression "a[5] := 10")

[<Fact>]
let ``Application expression`` () =
    Assert.Equal(
        AApplicationExpression
            (APrefixExpression ("proc", []),
            APrefixExpression ("loc", [AIdentifier "Term"])),
        runParser algebraicExpression "proc()loc(Term)")


[<Fact>]
let ``Rewrite system`` () =
    Assert.Equal(
        AInfixExpression
            (AIdentifier "R", ":=",
            AApplicationExpression (APrefixExpression ("rs", []), AIdentifier "x")),
        runParser algebraicExpression "R:=rs()x")
