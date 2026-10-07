module InterpreterTests

open System
open System.Diagnostics
open Xunit
open Interpreter
open Parser

let testEmptyEnv statement = 
    let globalEnv = createNewEnv()
    interpret { source = "" } globalEnv statement

[<Fact>]
let ``Name declaration interpretation`` () =
    let statement = SNamesDeclaration [("x", None); ("y", None); ("z", Some 2)]
    let newEnv = testEmptyEnv statement
    let s = createProgramParser()
    Assert.Equal(3, newEnv.names.Count)
    Assert.Equal(VEmpty, newEnv.names["x"])
    Assert.Equal(VEmpty, newEnv.names["y"])
    Assert.Equal(VArray [VEmpty; VEmpty], newEnv.names["z"])

