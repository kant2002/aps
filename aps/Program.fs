open Interpreter
open System
open System.IO
open Argu

type CliArguments =
    | [<AltCommandLine("-i")>] Include_File of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Include_File _ -> "File to include for processing, can be multiple of them."


[<EntryPoint>]
let main (args) =
    match args with
    | [||] ->
        let programCode = Console.In.ReadToEnd()
        interpretProgram { source = "<stdin>" } programCode
    | _ -> 
        let parser = ArgumentParser.Create<CliArguments>(programName = "aps.exe")
        let results = parser.Parse (args, raiseOnUsage = false)
        if results.IsUsageRequested then
            printfn "%s" (parser.PrintUsage())
            Environment.Exit(0)
        let fileNames = results.GetResults Include_File
        for fileName in fileNames do
            let programCode = File.ReadAllText(fileName)
            interpretProgram { source = fileName } programCode
    0
