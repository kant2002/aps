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
        programParser.setDefaultMarks()
        //printfn "Processing file: %A" fileNames
        for fileName in fileNames do
            //printfn "Processing file: '%s'" fileName
            let programCode = File.ReadAllText(fileName.Trim())
            let result = interpretProgram { source = fileName } (programCode + " \n ; ")
            if result <> 0 then
                Environment.Exit(result)
        0
