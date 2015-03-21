namespace While

open System.IO
open Microsoft.FSharp.Text.Lexing
open While.AST
open While.Interpreter
open While.Compiler

module Main =
   
    let private rand = System.Random()

    /// Returns a State with the given variables assigned random values
    /// in the range 0 .. 10.
    let private randomStateWithVars (vars : List<string>) : State =
        Map.ofList [ for v in vars -> (v, rand.Next(11)) ]

    /// Returns a concise, human-readable description of a state
    let private print (state : State) =
        sprintf "[%s]"
            (String.concat ", "
                [for var, value in (Map.toList state) ->
                    sprintf "%s=%d" var value])

    /// Runs a program from While.Examples with a random input using the
    /// Natural Semantics, SOS, and Denotational Semantics-based
    /// interpreters, and ensures that they all return the same result
    let private check program =
        let name, inputs, outputs, stm = program
        printf "Checking %-17s" (name + "...")
        let startState = randomStateWithVars inputs
        let nsEndState = NaturalSemantics.Interpret stm startState
        let sosEndState = SOS.Interpret stm startState
        let dsEndState = DenotationalSemantics.Interpret stm startState
        if nsEndState = sosEndState && sosEndState = dsEndState then
            printfn "OK\t%s -> %s" (print startState) (print nsEndState)
        else
            failwithf "FAILURE\tNS:  %s -> %s\n\tSOS: %s -> %s\n\DS: %s -> %s"
                (print startState) (print nsEndState)
                (print startState) (print sosEndState)
                (print startState) (print dsEndState)

    [<EntryPoint>]
    let main args =
        for program in While.Examples.AllExamples do check program
        printfn "Enter a While program (Ctrl+Z on new line when finished): "
        try
            let lexbuf = LexBuffer<char>.FromTextReader System.Console.In
            let ast = Parser.ParseStm Lexer.NextToken lexbuf
            printfn "AST: %A" ast

            let desktop = System.Environment.GetFolderPath System.Environment.SpecialFolder.Desktop
            Compile ast "program" desktop
            printfn "Compiled to %s\\program.exe" desktop

            printfn "Interpreting..."
            printfn "%A" (NaturalSemantics.Interpret ast Map.empty)
        with
        | Lexer.LexerError(msg)   -> fprintf System.Console.Error "Lexer Error: %s\n" msg
        | Parser.SyntaxError(msg) -> fprintf System.Console.Error "Syntax Error: %s\n" msg
        | _ as ex -> fprintf System.Console.Error "Error: %s\n" ex.Message
        0