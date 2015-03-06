namespace While

open While.Interpreter

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

    open While.AST
    [<EntryPoint>]
    let main args =
        for program in While.Examples.AllExamples do check program
        (*
        let demo =
            While(Lte(Var("i"), Var("j")),
                Seq(Assign("i", Add(Var("i"), Int(1))),
                    Assign("j", Sub(Var("j"), Int(1)))))
        printfn "\nDemo Program\nImplementation of the Natural Semantics:"
        printfn "%A" (NaturalSemantics.Interpret demo (Map.ofList [("i",1); ("j",5)]))
        printfn "Implementation of the Structural Operational Semantics:"
        printfn "%A" (SOS.Interpret demo (Map.ofList [("i",1); ("j",5)]))
        printfn "Implementation of the Denotational Semantics:"
        printfn "%A" (DenotationalSemantics.Interpret demo (Map.ofList [("i",1); ("j",5)]))
        *)
        0