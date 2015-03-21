namespace While2Arr

module Main =
   
    let private rand = System.Random()

    /// Returns a State with the given variables assigned random values
    /// in the range 0 .. 10.
    let private randomStateWithVars (vars : List<string>) : While.Interpreter.State =
        Map.ofList [ for v in vars -> (v, rand.Next(11)) ]

    /// Returns a concise, human-readable description of a While state
    let private printWhileState (state : While.Interpreter.State) =
        sprintf "[%s]" (String.concat ", "
            [for var, value in (Map.toList state) ->
                sprintf "%s = %d" var value])

    /// Returns a concise, human-readable description of an Arr state
    let private printArrState (state : Arr.Interpreter.State) =
        let printIndexArray a =
            sprintf "[%s]" (String.concat ", "
                [for idx, value in (Map.toList a) ->
                    sprintf "%d=%d" idx value])
        sprintf "[%s]" (String.concat ", "
            [for arr, entries in (Map.toList state) ->
                sprintf "%s = %s" arr (printIndexArray entries)])

    /// Converts a While state to a similar Arr state, where each While
    /// variable x has been mapped to x[0] in the Arr state.
    ///
    /// For example: [a->10, b->20] becomes [a->[0->10], b->[0->20]]
    let private convert (state : While.Interpreter.State) : Arr.Interpreter.State =
        let makeArray varName value = Map.ofList [ (0, value) ]
        Map.map makeArray state

    /// Removes all arrays whose name starts with "temp" from an Arr state.
    let private removeTempArrays (state : Arr.Interpreter.State) : Arr.Interpreter.State =
        Map.filter (fun name _ -> not (name.StartsWith "temp")) state

    /// Runs a While program, translates it to Arr, and then runs the Arr translation
    /// and checks that the resulting state corresponds to the output of the While
    /// program (after temporary arrays have been removed).
    let private check program =
        let name, inputs, outputs, stm = program
        printf "Checking %-17s" (name + "...")
        let translated = While2Arr.Translator.While2Arr stm

        let whileStartState = randomStateWithVars inputs
        let whileEndState = While.Interpreter.Interpret stm whileStartState

        let arrStartState = convert whileStartState
        let arrEndState = Arr.Interpreter.Interpret translated arrStartState

        let expected = convert whileEndState
        let actual = removeTempArrays arrEndState
        if expected = actual then
            printfn "OK"
        else
            let msg =
                (sprintf "FAILURE\n    While:    %s -> %s\n    Arr:      %s -> %s\n    Expected: %s -> %s"
                    (printWhileState whileStartState) (printWhileState whileEndState)
                    (printArrState arrStartState) (printArrState actual)
                    (printArrState arrStartState) (printArrState expected))
            failwith msg

    [<EntryPoint>]
    let main args =
        try
            for program in While.Examples.AllExamples do
                check program

            (* Test this manually.  The Arr interpreter will overflow the stack.
            printfn "Checking infinite loop..."
            let infinite = While2Arr.Translator.While2Arr (While.AST.While(While.AST.True, While.AST.Skip))
            Arr.Interpreter.Interpret infinite Map.empty |> ignore
            *)

            0
        with
        | Failure f ->
            fprintfn System.Console.Error "%s" (f.ToString())
            1
