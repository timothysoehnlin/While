namespace While2Arr

module Translator =

    module While = While.AST
    module Arr = Arr.AST

    let TEMP = "temp"
    let MAX = 0x7FFFFFFF

    let private genIndex = 
        let next = ref 0
        fun() ->
            incr next
            Arr.Aexp.Int(!next)

    let rec private xlateAexp (exp : While.Aexp) : Arr.Aexp =
        match exp with
        | While.Aexp.Int(n) -> Arr.Aexp.Int(n)
        | While.Aexp.Var(x) -> Arr.Aexp.Arr(x, Arr.Int(0))
        | While.Aexp.Add(a1, a2) -> Arr.Aexp.Add(xlateAexp a1, xlateAexp a2)
        | While.Aexp.Mul(a1, a2) -> Arr.Aexp.Mul(xlateAexp a1, xlateAexp a2)    
        | While.Aexp.Sub(a1, a2) -> Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)

    let rec private xlateBexp (exp : While.Bexp) : Arr.Stm * Arr.Aexp = 
        let out  = genIndex ()
        let itr  = genIndex ()
        let itr' = genIndex ()

        match exp with
        | While.Bexp.True -> (Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(1)), out)
        | While.Bexp.False -> (Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0)), out)
        | While.Bexp.Eq(a1, a2) -> 
            (Arr.Stm.Seq(
                Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(1)),
                Arr.Stm.Seq(
                    Arr.For(TEMP, itr,
                        Arr.Aexp.Add(Arr.Aexp.Int(1), Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)), Arr.Aexp.Int(0), 
                        Arr.Stm.Seq(
                            Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0)),
                            Arr.Stm.Assign(TEMP, itr, Arr.Aexp.Int(1)))),
                    Arr.For(TEMP, itr', 
                        Arr.Aexp.Int(0), Arr.Aexp.Add(Arr.Aexp.Int(1), Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)), 
                        Arr.Stm.Seq(
                            Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0)),
                            Arr.Stm.Assign(TEMP, itr', Arr.Aexp.Int(MAX)))))), out)
        | While.Bexp.Lte(a1, a2) -> 
            (Arr.Stm.Seq(
                Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0)),
                Arr.For(TEMP, itr, 
                    Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2), Arr.Aexp.Int(0), 
                    Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(1)))), out)
        | While.Bexp.Not(b1) -> 
            let (stm, ind) = xlateBexp b1
            (Arr.Stm.Seq(
                stm,
                Arr.Stm.Seq(
                    Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(1)),
                    Arr.Stm.For(TEMP, itr, Arr.Aexp.Int(1), Arr.Aexp.Arr(TEMP, ind), Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0))))), out)       
        | While.Bexp.And(b1, b2) -> 
            let (stm, ind) = xlateBexp b1
            let (stm', ind') = xlateBexp b2
            (Arr.Stm.Seq(
                Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(0)),
                Arr.Stm.Seq(
                    stm, 
                    Arr.Stm.Seq(
                        stm',
                        Arr.Stm.Seq(
                            Arr.Stm.Assign(TEMP, itr, Arr.Aexp.Add(Arr.Aexp.Arr(TEMP, ind), Arr.Aexp.Arr(TEMP, ind'))),
                            Arr.Stm.For(TEMP, itr', Arr.Aexp.Int(2), Arr.Aexp.Arr(TEMP, itr), Arr.Stm.Assign(TEMP, out, Arr.Aexp.Int(1))))))), out)        

    /// Translates a While program into an equivalent Arr program.
    ///
    /// A variable x in the While program is mapped to an array with
    /// one element in Arr.  So, x := 3 becomes x[0] := 3, and x := y
    /// becomes x[0] := y[0].
    ///
    /// The Arr program may contain assignments to temporary arrays.
    /// In the Arr program's output, any array whose name starts with
    /// "temp" should be ignored.
    let rec While2Arr (stm : While.Stm) : Arr.Stm =
        let itr = genIndex ()

        match stm with
        | While.Stm.Seq(s1, s2) -> Arr.Stm.Seq(While2Arr s1, While2Arr s2)
        | While.Stm.Assign(var, a1) -> Arr.Stm.Assign(var, Arr.Aexp.Int(0), xlateAexp a1)
        | While.Stm.Skip -> Arr.Stm.Assign(TEMP, itr, Arr.Aexp.Int(0))
        | While.Stm.IfElse(b1, s1, s2) -> 
            let stm, ind = xlateBexp b1
            let s1' = While2Arr s1
            let s2' = While2Arr s2
            Arr.Stm.Seq(stm,
                Arr.Stm.Seq(
                    Arr.Stm.For(TEMP, itr, Arr.Aexp.Int(1), Arr.Aexp.Arr(TEMP, ind), s1'),
                    Arr.Stm.For(TEMP, itr, Arr.Aexp.Arr(TEMP, ind), Arr.Aexp.Int(0), s2')))
        | While.Stm.While(b1, s1) -> 
            let stm, ind = xlateBexp b1
            Arr.Stm.Seq(stm,
                Arr.Stm.For(TEMP, itr, Arr.Aexp.Int(1), Arr.Aexp.Arr(TEMP, ind), 
                    Arr.Stm.Seq(
                        While2Arr s1, 
                        Arr.Stm.Seq(
                            Arr.Stm.Assign(TEMP, itr, Arr.Aexp.Int(1)),
                            stm))))