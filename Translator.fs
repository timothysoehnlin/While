namespace While2Arr

module Translator =

    module While = While.AST
    module Arr = Arr.AST

    let rec private xlateAexp (exp : While.Aexp) : Arr.Aexp =
        match exp with
        | While.Int(n) -> Arr.Aexp.Int(n)
        | While.Var(x) -> Arr.Aexp.Arr(x, Arr.Int(0))
        | While.Add(a1, a2) -> Arr.Aexp.Add(xlateAexp a1, xlateAexp a2)
        | While.Mul(a1, a2) -> Arr.Aexp.Mul(xlateAexp a1, xlateAexp a2)
        | While.Sub(a1, a2) -> Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)

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
        failwithf "NOT YET IMPLEMENTED"
