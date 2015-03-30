namespace While2Arr

module Translator =

    module While = While.AST
    module Arr = Arr.AST
   
    let TEMP = "temp"
    let FOR_QUIT_MAX = Arr.Aexp.Int(0x7FFFFFFF-1)
    let FOR_QUIT_MIN = Arr.Aexp.Int(-0x7FFFFFFF+1)
    let FALSE = Arr.Aexp.Int(0)
    let ZERO = Arr.Aexp.Int(0)
    let TRUE = Arr.Aexp.Int(1)
    let BOTH_AND = Arr.Aexp.Int(2)
    let VAR_INDEX = Arr.Aexp.Int(0)
    let SKIP_INDEX = Arr.Aexp.Int(-1)

    let rec private toSeq (x:Arr.Stm list)  =
        match x with
        | fst :: [] -> fst
        | fst :: snd :: [] -> Arr.Stm.Seq(fst, snd)
        | fst :: tail -> Arr.Stm.Seq(fst, ( toSeq tail ))
        | [] -> failwith "Should not have empty lists"

    let private genIndex = 
        let next = ref 0
        fun() ->
            incr next
            Arr.Aexp.Int(!next)

    let rec private xlateAexp (exp : While.Aexp) : Arr.Aexp =
        match exp with
        | While.Aexp.Int(n) -> Arr.Aexp.Int(n)
        | While.Aexp.Var(x) -> Arr.Aexp.Arr(x, VAR_INDEX)
        | While.Aexp.Add(a1, a2) -> Arr.Aexp.Add(xlateAexp a1, xlateAexp a2)
        | While.Aexp.Mul(a1, a2) -> Arr.Aexp.Mul(xlateAexp a1, xlateAexp a2)    
        | While.Aexp.Sub(a1, a2) -> Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)

    let rec private xlateBexp (exp : While.Bexp) : Arr.Stm * Arr.Aexp = 
        let out  = genIndex ()

        match exp with
        | While.Bexp.True -> (Arr.Stm.Assign(TEMP, out, TRUE), out)
        | While.Bexp.False -> (Arr.Stm.Assign(TEMP, out, FALSE), out)
        | While.Bexp.Eq(a1, a2) -> 
            let a1' = xlateAexp a1
            let a2' = xlateAexp a2
            let itr  = genIndex ()
            let bound = genIndex ()

            (toSeq [
                Arr.Stm.Assign(TEMP, out, TRUE);
                Arr.Stm.Assign(TEMP, bound, ZERO);
                Arr.For(TEMP, itr,
                    Arr.Aexp.Add(Arr.Aexp.Sub(a1', a2'), Arr.Aexp.Int(1)), 
                    Arr.Aexp.Arr(TEMP, bound), 
                    toSeq [
                        Arr.Stm.Assign(TEMP, out, FALSE);
                        Arr.Stm.Assign(TEMP, bound, FOR_QUIT_MIN)
                    ]);
                Arr.Stm.Assign(TEMP, bound, ZERO);
                Arr.For(TEMP, itr, 
                    Arr.Aexp.Arr(TEMP, bound), 
                    Arr.Aexp.Sub(Arr.Aexp.Sub(a1', a2'), Arr.Aexp.Int(1)), 
                    toSeq [
                        Arr.Stm.Assign(TEMP, out, FALSE);
                        Arr.Stm.Assign(TEMP, bound, FOR_QUIT_MAX)
                    ])
           ], out)
        | While.Bexp.Lte(a1, a2) -> 
            let a1' = xlateAexp a1
            let a2' = xlateAexp a2
            let itr  = genIndex ()
            let bound = genIndex ()

            (toSeq [
                Arr.Stm.Assign(TEMP, out, FALSE);
                Arr.Stm.Assign(TEMP, bound, ZERO);
                Arr.For(TEMP, itr, 
                    Arr.Aexp.Sub(a1', a2'), 
                    Arr.Aexp.Arr(TEMP, bound), 
                    toSeq [
                        Arr.Stm.Assign(TEMP, out, TRUE);
                        Arr.Stm.Assign(TEMP, bound, FOR_QUIT_MIN)
                 ])
           ], out)
        | While.Bexp.Not(b1) -> 
            let (stm, ind) = xlateBexp b1
            let itr  = genIndex ()

            (toSeq [
                stm;
                Arr.Stm.Assign(TEMP, out, TRUE);
                Arr.Stm.For(TEMP, itr,
                    TRUE, 
                    Arr.Aexp.Arr(TEMP, ind), 
                    Arr.Stm.Assign(TEMP, out, FALSE))
            ], out)       
        | While.Bexp.And(b1, b2) -> 
            let (stm1, ind1) = xlateBexp b1
            let (stm2, ind2) = xlateBexp b2
            let itr  = genIndex ()
            let itr' = genIndex ()

            (toSeq [
                Arr.Stm.Assign(TEMP, out, FALSE);
                stm1;
                stm2;
                Arr.Stm.Assign(TEMP, itr, 
                    Arr.Aexp.Add(
                        Arr.Aexp.Arr(TEMP, ind1), 
                        Arr.Aexp.Arr(TEMP, ind2)));
                Arr.Stm.For(TEMP, itr', 
                    BOTH_AND, 
                    Arr.Aexp.Arr(TEMP, itr), 
                    Arr.Stm.Assign(TEMP, out, TRUE))
            ], out)

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
        match stm with
        | While.Stm.Seq(s1, s2) -> 
            let s1' = While2Arr s1
            let s2' = While2Arr s2
            Arr.Stm.Seq(s1', s2')
        | While.Stm.Assign(var, a1) -> 
            let a1' = xlateAexp a1
            Arr.Stm.Assign(var, VAR_INDEX, a1')
        | While.Stm.Skip -> Arr.Stm.Assign(TEMP, SKIP_INDEX, FALSE)
        | While.Stm.IfElse(b1, s1, s2) -> 
            let itr = genIndex ()
            let (stm', ind) = xlateBexp b1
            let s1' = While2Arr s1
            let s2' = While2Arr s2
            toSeq [ 
                stm';
                Arr.Stm.For(TEMP, itr, 
                    TRUE, 
                    Arr.Aexp.Arr(TEMP, ind), 
                    s1');
                Arr.Stm.For(TEMP, itr, 
                    Arr.Aexp.Arr(TEMP, ind), 
                    FALSE, 
                    s2')
           ]
        | While.Stm.While(b1, s1) -> 
            let itr = genIndex ()
            let (stm', ind) = xlateBexp b1
            let s1' = While2Arr s1
            let lower = genIndex ()
            let upper = genIndex ()
          
            toSeq [
                stm';
                Arr.Stm.Assign(TEMP, lower, TRUE);
                Arr.Stm.Assign(TEMP, upper, Arr.Aexp.Arr(TEMP, ind));
                Arr.Stm.For(TEMP, itr, 
                    Arr.Aexp.Arr(TEMP, lower), 
                    Arr.Aexp.Arr(TEMP, upper), 
                    toSeq [
                        s1'; 
                        stm';
                        Arr.Stm.Assign(TEMP, upper, 
                            Arr.Aexp.Add(
                                Arr.Aexp.Arr(TEMP, upper), 
                                Arr.Aexp.Arr(TEMP, ind)))
                    ])
            ]