namespace While2Arr

module Translator =

    module While = While.AST
    module Arr = Arr.AST
   
    //Store all temp variables under one string
    let TEMP = "temp"

    let ZERO = Arr.Aexp.Int(0)
    let ONE = Arr.Aexp.Int(1)

    //Treat Boolean values as 0 = false and 1 = true
    let FALSE = ZERO
    let TRUE = ONE
    
    //Treat Boolean Intersection as addition
    let BOTH_AND = Arr.Aexp.Int(2) 

    //Default Variable indexes for operations
    let VAR_INDEX = Arr.Aexp.Int(0)
    let SKIP_INDEX = Arr.Aexp.Int(-1)

    //Convert list of statements into sequence chain
    let rec private toSeq (x:Arr.Stm list)  =
        match x with
        | fst :: [] -> fst
        | fst :: snd :: [] -> Arr.Stm.Seq(fst, snd)
        | fst :: tail -> Arr.Stm.Seq(fst, ( toSeq tail ))
        | [] -> failwith "Should not have empty lists"

    //Generate new unique index on every invocation
    let private genIndex = 
        let next = ref 0
        fun() ->
            incr next
            Arr.Aexp.Int(!next)

    //Provided, but as numeric operations are almost 1-1 between the languages pretty simple
    let rec private xlateAexp (exp : While.Aexp) : Arr.Aexp =
        match exp with
        | While.Aexp.Int(n) -> Arr.Aexp.Int(n)
        | While.Aexp.Var(x) -> Arr.Aexp.Arr(x, VAR_INDEX)
        | While.Aexp.Add(a1, a2) -> Arr.Aexp.Add(xlateAexp a1, xlateAexp a2)
        | While.Aexp.Mul(a1, a2) -> Arr.Aexp.Mul(xlateAexp a1, xlateAexp a2)    
        | While.Aexp.Sub(a1, a2) -> Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)

    (*
        Convert all boolean expression into a sequence of operations with the
        correpsonding temp variable offset of where the result is stored.
        The result will be a boolean cast into an integer (0 = false and 1 = true) 

        Boolean operators are complicated because the only comparison Arr provides, is the 
        less than or equal which determines whether or not to execute the loop body.

        What I have done is basically to set a default value that can be overwritten
        if the for loop executes.  Also, once the loop executes, we need to terminate the
        loop to prevent unecessary iterations.
    *)
    let rec private xlateBexp (exp : While.Bexp) : Arr.Stm * Arr.Aexp = 
        //Allocate return index
        let out  = genIndex ()

        match exp with
        | While.Bexp.True  -> (Arr.Stm.Assign(TEMP, out, TRUE),  out) //Map True  to 1
        | While.Bexp.False -> (Arr.Stm.Assign(TEMP, out, FALSE), out) //Map False to 0
        | While.Bexp.Eq(a1, a2) -> 
            //Need to check both directions due to the nature of <=.  We essentially need
            // to verify that a1 is <= a2 and a2 <= a1
            let a1' = xlateAexp a1
            let a2' = xlateAexp a2
            let itr  = genIndex ()
            let bound = genIndex()

            (toSeq [
                Arr.Stm.Assign(TEMP, out, TRUE);

                //Check a1 <= a2, if a1 <= a2 -1, then a1 < a2 and a1 != a2
                Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Sub(a2', ONE));
                Arr.For(TEMP, itr,
                    a1', 
                    Arr.Aexp.Arr(TEMP, bound), 
                    toSeq [
                        Arr.Stm.Assign(TEMP, out, FALSE);
                        Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Sub(a1', ONE))
                    ]);

                //Check a2 <= a1, if a2 <= a1 -1, then a2 < a1 and a2 != a1
                Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Sub(a1', ONE));
                Arr.For(TEMP, itr, 
                    a2', 
                    Arr.Aexp.Arr(TEMP, bound), 
                    toSeq [
                        Arr.Stm.Assign(TEMP, out, FALSE);
                        Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Sub(a2', ONE))
                    ])
           ], out)
        | While.Bexp.Lte(a1, a2) -> 
            //Since Lte is almost a 1-1 mapping with While, we just need  

            let a1' = xlateAexp a1
            let a2' = xlateAexp a2
            let itr  = genIndex ()
            let bound = genIndex ()

            (toSeq [
                Arr.Stm.Assign(TEMP, out, FALSE); 
                Arr.Stm.Assign(TEMP, bound, a2');
                Arr.For(TEMP, itr, 
                    a1', 
                    Arr.Aexp.Arr(TEMP, bound),
                    toSeq [
                        //Mark Lte comparison as true
                        Arr.Stm.Assign(TEMP, out, TRUE);
                        //Set final bound to a2 - 1 to enforce termination
                        Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Sub(a1', ONE))  
                 ])
           ], out)
        | While.Bexp.Not(b1) -> 
            (*
              Since True is 1, We loop from 1 to b1 (0 or 1) if b1 is 1 (True), 
              then we can execute the body which is to set the flag as 0 (False).
              Because the default value is 1 (True), this results in:
                  1 mapping to 0 and
                  0 mapping to 1
            *)
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
            (*
               We handle And by means of addition.  We add both int values
               and then for loop from 2 to sum.  If sum is 2 both b1 and b2 are 1 (True).

               If either b1 or b2 is 0, then the loop never executes.  This allows us to default to 
               0 (False) and then set 1 (True) if and only if the loop executes
            *)
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
        | While.Stm.Seq(s1, s2) ->  Arr.Stm.Seq(While2Arr s1, While2Arr s2)
        | While.Stm.Assign(var, a1) -> Arr.Stm.Assign(var, VAR_INDEX, xlateAexp a1)
        | While.Stm.Skip -> Arr.Stm.Assign(TEMP, SKIP_INDEX, FALSE) //Define skip as assigning 0 to an unused index
        | While.Stm.IfElse(b1, s1, s2) -> 
            (*
               Since 1 is True and 0 is False we can handle both clauses of the If/Else in the same manner.
               For If (True) we Loop from 1 to b1 (and execute s1 if and only if b1 is 1)
               For If (False) we Loop from b1 to 0 (and execute s2 if and only if b1 is 0)
            *)
            let (stm, ind) = xlateBexp b1
            let s1' = While2Arr s1
            let s2' = While2Arr s2
            let itr = genIndex ()

            toSeq [ 
                stm;
                Arr.Stm.For(TEMP, itr, 
                    TRUE, //ONE 
                    Arr.Aexp.Arr(TEMP, ind), 
                    s1');
                Arr.Stm.For(TEMP, itr, 
                    Arr.Aexp.Arr(TEMP, ind), 
                    FALSE, //ZERO
                    s2')
           ]
        | While.Stm.While(b1, s1) -> 
            (*
              The trickiest part of translate was getting the While loop to iterate as long as the predicate held true
              I set the lower bound of the for loop to 1 (True) and the upper bound of the loop to the int value of the
              predicate so that the loop will only begin iterating if the predicate evaluates to 1 (True).  

              On every iteration, we re-evaluate the predicate against the current state.  I set the
              upper bound = upper bound + predicate (1 for True of 0 for False).  As long as the predicate is True
              the upper bound will continue to increase allowing the for loop to continue indefinitely.
            *)
            let (stm, ind) = xlateBexp b1
            let s1' = While2Arr s1
            let itr = genIndex ()
            let bound = genIndex ()
          
            toSeq [
                stm;
                Arr.Stm.Assign(TEMP, bound, Arr.Aexp.Arr(TEMP, ind));
                Arr.Stm.For(TEMP, itr, 
                    TRUE, //ONE 
                    Arr.Aexp.Arr(TEMP, bound), 
                    toSeq [
                        s1'; 
                        stm;
                        Arr.Stm.Assign(TEMP, bound, 
                            Arr.Aexp.Add(
                                Arr.Aexp.Arr(TEMP, bound), 
                                Arr.Aexp.Arr(TEMP, ind)))
                    ])
            ]