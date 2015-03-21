namespace While

open AST

/// Contains example While programs.  Each example is a quadruple with
/// (1) a name, (2) a list of input variables, (3) a list of output variables,
/// and (4) an AST for a While program.
module Examples =

    /// skip
    let DoNothing = ("Do Nothing", [], [], Skip)

    /// i := 3; j := 2
    let Boring =
        ("Boring", [], ["i"; "j"],
            Seq(Assign("i", Int(3)), Assign("j", Mul(Int(2), Var("i")))))

    let If = [
        ("If[0]", [], ["result"],
            IfElse(True, Assign("result", Int(1)), Skip));
        ("If[1]", [], ["result"],
            IfElse(False, Assign("result", Int(1)), Skip));
        ("If[2]", [], ["result"],
            IfElse(Not(True), Assign("result", Int(1)), Skip));
        ("If[3]", [], ["result"],
            IfElse(Not(False), Assign("result", Int(1)), Skip));
        ("If[4]", [], ["result"],
            IfElse(And(False, False), Assign("result", Int(1)), Skip));
        ("If[5]", [], ["result"],
            IfElse(And(False, True), Assign("result", Int(1)), Skip));
        ("If[6]", [], ["result"],
            IfElse(And(True, False), Assign("result", Int(1)), Skip));
        ("If[7]", [], ["result"],
            IfElse(And(True, True), Assign("result", Int(1)), Skip));
        ("If[8]", [], ["result"],
            IfElse(Eq(Int(3), Int(3)), Assign("result", Int(1)), Skip));
        ("If[9]", [], ["result"],
            IfElse(Eq(Int(3), Int(4)), Assign("result", Int(1)), Skip));
        ("If[10]", [], ["result"],
            IfElse(Eq(Int(3), Int(2)), Assign("result", Int(1)), Skip));
        ("If[11]", [], ["result"],
            IfElse(Lte(Int(3), Int(3)), Assign("result", Int(1)), Skip));
        ("If[12]", [], ["result"],
            IfElse(Lte(Int(3), Int(4)), Assign("result", Int(1)), Skip));
        ("If[13]", [], ["result"],
            IfElse(Lte(Int(3), Int(2)), Assign("result", Int(1)), Skip))
    ]

    /// If i < 7, increases i by 1 until it is equal to 7
    /// while (i ≤ 7) ∧ ¬(i = 7) do i := i + 1
    let IncreaseTo7 =
        ("Increase to 7", ["i"], ["i"],
            While(
                And(
                    Lte(Var("i"), Int(7)),
                    Not(Eq(Var("i"), Int(7)))),
                Assign("i", Add(Var("i"), Int(1)))))

    /// Computes the n-th power of 2 by brute force:
    /// result := 1; i := 1; while i ≤ n do ( result = 2 * result; i := i + 1 )
    let PowerOf2 =
        ("Power of 2", ["n"], ["result"],
            Seq(
                Seq(
                    Assign("result", Int(1)),
                    Assign("i", Int(1))),
                While(
                    Lte(Var("i"), Var("n")),
                    Seq(
                        Assign("result", Mul(Int(2), Var("result"))),
                        Assign("i", Add(Var("i"), Int(1)))))))

    /// Swaps x and y:
    /// z := x; x := y; y := z
    let Swap =
        ("Swap", ["x"; "y"], ["x"; "y"],
            Seq(
                Seq(
                    Assign("z", Var("x")),
                    Assign("x", Var("y"))),
                Assign("y", Var("z"))))

    /// Computes the GCD of a and b using the Euclidean Algorithm
    /// (both a and b must be positive)
    /// if a*b = 0 then skip else
    ///     while ¬(a = b) if ¬(a ≤ b) then a := a − b else b := b − a
    let GCD =
        ("GCD", ["a"; "b"], ["a"],
            IfElse(Eq(Mul(Var("a"), Var("b")), Int(0)),
                Skip,
                While(
                    Not(Eq(Var("a"), Var("b"))),
                    IfElse(
                        Not(Lte(Var("a"), Var("b"))),
                        Assign("a", Sub(Var("a"), Var("b"))),
                        Assign("b", Sub(Var("b"), Var("a")))))))

    let AllExamples = [DoNothing; Boring] @ If @ [IncreaseTo7; PowerOf2; Swap; GCD]