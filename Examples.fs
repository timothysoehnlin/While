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

    let AllExamples = [DoNothing; Boring; IncreaseTo7; PowerOf2; Swap; GCD]