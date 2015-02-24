namespace While

open While.AST

/// Contains some simple syntactic analyses for While programs, to illustrate
/// how to traverse While ASTs.
module Analyses =

    /// Returns true iff the statement contains at least one "while" loop.
    let rec ContainsWhile stm =
        match stm with
        | Assign(_, _) | Skip -> false
        | Seq(s1, s2)| IfElse(_, s1, s2) ->
            (ContainsWhile s1) || (ContainsWhile s2)
        | While(_, s) -> true

    /// Returns true iff the statement contains at least one loop of the form
    /// "while true do ..." (where the test expression is the literal "true")
    let rec ContainsWhileTrue stm =
        match stm with
        | Assign(_, _) | Skip -> false
        | Seq(s1, s2)| IfElse(_, s1, s2) ->
            (ContainsWhileTrue s1) || (ContainsWhileTrue s2)
        | While(True, _) -> true
        | While(_, s) -> ContainsWhileTrue s
