namespace While

open While.AST

/// Contains functions and submodules for evaluating While expressions and
/// interpreting While programs.
module Interpreter =

    /// A State maps a variable names to their (integer) values.  Variables
    /// not present in the map are assumed to have the value 0.
    type State = Map<string, int>

    /// Evaluates an arithmetic expression, using the given state to determine
    /// the value of variables used in the expression.
    let rec EvalAexp (exp : Aexp) (s : State) : int =
        match exp with
        | Int(n) -> n
        | Var(x) -> match Map.tryFind x s with
                    | Some(n) -> n
                    | None -> 0
        | Add(a1, a2) -> (EvalAexp a1 s) + (EvalAexp a2 s)
        | Mul(a1, a2) -> (EvalAexp a1 s) * (EvalAexp a2 s)
        | Sub(a1, a2) -> (EvalAexp a1 s) - (EvalAexp a2 s)

    /// Evaluates a Boolean expression, using the given state to determine the
    /// value of variables used in the expression.
    let rec EvalBexp (exp : Bexp) (s : State) : bool =
        match exp with
        | True  -> true
        | False -> false
        | Eq(a1, a2)  -> (EvalAexp a1 s) = (EvalAexp a2 s)
        | Lte(a1, a2) -> (EvalAexp a1 s) <= (EvalAexp a2 s)
        | Not(b)      -> not (EvalBexp b s)
        | And(b1, b2) -> (EvalBexp b1 s) && (EvalBexp b2 s)

    /// Contains a While interpretater based on a straightforward
    /// implementation of the natural semantics given in
    /// Nielson & Nielson, Semantics with Applications, page 20.
    module NaturalSemantics =

        /// Interprets a While statement, starting in the given state.  If the
        /// statement's execution terminates, the terminal state is returned.
        let rec Interpret (stm : Stm) (s : State) : State =
            match stm with
            | Assign(x, a) -> Map.add x (EvalAexp a s) s
            | Skip -> s
            | Seq(s1, s2) -> let s' = Interpret s1 s
                             Interpret s2 s'
            | IfElse(b, s1, s2) ->
                if (EvalBexp b s) then
                    Interpret s1 s
                else
                    Interpret s2 s
            | While(b, body) ->
                if EvalBexp b s then
                    let s' = Interpret body s
                    Interpret (While(b,body)) s'
                else
                    s

    /// Contains a While interpretater based on a straightforward
    /// implementation of the structural operational semantics given in
    /// Nielson & Nielson, Semantics with Applications, page 3.
    module SOS =
        /// Computes one step of the SOS (⇒) relation.
        /// If <stm, s> ⇒ <stm', s'>, returns (Some(stm'), s').
        /// If <stm, s> ⇒ s', returns (None, s').
        let rec private step stm s : Option<Stm> * State =
            match stm with
            | Assign(x, a) -> (None, Map.add x (EvalAexp a s) s)
            | Skip -> (None, s)
            | Seq(s1, s2) ->
                match step s1 s with
                | (Some(s1'), s') -> (Some(Seq(s1', s2)), s')
                | (None, s') -> (Some(s2), s')
            | IfElse(b, s1, s2) ->
                if EvalBexp b s then
                    (Some(s1), s)
                else
                    (Some(s2), s)
            | While(b, body) ->
                (Some(IfElse(b, Seq(body, While(b, body)), Skip)), s)

        /// Interprets a While statement, starting in the given state.  If the
        /// statement's execution terminates, the terminal state is returned.
        let rec Interpret stm state : State =
            match step stm state with
            | (Some(stm'), state') -> Interpret stm' state'
            | (None, state') -> state'
