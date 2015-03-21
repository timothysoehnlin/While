namespace While

module AST =

    type Aexp =
    | Int of int
    | Var of string
    | Add of Aexp * Aexp
    | Mul of Aexp * Aexp
    | Sub of Aexp * Aexp

    type Bexp =
    | True
    | False
    | Eq of Aexp * Aexp
    | Lte of Aexp * Aexp
    | Not of Bexp
    | And of Bexp * Bexp

    type Stm =
    | Assign of string * Aexp
    | Skip
    | Seq of Stm * Stm
    | IfElse of Bexp * Stm * Stm
    | While of Bexp * Stm

module Interpreter =
    open AST

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
