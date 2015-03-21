namespace Arr

module AST =

    type Aexp =
    | Int of int
    | Arr of string * Aexp
    | Add of Aexp * Aexp
    | Mul of Aexp * Aexp
    | Sub of Aexp * Aexp

    type Stm =
    | Assign of string * Aexp * Aexp
    | Seq of Stm * Stm
    | For of string * Aexp * Aexp * Aexp * Stm

module Interpreter =
    open AST

    type State = Map<string, Map<int, int>>

    let rec EvalAexp (exp : Aexp) (s : State) : int =
        match exp with
        | Int(n) -> n
        | Arr(x, a) -> match Map.tryFind x s with
                       | Some(map) ->
                           match Map.tryFind (EvalAexp a s) map with
                           | Some(n) -> n
                           | None -> 0
                       | None -> 0
        | Add(a1, a2) -> (EvalAexp a1 s) + (EvalAexp a2 s)
        | Mul(a1, a2) -> (EvalAexp a1 s) * (EvalAexp a2 s)
        | Sub(a1, a2) -> (EvalAexp a1 s) - (EvalAexp a2 s)

    let rec Interpret (stm : Stm) (s : State) : State =
        match stm with
        | Seq(s1, s2) -> Interpret s2 (Interpret s1 s)
        | Assign(variable, indexExpr, valueExpr) ->
            let index = EvalAexp indexExpr s
            let value = EvalAexp valueExpr s
            match Map.tryFind variable s with
            | Some(map) ->
                Map.add variable (Map.add index value map) s
            | None ->
                Map.add variable (Map.ofList [(index, value)]) s
        | For(variable, indexExpr, fromExpr, thruExpr, body) ->
            if EvalAexp fromExpr s > EvalAexp thruExpr s then s
            else let fstIter = Seq(Assign(variable, indexExpr, fromExpr), body)
                 let s' = Interpret fstIter s

                 let lb = match Add(fromExpr, Int(1)) with // Simplify
                          | Add(Add(expr, Int(1)), Int(1)) -> Add(expr, Int(2))
                          | expr -> expr
                 let otherIters = For(variable, indexExpr, lb, thruExpr, body)

                 Interpret otherIters s'
