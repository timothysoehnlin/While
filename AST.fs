namespace While

/// Contains types for constructing abstract syntax trees for While programs.
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