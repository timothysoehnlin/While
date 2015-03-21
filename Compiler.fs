namespace While

open System
open System.Reflection
open System.Reflection.Emit

open AST

/// Contains a Compile method, which compiles a While statement to a
/// .NET executable.
module Compiler =

    /// Returns a list of all the variable names that occur in a statement.
    let private listVars (stm : Stm) =
        let rec varsInAexp exp =
            match exp with
            | Int(n)      -> Set.empty
            | Var(x)      -> Set.singleton x
            | Add(a1, a2)
            | Mul(a1, a2)
            | Sub(a1, a2) -> Set.union (varsInAexp a1) (varsInAexp a2)
        let rec varsInBexp exp =
            match exp with
            | True        -> Set.empty
            | False       -> Set.empty
            | Eq(a1, a2)
            | Lte(a1, a2) -> Set.union (varsInAexp a1) (varsInAexp a2)
            | Not(b)      -> varsInBexp b
            | And(b1, b2) -> Set.union (varsInBexp b1) (varsInBexp b2)
        let rec varsInStm stm =
            match stm with
            | Assign(x, a)      -> Set.union (Set.singleton x) (varsInAexp a)
            | Skip              -> Set.empty
            | Seq(s1, s2)       -> Set.union (varsInStm s1) (varsInStm s2)
            | IfElse(b, s1, s2) -> Set.unionMany [ (varsInBexp b); (varsInStm s1); (varsInStm s2) ]
            | While(b, body)    -> Set.union (varsInBexp b) (varsInStm body)
        Set.toList (varsInStm stm)

    /// Iterates through a list of variable names, adding a local variable
    /// declaration for each variable using an ILGenerator.
    let private initLocals (varList : list<string>) (ilg : ILGenerator) =
        for var in varList do
            ilg.DeclareLocal(typeof<int>) |> ignore

    /// Emits the CIL translation of an arithmetic expression.
    let rec private xlateAexp (exp : Aexp) (vars : Map<string, int>) (ilg : ILGenerator) : unit =
        match exp with
        | Int(n) ->
            ilg.Emit(OpCodes.Ldc_I4, n)
        | Var(x) ->
            ilg.Emit(OpCodes.Ldloc, Map.find x vars)
        | Add(a1, a2) ->
            xlateAexp a1 vars ilg
            xlateAexp a2 vars ilg
            ilg.Emit(OpCodes.Add)
        | Mul(a1, a2) ->
            xlateAexp a1 vars ilg
            xlateAexp a2 vars ilg
            ilg.Emit(OpCodes.Mul)
        | Sub(a1, a2) ->
            xlateAexp a1 vars ilg
            xlateAexp a2 vars ilg
            ilg.Emit(OpCodes.Sub)

    /// Emits the CIL translation of a Boolean expression.
    let rec private xlateBexp (exp : Bexp) (vars : Map<string, int>) (ilg : ILGenerator) : unit =
        match exp with
        | True ->
            ilg.Emit(OpCodes.Ldc_I4_1)
        | False ->
            ilg.Emit(OpCodes.Ldc_I4_0)
        | Eq(a1, a2) ->
            xlateAexp a1 vars ilg
            xlateAexp a2 vars ilg
            ilg.Emit(OpCodes.Ceq)
        | Lte(a1, a2) ->
            xlateAexp a1 vars ilg
            xlateAexp a2 vars ilg
            ilg.Emit(OpCodes.Cgt)
            ilg.Emit(OpCodes.Ldc_I4_0)
            ilg.Emit(OpCodes.Ceq)
        | Not(b) ->
            xlateBexp b vars ilg
            ilg.Emit(OpCodes.Ldc_I4_0)
            ilg.Emit(OpCodes.Ceq)
        | And(b1, b2) ->
            xlateBexp b1 vars ilg
            xlateBexp b2 vars ilg
            ilg.Emit(OpCodes.And)

    /// Emits the CIL translation of a statement (or sequence of statements).
    let rec private xlateStm (stm : Stm) (vars : Map<string, int>) (ilg : ILGenerator) : unit =
        match stm with
        | Assign(varName, exp) ->
            xlateAexp exp vars ilg
            ilg.Emit(OpCodes.Stloc, Map.find varName vars)
        | Skip ->
            ilg.Emit(OpCodes.Nop)
        | Seq(stm1, stm2) ->
            xlateStm stm1 vars ilg
            xlateStm stm2 vars ilg
        | IfElse(testExp, thenStm, elseStm) ->
            let elseLbl = ilg.DefineLabel()
            let doneLbl = ilg.DefineLabel()
            xlateBexp testExp vars ilg
            ilg.Emit(OpCodes.Brfalse, elseLbl)
            xlateStm thenStm vars ilg
            ilg.Emit(OpCodes.Br, doneLbl)
            ilg.MarkLabel(elseLbl)
            xlateStm elseStm vars ilg
            ilg.MarkLabel(doneLbl)
        | While(testExp, body) ->
            let topLbl = ilg.DefineLabel()
            let doneLbl = ilg.DefineLabel()
            ilg.MarkLabel(topLbl)
            xlateBexp testExp vars ilg
            ilg.Emit(OpCodes.Brfalse, doneLbl)
            xlateStm body vars ilg
            ilg.Emit(OpCodes.Br, topLbl)
            ilg.MarkLabel(doneLbl)

    /// Emits code to display all of the variables that occur in a statement,
    /// along with their values.  Variable names are detemined by the given
    /// list; a variable's position in the list (0, 1, 2, ...) is assumed to
    /// correspond to its local variable index in the generated CIL.
    let private emitDisplayAllVars (varList : list<string>) (ilg : ILGenerator) : unit =
        let display (index : int) (varName : string) : unit =
            ilg.Emit(OpCodes.Ldstr, varName)
            ilg.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<string> |]))
            ilg.Emit(OpCodes.Ldstr, " = ")
            ilg.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<string> |]))
            ilg.Emit(OpCodes.Ldloc, index)
            ilg.Emit(OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<int> |]))
        List.iteri display varList

    /// Compiles a While statement, saving an executable with the given
    /// assembly name in the given directory.
    let Compile (stm : Stm) (assemblyName : string) (directory : string) : unit =
        let an = AssemblyName(assemblyName)
        let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.Save, directory)
        let mb = ab.DefineDynamicModule(an.Name, assemblyName + ".exe")
        let tb = mb.DefineType("Main", TypeAttributes.Public ||| TypeAttributes.Class)
        let fb = tb.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static,
                                 typeof<System.Void>, Array.empty)
        let ilg = fb.GetILGenerator()

        let varList = listVars stm
        initLocals varList ilg
        xlateStm stm (Map.ofList (List.mapi (fun idx name -> (name, idx)) varList)) ilg
        emitDisplayAllVars varList ilg
        ilg.Emit(OpCodes.Ret)

        let t = tb.CreateType()
        ab.SetEntryPoint(fb, PEFileKinds.ConsoleApplication)
        ab.Save(assemblyName + ".exe")
