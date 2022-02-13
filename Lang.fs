module Fang.Lang

// variable:    var -> x, y
// abstraction: abs -> \x, (x + 1)
// application: app -> (\x, x + 1) y

type VarName = string

type BType =
    Int of int

type ArithmeticFn =
    | Add
    | Sub
    | Mul
    | Div

module ArithmeticFn =
    let apply (fn: ArithmeticFn) (a: int) (b: int) : int =
        match fn with
        | Add -> a + b
        | Sub -> a - b
        | Mul -> a * b
        | Div -> a / b

type ComparisonFn =
    | Less
    | Equal
    | Greater

module ComparisonFn =
    let apply (fn: ComparisonFn) (lhs: int) (rhs: int) : int =
        match fn with
        | Less -> if lhs < rhs then 1 else 0
        | Equal -> if lhs = rhs then 1 else 0
        | Greater -> if lhs > rhs then 1 else 0

type BuiltinFn =
    | Arithmetic of fn: ArithmeticFn * opA: Expr * opB: Expr
    | Comparison of fn: ComparisonFn * lhs: Expr * rhs: Expr
and Expr =
    | Var of VarName
    | Abs of var: VarName * body: Expr
    | App of expr: Expr * arg: Expr
    | Lit of BType
    | Builtin of BuiltinFn
    | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
    | Bind of recursive: bool * var: VarName * body: Expr * expr: Expr

[<RequireQualifiedAccess>]
type Value =
    | Int of int
    | Closure of Closure
    | BlackHole 
and Closure = 
    { mutable env: Env; 
      var: VarName; 
      body: Expr }
and Env = Map<VarName, Value>

type EvalError =
    | WrongType of Value * expectedType: string
    | UnboundName of VarName
    | UnevalName of VarName

exception EvalException of EvalError

module Value =
    let asInt =
        function
        | Value.Int i -> i
        | other -> raise (EvalException(WrongType (other, "int")))

    let asClosure =
        function
        | Value.Closure c -> c
        | other -> raise (EvalException(WrongType (other, "closure")))

    let checkBlackHole var =
        function
        | Value.BlackHole -> raise(EvalException(UnevalName var))
        | other -> other

let rec eval (env: Env) (expr: Expr) : Value =
    match expr with
    | Lit (BType.Int i) -> Value.Int i
    | Builtin (Arithmetic(fn, opA, opB)) ->
        let valA = eval env opA |> Value.asInt
        let valB = eval env opB |> Value.asInt
        ArithmeticFn.apply fn valA valB |> Value.Int
    | Builtin (Comparison(fn, lhs, rhs)) ->
        let lhs = eval env lhs |> Value.asInt
        let rhs = eval env rhs |> Value.asInt
        ComparisonFn.apply fn lhs rhs |> Value.Int
    | Cond (pred, trueBranch, falseBranch) ->
        let valPred = eval env pred |> Value.asInt
        if valPred <> 0 then 
            eval env trueBranch 
        else 
            eval env falseBranch
    | Abs(var, body) ->
        // (\x. \y. x + y) 5 -> (\y. x + y {x -> 5})
        Value.Closure { env = env; var = var; body = body }
    | App (expr, arg) ->
        let { env = closureEnv; var = closureVar; body = closureBody } = eval env expr |> Value.asClosure
        let argValue = eval env arg
        let newEnv = Map.add closureVar argValue closureEnv
        eval newEnv closureBody
    | Var name -> 
        Map.tryFind name env 
        |> Option.map (Value.checkBlackHole name)
        |> Option.defaultWith (fun () -> raise(EvalException(UnboundName(name))))
    | Bind (recursive, var, body, expr) ->
        let bodyEnv = 
            if not recursive then 
                env 
            else 
                Map.add var Value.BlackHole env

        let bodyVal = 
            match eval bodyEnv body with
            | Value.Closure closure as v -> 
                let closureEnv = Map.add var v env
                closure.env <- closureEnv
                v
            | other -> other

        let exprEnv = Map.add var bodyVal env
        eval exprEnv expr

module Ex =
    let lit (n: int) = Lit (BType.Int n)

    let incrFn = 
        // \x. (x + 1)
        Abs(
            var = "x",
            body = Builtin (Arithmetic (Add, Var "x", lit 1))
        )

    let incrApp (n: int) = App(expr = incrFn, arg = lit n)

    let addFn =
        // \x. (\y. (x + y))
        Abs(
            var = "x",
            body = Abs(
                var = "y",
                body = Builtin (Arithmetic(Add, Var "x", Var "y")))
        )

    let addApp (x: int) (y: int) =
        App(App(addFn, lit x), lit y)

    let subFn =
        // \x. (\y. (x - y))
        Abs(
            var = "x",
            body = Abs(
                var = "y",
                body = Builtin (Arithmetic(Sub, Var "x", Var "y")))
        )
    
    let subApp (x: int) (y: int) =
        App(App(subFn, lit x), lit y)

    // fib n = if n < 2 then 1 else fib (n - 1) * fib (n - 2)

    let lazyFixpoint =
        // Y = \f. (\x. f (x x)) (\x. f (x x))
        let innerAbs = Abs (
            var = "x", 
            body = App (expr = Var "f", arg = App(expr = Var "x", arg = Var "x")))

        Abs (var = "f", body = App(expr = innerAbs, arg = innerAbs))

    let eagerFixpoint =
        // Y = \f . (\x. f (\v. x x v)) (\x. f (x x v))

        let indirection = Abs (
            "v",
            App(App(Var "x", Var "x"), Var "v")
        )

        let innerAbs = Abs (
            var = "x", 
            body = App (expr = Var "f", arg = indirection)
        )

        Abs (var = "f", body = App(expr = innerAbs, arg = innerAbs))

    let fibStep =
        // \f. \x if n < 2 then 1 else f (x - 1) + f (x - 2)
        let xMinus n =
            Builtin(Arithmetic(Sub, Var "x", lit n))
        
        let falseBranch =
            Builtin(Arithmetic(Add, App(Var "f", xMinus 1), App(Var "f", xMinus 2)))

        Abs (
            var = "f",
            body = 
                Abs (
                    var = "x", 
                    body = 
                        Cond (
                            pred = Builtin(Comparison(Less, Var "x", lit 2)),
                            trueBranch = lit 1,
                            falseBranch = falseBranch
                        )
                    )
            )

    let fib (n: int) =
        let fn = App (eagerFixpoint, fibStep)
        App (fn, lit n)

    let fibDirect (n: int) =
        let xMinus n =
            Builtin(Arithmetic(Sub, Var "x", lit n))

        let falseBranch =
            Builtin(Arithmetic(Add, App(Var "fib", xMinus 1), App(Var "fib", xMinus 2)))

        Bind(
            recursive = true,
            var = "fib",
            body =
                Abs(
                    var = "x",
                    body =
                        Cond (
                            pred = Builtin(Comparison(Less, Var "x", lit 2)),
                            trueBranch = lit 1,
                            falseBranch = falseBranch
                        )
                ),
            expr = App(Var "fib", lit n)
        )

    let badRecBinding =
        Bind(recursive = true, var = "x", body = Builtin(Arithmetic(Add, Var "x", lit 1)), expr = Var "x")