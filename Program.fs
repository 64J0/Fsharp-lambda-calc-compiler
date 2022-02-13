module Fang.Program

let evalPrint expr =
    let start = System.DateTime.Now
    try
        let result = Lang.eval Map.empty expr
        let finish = System.DateTime.Now
        let duration = finish - start
        printfn $"[{duration.TotalMilliseconds}ms]>> {result}"
    with Lang.EvalException error -> 
        printfn $"[!!]>> {error}"

evalPrint (Lang.Ex.incrApp 42)
evalPrint (Lang.Ex.addApp 42 31)
evalPrint (Lang.Ex.subApp 42 31)
evalPrint (Lang.Ex.fib 5)
evalPrint (Lang.Ex.fib 10)
evalPrint (Lang.Ex.fib 15)
evalPrint (Lang.Ex.fib 20)
evalPrint (Lang.Ex.fib 26)
evalPrint (Lang.Ex.fib 30)
evalPrint (Lang.Ex.fibDirect 30)
evalPrint (Lang.Ex.badRecBinding)