module Fang.Program

let evalPrint expr =
    let start = System.DateTime.Now
    let result = Lang.eval expr
    let finish = System.DateTime.Now
    let duration = finish - start
    printfn $"[{duration.TotalMilliseconds}ms]>> {result}"

evalPrint (Lang.Ex.incrApp 42)
evalPrint (Lang.Ex.addApp 42 31)
evalPrint (Lang.Ex.subApp 42 31)
evalPrint (Lang.Ex.fib 5)
