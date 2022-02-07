// options log=true,log_infer_passes=false,optimize=false

def foldR(a:array<auto(T)>; foldOp:function < (arg1:T; arg2:T) : T> )
    let aLen = a |> length
    var from = a[aLen-1]
    for i in range(1, aLen)
        from = foldOp |> invoke(a[aLen-1-i], from)
    return from

def minus(a,b:float): float
    return a-b

def sumHalf(a,b:float):float
    return (a+b)/2.0

def makePlusString(a,b:string):string
    return "{a}+{b}"

[export]
def test
    var a:array<float>
    push(a,10.0)
    push(a,11.0)
    push(a,54.0)
    let f = foldR(a, @@minus)
    print("{f}\n") //53.0
    
    var b:array<float>
    push(b,12.0)
    push(b,4.0)
    push(b,10.0)
    push(b,6.0)
    push(b,54.0)
    let fb = foldR(b, @@sumHalf)
    print("{fb}\n") //12.0
    
    var c:array<string>
    push(c,"aCount")
    push(c,"bCount")
    push(c,"cCount")
    let fc = foldR(c, @@makePlusString)
    print("{fc}\n") //aCount+bCount+cCount