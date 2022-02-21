//calc first N primes, run-time version

require math

def calc_primes(n: int; var a:array<int>)
    a |> clear
    a |> reserve(n)

    a |> push(2)
    if n == 1
        return
    a |> push(3)

    let gen <- generator<int>() <| $()
        var t = 6
        while true
            yield t-1
            yield t+1
            t += 6
        return false

    var iterVal: int
    while length(a) < n
        next(gen, iterVal)
        var simple = true
        for prim in a
            if iterVal % prim == 0
                simple = false
                break
            if prim * prim > iterVal
                break
        if simple
            a |> push(iterVal)

[export]
def main
    var a: array<int>
    calc_primes(1000000, a)
    print("{a[length(a)-1]}\n")