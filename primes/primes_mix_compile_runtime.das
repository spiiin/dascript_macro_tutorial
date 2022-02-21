//calc first N primes, mix of compile-time + run-time version

require math
require daslib/ast_boost
require primes_cache

[cached_primes (count=200)]
def calc_primes(n: int; var a:array<int>)
    a |> clear
    a |> reserve(n)

    let cachedCopyCount = min(n, length(calc_primes`compileTimeResult)-1)
    var firstPrimeCandidat : int

    if cachedCopyCount > -1
        //run-time
        for i in range(0,cachedCopyCount)  //memcpy?
            a |> push(calc_primes`compileTimeResult[i])
        firstPrimeCandidat = (calc_primes`compileTimeResult[cachedCopyCount]+1) / 6 * 6   //check if it correct
    else
        //compile-time
        a |> push(2)
        a |> push(3)
        firstPrimeCandidat = 6

    //wheel: check only 6t+/-1
    let gen <- generator<int>() <| $()
        var t = firstPrimeCandidat
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
def test
    var a: array<int>
    calc_primes(1000000, a)

    //for el in a
    //    print("{el} ")
    
    print("\n-------------------\n")
    print("{a[length(a)-1]}\n")