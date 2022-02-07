struct Memblock
    mem : array<int>
    a: int?
    aCount: int
    b: int?
    bCount: int

def initMemblock(var memblock: Memblock; aCount:int; bCount:int)
    var arr: array<int>
    arr |> resize(aCount + bCount)
    memblock.mem <- arr

    memblock.aCount = aCount
    memblock.bCount = bCount
    unsafe
        memblock.a = addr(memblock.mem[0])
        memblock.b = addr(memblock.mem[bCount])

[export]
def main
    unsafe
        var memblock: Memblock
        memblock |> initMemblock(5,5)
        print("Memblock = {memblock}")
