require macro_test

[memblock]
struct Memblock
    mem : array<uint8>
    a: int?
    aCount: int
    b: float?
    bCount: int

def initMemblock(var memblock: MemblockTest; aCount:int; bCount:int)

    let aSize = typeinfo(sizeof *memblock.a) * aCount
    let bSize = typeinfo(sizeof *memblock.b) * bCount

    var arr: array<uint8>
    arr |> resize(aSize + bSize )
    memblock.mem <- arr

    memblock.aCount = aCount
    memblock.bCount = bCount
    unsafe
        memblock.a = reinterpret<int?> addr(memblock.mem[0])
        memblock.b = reinterpret<float?> addr(memblock.mem[aSize])

        memblock.a[0] = int(0x11223344)
        memblock.a[1] = 0
        memblock.a[2] = -1

        memblock.b[0] = reinterpret<float>(0x55667788)
        memblock.b[1] = 0.0
        memblock.b[2] = 123.456

[export]
def test
    unsafe
        var memblock: MemblockTest
        memblock |> initMemblock(5,5)
        print("MemblockTest = {memblock}")
		
		
/////
module macro_test

require ast
require daslib/ast_boost

[structure_macro(name=memblock)]
class GenMemblock : AstStructureAnnotation
    def override apply ( var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        var mkS <- new [[ Structure() ]]
        mkS.name := "MemblockTest"
        let fieldsLen = st.fields |> length
        mkS.fields |> resize(fieldsLen)
        for i in range(0, fieldsLen)
            mkS.fields[i] := st.fields[i]
        compiling_module() |> add_structure(mkS)
        return true