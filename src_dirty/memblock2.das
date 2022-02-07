require macro_test

[dump_fields]
struct Memblock
    mem : array<uint8>
    a: int?
    aCount: int
    b: float?
    bCount: int

def initMemblock(var memblock: Memblock; aCount:int; bCount:int)

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
        var memblock: Memblock
        memblock |> initMemblock(5,5)
        print("Memblock = {memblock}")
		
		
//macro_test
module macro_test

require ast
require daslib/ast_boost

class DumpFields : AstStructureAnnotation
    def override finish ( var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        print("struct {st.name} \{\n")
        for field in st.fields
            print("\t{describe_cpp(field._type)} {field.name};\n")
        print("\}\n")
        return true

[macro]
def private init
    add_new_structure_annotation("dump_fields", new DumpFields())
