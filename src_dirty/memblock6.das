require macro_test

[memblock, dump_fields]
struct Memblock
    a: int?
    b: float?

def initMemblock(var memblock: Memblock; aCount:int; bCount:int)

    let aSize = typeinfo(sizeof *memblock.a) * aCount
    let bSize = typeinfo(sizeof *memblock.b) * bCount

    memblock.mem |> resize(aSize + bSize )

    memblock.a`count = aCount
    memblock.b`count = bCount
    unsafe
        memblock.a = reinterpret<int?> addr(memblock.mem[0])
        memblock.b = reinterpret<float?> addr(memblock.mem[aSize])

def fillMemblock(var memblock: Memblock)
    unsafe
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
		
		
/////
module macro_test

require ast
require daslib/ast_boost

[structure_macro(name=dump_fields)]
class DumpFields : AstStructureAnnotation
    def override finish ( var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        print("struct {st.name} \{\n")
        for field in st.fields
            print("\t{describe_cpp(field._type)} {field.name};\n")
        print("\}\n")
        return true

[structure_macro(name=memblock)]
class GenMemblock : AstStructureAnnotation
    def override apply ( var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        //create "mem" field of type array<uint8>
        var uint8Type <- new [[TypeDecl() baseType=Type tUInt8]]
        var uint8ArrayType <- new [[TypeDecl() baseType=Type tArray, firstType <- uint8Type]]

        let fieldsLen = st.fields |> length
        var ptrsCount = 0
        var ptrsTypeIndexes : array<int>
        ptrsTypeIndexes |> reserve(fieldsLen)
        for i in range(0, fieldsLen)
            if st.fields[i]._type.baseType == Type tPointer
                ptrsCount++
                ptrsTypeIndexes |> push(i)

        //add one field for memblock and one field per pointer type to handle count of object per type
        st.fields |> resize(fieldsLen+ ptrsCount + 1)
        st.fields[fieldsLen] := *new [[ FieldDeclaration() name:="mem", _type <- uint8ArrayType]]

        for i in range(0, ptrsCount)
            let fieldCountName = "{st.fields[ptrsTypeIndexes[i]].name}{"`count"}"
            st.fields[fieldsLen+1+i] := *new [[ FieldDeclaration() name:=fieldCountName, _type <- new [[TypeDecl() baseType=Type tInt]]]]

        return true