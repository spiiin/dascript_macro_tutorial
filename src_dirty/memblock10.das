require macro_test
require daslib/ast_boost
require daslib/templates
require macro_print_ast
require ast_print

[memblock, dump_fields]
struct Memblock
    a: int?
    b: float?
    c: int?

def initMemblock(var memblock: Memblock; aCount:int; bCount:int; cCount:int)
    memblock.a`count = aCount
    memblock.b`count = bCount
    memblock.c`count = cCount

    let aSize = typeinfo(sizeof *memblock.a) * aCount
    let bSize = typeinfo(sizeof *memblock.b) * bCount
    let cSize = typeinfo(sizeof *memblock.c) * cCount

    memblock.mem |> resize(aSize + bSize + cSize)

    unsafe
        memblock.a = reinterpret<int?> addr(memblock.mem[0])
        memblock.b = reinterpret<float?> addr(memblock.mem[aSize])
        memblock.c = reinterpret<int?> addr(memblock.mem[aSize+bSize])
    //memblock |> init`struct`Memblock(5,5,5)

def fillMemblock(var memblock: Memblock)
    unsafe
        memblock.a[0] = int(0x11223344)
        memblock.a[1] = 0
        memblock.a[2] = -1

        memblock.b[0] = reinterpret<float>(0x55667788)
        memblock.b[1] = 0.0
        memblock.b[2] = 123.456

        memblock.c[0] = int(0xAABBCCDD)
        memblock.c[1] = 0
        memblock.c[2] = -1

/*[export]
def test
    unsafe
        var memblock: Memblock
        memblock |> initMemblock(5,5,5)
        memblock |> fillMemblock
        print("Memblock = {memblock}")


    let fd <- typeinfo(ast_function @@initMemblock)
    print("fd = {describe(fd)}\n")
    print("fd = {fd}\n")*/

[export]
def test
    let fd_gen <- typeinfo(ast_function @@init`struct`Memblock)
    var fd_das <- typeinfo(ast_function @@initMemblock)
    var astVisitor = new PrintVisitor()
    var astVisitorAdapter <- make_visitor(*astVisitor)

    var astVisitor2 = new PrintVisitorSource()
    var astVisitorAdapter2 <- make_visitor(*astVisitor2)
    print("---das------------------\n")
    visit(fd_das, astVisitorAdapter)
    print("---gen-------------------\n")
    visit(fd_gen, astVisitorAdapter)
    print("---gen_text--------------\n")
    visit(fd_gen, astVisitorAdapter2)


		
		
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

def generateStructureFields(var st:StructurePtr)
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
    return <-ptrsTypeIndexes

def generateStructureInitFunction(var st:StructurePtr; ptrsTypeIndexes:array<int>&)
    let ptrFieldsLen = ptrsTypeIndexes |> length
    for i in range(0, ptrFieldsLen)
        //let ptrField :FieldDeclaration& = st.fields[ptrsTypeIndexes[i]]
        print("Field name: {st.fields[ptrsTypeIndexes[i]].name}. Type: {describe(st.fields[ptrsTypeIndexes[i]]._type)}\n")
        

    let fnname = "init`struct`{st.name}"
    var fn <- new [[Function() at=st.at, atDecl=st.at, name:=fnname]]
    fn.flags |= FunctionFlags generated
    fn.flags |= FunctionFlags privateFunction

    //function signature
    unsafe
        fn.arguments |> emplace_new <| new [[Variable() at=st.at, name:= "memblock", _type <- new [[TypeDecl() baseType=Type tStructure, structType=addr(*st)]]]]
    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}`count"
        fn.arguments |> emplace_new <| new [[Variable() at=st.at, name:= argumentName,  _type <- new [[TypeDecl() baseType=Type tInt]] ]]
    fn.result <- new [[TypeDecl() baseType=Type tVoid, at=st.at]]

    var blk <- new [[ExprBlock() at=st.at]]

    var printF <- new [[ExprCall() name:="print"]]
    printF.arguments |> emplace_new <| new [[ExprConstString() value:="Hello, world!\n"]]
    blk.list |> emplace(printF)

    //-------------------------
    //1. Make Expressions
    //-------------------------
    //memblock.a`count = aCount
    //  ExprVar memblock. ExprField a`count ExprCopyRight = ExprVar aCount
    var exprVar_memblock <- new [[ExprVar() name:="memblock"]]
    var exprField_acount <- new [[ExprField() name:="a`count", value <- exprVar_memblock]]
    var exprVar_acount <- new [[ExprVar() name:="a`count"]]
    var exprCopy <- new [[ExprCopy() op:="=", left <- exprField_acount, right <- exprVar_acount]]
    blk.list |> emplace(exprCopy)

    //-------------------------
    //2. Quotes
    //-------------------------
    //let aSize = typeinfo(sizeof *memblock.a) * aCount
    //  ExprLet var  ExprLetVariable aSize : int const =  ExprOp2(ExprVar aCount ExprOp2Right * 4)
    var exprLet_aSize_value <- quote(
        typeinfo(sizeof * memblock.a) * a`count
    )
    
    var exprLet_aSize <- new [[ExprLet()]]
    exprLet_aSize.variables |> emplace_new() <| new [[Variable()
        name := "aSize",
        _type <- new [[TypeDecl() baseType=Type tInt]],
        init <- exprLet_aSize_value
    ]]
    blk.list |> emplace(exprLet_aSize)

    //prevent drop unused code
    var exprCall_printUnused <- quote(
        print("{aSize}")
    )
    blk.list |> emplace(exprCall_printUnused)
    

    fn.body <- blk
    compiling_module() |> add_function(fn)

[structure_macro(name=memblock)]
class GenMemblock : AstStructureAnnotation
    def override apply (var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        var ptrsTypeIndexes <- generateStructureFields(st)
        generateStructureInitFunction(st, ptrsTypeIndexes)
        return true