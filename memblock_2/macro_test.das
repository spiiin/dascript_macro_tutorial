module macro_test

require ast
require daslib/ast_boost
require daslib/templates
require daslib/templates_boost
require daslib/functional
require reduce_while

def makeSumExpr(a, b: ExpressionPtr const): ExpressionPtr const
    return new [[ExprOp2() op:="+", left := a, right := b]]

[structure_macro(name=dump_fields)]
class DumpFields : AstStructureAnnotation
    def override finish ( var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        print("struct {st.name} \{\n")
        for field in st.fields
            print("\t{describe_cpp(field._type)} {field.name};\n")
        print("\}\n")
        return true

let intAstType <- typeinfo(ast_typedecl type<int>)

def generateStructureFields(var st:StructurePtr)
    let fieldsLen = st.fields |> length
    var ptrsCount = 0
    var ptrsOwnCount = 0
    var ptrsTypeIndexes : array<int>
    var ptrsOwnTypeIndexes : array<int>
    ptrsTypeIndexes |> reserve(fieldsLen)
    ptrsOwnTypeIndexes |> reserve(fieldsLen)

    for i in range(0, fieldsLen)
        if st.fields[i]._type.baseType == Type tPointer
            var memblockAnnExist = find_arg("in_memblock", st.fields[i].annotation) ?as tBool ?? false
            if !memblockAnnExist
                ptrsOwnCount++
                ptrsOwnTypeIndexes |> push(i)
                st.fields[i].annotation |> add_annotation_argument("in_memblock", true)
            ptrsCount++
            ptrsTypeIndexes |> push(i)

    //if first memblock in inheritance hierarchy
    let hasMemField = ptrsOwnCount != ptrsCount
    let memFieldAddIndex = hasMemField ? 0 : 1
    if !hasMemField
        //add one field for memblock and one field per pointer type to handle count of object per type
        st.fields |> resize(fieldsLen+ ptrsOwnCount + 1)

        //create "mem" field of type array<uint8>
        st.fields[fieldsLen] := *new [[ FieldDeclaration() name:="mem", _type <- typeinfo(ast_typedecl type<array<uint8>>)]]
    else
        st.fields |> resize(fieldsLen+ ptrsOwnCount)

    for i in range(0, ptrsOwnCount)
        let fieldCountName = "{st.fields[ptrsOwnTypeIndexes[i]].name}{"`count"}"
        st.fields[fieldsLen+i+memFieldAddIndex] := *new [[ FieldDeclaration() name:=fieldCountName, _type := intAstType]]
    return <-ptrsTypeIndexes

def generateStructureInitFunction(var st:StructurePtr; ptrsTypeIndexes:array<int>&)
    let ptrFieldsLen = ptrsTypeIndexes |> length

    let fnname = "init`struct`{st.name}"
    var fn <- new [[Function() at=st.at, atDecl=st.at, name:=fnname]]
    fn.flags |= FunctionFlags generated
    fn.flags |= FunctionFlags privateFunction

    //function signature
    unsafe
        fn.arguments |> emplace_new <| new [[Variable() at=st.at, name:= "memblock", _type <- new [[TypeDecl() baseType=Type tStructure, structType=addr(*st)]]]]
    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}`count"
        fn.arguments |> emplace_new <| new [[Variable() at=st.at, name:= argumentName,  _type := intAstType]]
    fn.result := typeinfo(ast_typedecl type<void>)

    var blk <- new [[ExprBlock() at=st.at]]

    //-------------------------
    //memblock.a`count = aCount

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}`count"
        blk.list |> emplace_new <| qmacro_expr(
            ${memblock.$f(argumentName) = $i(argumentName);}
        )

    //-------------------------
    //let aSize = typeinfo(sizeof *memblock.a) * aCount

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        let argumentNameSize = "{argumentName}Size"
        let argumentNameCount = "{argumentName}`count"
        blk.list |> emplace_new <| qmacro_expr(
            ${let $i(argumentNameSize) = typeinfo(sizeof *memblock.$f(argumentName)) * $i(argumentNameCount);}
        )
    
    //-------------------------
    //memblock.mem |> resize(aSize + bSize + cSize)

    var sumArgumentsArray: array<ExpressionPtr>
    sumArgumentsArray |> emplace(new [[ExprConstInt() value=0]])
    for i in range(0, ptrFieldsLen) 
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        let nameSize := "{argumentName}Size"
        sumArgumentsArray |> emplace <| qmacro_expr(${$i(nameSize);})
    unsafe
        var sumExpr <- reduce(each(sumArgumentsArray), @@makeSumExpr)
        blk.list |> emplace_new <| qmacro_expr(
            ${memblock.mem |> resize($e(sumExpr));}
        )

    //-------------------------
    //memblock.a = reinterpret<int?> addr(memblock.mem[0])

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        unsafe
            var exprStartAddress <- reduce_while(each(sumArgumentsArray), @@makeSumExpr,  @(e:ExpressionPtr; counter:int):bool => counter <= i)
            var subtype := st.fields[ptrsTypeIndexes[i]]._type
            var exprAssign <- qmacro_expr <|
                unsafe{ memblock.$f(argumentName) = reinterpret<$t(subtype)> addr(memblock.mem[$e(exprStartAddress)]); }
            blk.list |> emplace_new(exprAssign)

    let variableName = "a"
    let op1 = 40
    let op2 = 2
    var exprLet <-qmacro <|
        let $i(variableName) = $v(op1) + $v(op2)
    print(describe(exprLet))
    //-------------------------
    fn.body <- blk
    compiling_module() |> add_function(fn)

[structure_macro(name=memblock)]
class GenMemblock : AstStructureAnnotation
    def override apply (var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        var ptrsTypeIndexes <- generateStructureFields(st)
        generateStructureInitFunction(st, ptrsTypeIndexes)
        return true