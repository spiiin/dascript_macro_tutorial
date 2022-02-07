module macro_test

require ast
require daslib/ast_boost
require daslib/templates
require daslib/templates_boost

def foldR(var a:array<ExpressionPtr>; foldOp:function<(arg1, arg2: ExpressionPtr) : ExpressionPtr> )
    let aLen = a |> length
    var from := a[aLen-1]
    for i in range(1, aLen)
        from := foldOp |> invoke(a[aLen-1-i], from)
    return from

def foldR_partial(var a:array<ExpressionPtr>; foldOp:function<(arg1, arg2: ExpressionPtr) : ExpressionPtr>; initial: ExpressionPtr; endIndex:int)
    let aLen = a |> length
    var from := initial
    for i in range(endIndex, aLen)
        from := foldOp |> invoke(a[aLen-1-i], from)
    return from

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
        st.fields[fieldsLen] := *new [[ FieldDeclaration() name:="mem", _type <- uint8ArrayType]]
    else
        st.fields |> resize(fieldsLen+ ptrsOwnCount)

    for i in range(0, ptrsOwnCount)
        let fieldCountName = "{st.fields[ptrsOwnTypeIndexes[i]].name}{"`count"}"
        st.fields[fieldsLen+i+memFieldAddIndex] := *new [[ FieldDeclaration() name:=fieldCountName, _type <- new [[TypeDecl() baseType=Type tInt]]]]
    return <-ptrsTypeIndexes

def makeSumExpr(a, b: ExpressionPtr const): ExpressionPtr
    return new [[ExprOp2() op:="+", left := a, right := b]]

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
        fn.arguments |> emplace_new <| new [[Variable() at=st.at, name:= argumentName,  _type <- new [[TypeDecl() baseType=Type tInt]] ]]
    fn.result <- new [[TypeDecl() baseType=Type tVoid, at=st.at]]

    var blk <- new [[ExprBlock() at=st.at]]

    //-------------------------
    //1. Make Expressions
    //-------------------------
    //memblock.a`count = aCount
    //  ExprVar memblock. ExprField a`count ExprCopyRight = ExprVar aCount

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}`count"
        var exprVar_memblock <- new [[ExprVar() name:="memblock"]]
        var exprField_acount <- new [[ExprField() name:=argumentName, value <- exprVar_memblock]]
        var exprVar_acount <- new [[ExprVar() name:=argumentName]]
        var exprCopy <- new [[ExprCopy() op:="=", left <- exprField_acount, right <- exprVar_acount]]
        blk.list |> emplace(exprCopy)

    //-------------------------
    //2. Quotes and templates
    //-------------------------
    //let aSize = typeinfo(sizeof *memblock.a) * aCount
    //  ExprLet var  ExprLetVariable aSize : int const =  ExprOp2(ExprVar aCount ExprOp2Right * 4)

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        var exprLet_aSize_value <- quote(
            typeinfo(sizeof *ELEMENT) * ELEMENTS_COUNT
        )

        var exprVar_memblock2 <- new [[ExprVar() name:="memblock"]]
        var exprField_acount2 <- new [[ExprField() name:=argumentName, value <- exprVar_memblock2]]

        var exprLet_rules : Template
        exprLet_rules |> replaceVariable("ELEMENT", exprField_acount2)
        exprLet_rules |> renameVariable("ELEMENTS_COUNT", "{argumentName}`count")
        apply_template(exprLet_rules, exprLet_aSize_value.at, exprLet_aSize_value)
        
        var exprLet_aSize <- new [[ExprLet()]]
        exprLet_aSize.variables |> emplace_new() <| new [[Variable()
            name := "{argumentName}Size",
            _type <- new [[TypeDecl() baseType=Type tInt]],
            init <- exprLet_aSize_value
        ]]
        blk.list |> emplace(exprLet_aSize)
    
    //-------------------------
    // 3. Foldr
    //-------------------------
    //memblock.mem |> resize(aSize + bSize + cSize)
    //     ExprCall __::builtin`resize( ExprCallArgument ExprVar memblock. ExprField mem, ExprCallArgument  ExprOp2( ExprOp2(ExprVar aSize ExprOp2Right + ExprVar bSize) ExprOp2Right + ExprVar cSize))

    var sumArgumentsArray: array<ExpressionPtr>
    for i in range(0, ptrFieldsLen) 
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        let nameSize := "{argumentName}Size"
        sumArgumentsArray |> emplace(new [[ExprVar() name:=nameSize]])
    var sumExpr <- foldR(sumArgumentsArray, @@makeSumExpr)

    //--------
    var exprLet_totalSize <- new [[ExprLet()]]
    exprLet_totalSize.variables |> emplace_new() <| new [[Variable()
        name := "totalSize",
        _type <- new [[TypeDecl() baseType=Type tInt]],
        init <- sumExpr
    ]]
    blk.list |> emplace(exprLet_totalSize)

    var exprCall_resize <- quote(
        memblock.mem |> resize(totalSize)
    )

    blk.list |> emplace(exprCall_resize)

    //-------------------------
    // 4. castType
    //-------------------------
    //memblock.a = reinterpret<int?> addr(memblock.mem[0])

    for i in range(0, ptrFieldsLen)
        let argumentName = "{st.fields[ptrsTypeIndexes[i]].name}"
        var exprVar_memblock <- new [[ExprVar() name:="memblock"]]
        var exprField_a <- new [[ExprField() name:=argumentName, value <- exprVar_memblock]]
        var exprAddr <- quote(
            addr(memblock.mem[START_ADDRESS])
        )

        var exprZero <- new [[ExprConstInt() value=0]]
        var exprStartAddress <- foldR_partial(sumArgumentsArray, @@makeSumExpr, exprZero, ptrFieldsLen-i)
        var exprAddr_rules : Template
        exprAddr_rules |> replaceVariable("START_ADDRESS", exprStartAddress)
        apply_template(exprAddr_rules, exprAddr.at, exprAddr)

        var exprReinterpretCast<- new [[ExprCast() 
            castType := st.fields[ptrsTypeIndexes[i]]._type,
            castFlags = ExprCastFlags reinterpretCast,
            subexpr <- exprAddr]]
        
        var exprUnsafe<- new [[ExprUnsafe() body <- exprReinterpretCast]]
        var exprCopy <- new [[ExprCopy() op:="=", left <- exprField_a, right <- exprUnsafe]]
        blk.list |> emplace(exprCopy)

    //-------------------------
    fn.body <- blk
    compiling_module() |> add_function(fn)

[structure_macro(name=memblock)]
class GenMemblock : AstStructureAnnotation
    def override apply (var st:StructurePtr; var group:ModuleGroup; args:AnnotationArgumentList; var errors : das_string ) : bool
        var ptrsTypeIndexes <- generateStructureFields(st)
        generateStructureInitFunction(st, ptrsTypeIndexes)
        return true