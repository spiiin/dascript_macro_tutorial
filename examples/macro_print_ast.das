module macro_print_ast

require ast
require rtti
require daslib/ast_boost
require daslib/strings_boost

let
    function_annotation_flags = (
        (FunctionFlags fastCall) |
        (FunctionFlags exports) |
        (FunctionFlags privateFunction) |
        (FunctionFlags unsafeDeref) |
        (FunctionFlags unsafeOperation) |
        (FunctionFlags _generator) )

// TODO: fixme
def noBracket(expr)
    return false

class PrintVisitor : AstVisitor
    extraTypeInfo : bool = true
    printCStyle : bool = false
    tab : int = 0
    // TODO: fixme
    def newLine()
        print("\n")
// program
    def override preVisitProgram(prog:ProgramPtr)
        print("// program\n")
    def override visitProgram(prog:ProgramPtr)
        print("// end program\n")
    def override preVisitProgramBody(prog:ProgramPtr; mod:Module?)
        print("// program body\n")
// type
    ET : bool = false
    def override preVisitTypeDecl(typ:TypeDeclPtr)
        ET = typ.isExprType
        if ET
            print("/*[")
    def override visitTypeDecl(typ:TypeDeclPtr)
        if ET
            print("]*/")
        return typ
// alias
    def override preVisitAlias(typ:TypeDeclPtr;name:das_string)
        print("typedef\n\t{name} = {describe([decl=typ,extra=extraTypeInfo])}\n\n")
// enumeration
    def override preVisitEnumeration(enu:EnumerationPtr)
        print("enum {enu.name} : {das_to_string(enu.baseType)}\n")
    def override preVisitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool)
        print("\t{name} =")
    def override visitEnumerationValue(enu:EnumerationPtr;name:das_string;value:ExpressionPtr;last:bool)
        print("\n")
        return value
    def override visitEnumeration(enu:EnumerationPtr)
        print("\n")
        return enu
// structure
    def override preVisitStructure(str:StructurePtr)
        if length(str.annotations) != 0
            print("[{describe(str.annotations)}]\n")
        print("{str.flags.isClass ? "class" : "struct"} {str.name}\n")
    def override preVisitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool)
        print("\t")
        if length(decl.annotation) != 0
            print("[{describe(decl.annotation)}] ")
        print("{decl.name} : {describe([decl=decl._type,extra=extraTypeInfo])}")
        if decl.flags.parentType
            print(" /* from {str.parent.name} */")
        if decl.init != null
            print("{decl.flags.moveSemantics ? " <- " : " = "}")
    def override visitStructureField(str:StructurePtr;decl:FieldDeclaration;last:bool)
        print("\n")
    def override visitStructure(str:StructurePtr) : StructurePtr
        print("\n")
        return str
// function
    def override preVisitFunction(fun:FunctionPtr)
        if fun.flags.knownSideEffects
            if fun.sideEffectFlags == bitfield(0)
                print("// [nosideeffects]\n")
            else
                print("// {describe_bitfield(fun.sideEffectFlags)}\n")
        let bfa : FunctionFlags = fun.flags & function_annotation_flags
        print("{describe_bitfield(bfa,"\n")}")
        if length(fun.annotations) != 0
            print("[{describe(fun.annotations)}]\n")
        print("def {fun.name}")
        if length(fun.arguments) != 0
            print(" ( ")
    def override preVisitFunctionBody(fun:FunctionPtr; expr:ExpressionPtr)
        if length(fun.arguments) != 0
            print(" ) ")
        if fun.result != null && !fun.result.isVoid
            print(" : {describe([decl=fun.result,extra=extraTypeInfo])}")
        print("\n")
    def override visitFunction(fun:FunctionPtr) : FunctionPtr
        print("\n")
        return fun
    def override preVisitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;last:bool)
        if length(arg.annotation) != 0
            print("[{describe(arg.annotation)}] ")
        if !arg._type.flags.constant
            print("var ")
        if arg.isAccessUnused
            print(" /*unsued*/ ")
        print("{arg.name} : {describe([decl=arg._type,extra=extraTypeInfo])}")
    def override visitFunctionArgument(fun:FunctionPtr;arg:VariablePtr;last:bool)
        if !last
            print("; ")
        return arg
    def override preVisitFunctionArgumentInit(fun:FunctionPtr;arg:VariablePtr;value:ExpressionPtr)
        print(" = ")
// block
    def override preVisitExprBlock(blk:smart_ptr<ExprBlock>)
        if blk.blockFlags.isClosure
            if blk.returnType!=null || length(blk.arguments)!=0
                print("ExprBlock $(")
                for arg,argIndex in blk.arguments,range(length(blk.arguments))
                    if length(arg.annotation) != 0
                        print("[{describe(arg.annotation)}] ")
                    if arg._type.flags.constant
                        print("var ")
                    print("{arg.name} : {describe([decl=arg._type,extra=extraTypeInfo])}")
                    if argIndex != length(blk.arguments)-1
                        print("; ")
                print(")")
                if blk.returnType!=null
                    print(":{describe([decl=blk.returnType,extra=extraTypeInfo])}")
                print("\n")
        if printCStyle || blk.blockFlags.isClosure
            print("{repeat("\t",tab)}\{\n")
        tab ++
    def override visitExprBlock(blk:smart_ptr<ExprBlock>) : ExpressionPtr
        tab --
        if printCStyle || blk.blockFlags.isClosure
            print("{repeat("\t",tab)}\}\n")
        return blk
    def override preVisitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr)
        print("{repeat("\t",tab)}")
    def override visitExprBlockExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr)
        if printCStyle || blk.blockFlags.isClosure
            print(";")
        self->newLine()
        return expr
    def override visitExprBlockFinal(blk:smart_ptr<ExprBlock>)
        if printCStyle || blk.blockFlags.isClosure
            print("{repeat("\t",tab-1)}\} finally \{\n")
        else
            print("{repeat("\t",tab-1)}finally\n")
    def override preVisitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr)
        print("{repeat("\t",tab)}")
    def override visitExprBlockFinalExpression(blk:smart_ptr<ExprBlock>;expr:ExpressionPtr)
        if printCStyle || blk.blockFlags.isClosure
            print(";")
        self->newLine()
        return expr
// let
    def override preVisitExprLet(expr:smart_ptr<ExprLet>)
        var isLet = true
        for pv in expr.variables
            if pv._type!=null && pv._type.flags.constant
                isLet = false
                break
        print("ExprLet {isLet ? "let" : "var"} ")
    def override preVisitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool)
        if arg.isAccessUnused
            print(" /*unused*/ ")
        print("ExprLetVariable {arg.name} : {describe([decl=arg._type,extra=extraTypeInfo])}")
    def override visitExprLetVariable(expr:smart_ptr<ExprLet>;arg:VariablePtr;lastArg:bool)
        if !lastArg
            print(";")
        return arg
    def override preVisitExprLetVariableInit(blk:smart_ptr<ExprLet>;arg:VariablePtr;expr:ExpressionPtr)
        if arg.flags.init_via_move
            print(" <- ")
        elif arg.flags.init_via_clone
            print(" := ")
        else
            print(" = ")
// global let
    def override preVisitGlobalLetVariable(arg:VariablePtr;lastArg:bool)
        print("{arg._type.flags.constant ? "let" : "var"}{arg.flags.global_shared ? " shared" : ""}\n\t")
        if arg.isAccessUnused
            print(" /*unused*/ ")
        print("{arg.name} : {describe([decl=arg._type,extra=extraTypeInfo])}")
    def override visitGlobalLetVariable(arg:VariablePtr;lastArg:bool)
        print("\n\n")
        return arg
    def override preVisitGlobalLetVariableInit(arg:VariablePtr;expr:ExpressionPtr)
        if arg.flags.init_via_move
            print(" <- ")
        elif arg.flags.init_via_clone
            print(" := ")
        else
            print(" = ")
// string builder
    def override preVisitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>)
        print("ExprStringBuilder string_builder(")
    def override visitExprStringBuilder(expr:smart_ptr<ExprStringBuilder>) : ExpressionPtr
        print(")")
        return expr
    def override visitExprStringBuilderElement(expr:smart_ptr<ExprStringBuilder>;elem:ExpressionPtr;last:bool)
        if !last
            print(",")
        return elem
// new
    def override preVisitExprNew(expr:smart_ptr<ExprNew>)
        print("ExprNew new {describe(expr.typeexpr)}")
        if expr.initializer
            print("(")
    def override visitExprNew(expr:smart_ptr<ExprNew>) : ExpressionPtr
        if expr.initializer
            print(")")
        return expr
    def override visitExprNewArgument(expr:smart_ptr<ExprNew>;arg:ExpressionPtr;last:bool)
        if !last
            print(",")
        return arg
// named call
    def override preVisitExprNamedCall(expr:smart_ptr<ExprNamedCall>)
        print("ExprNamedCall {expr.name}([")
    def override visitExprNamedCall(expr:smart_ptr<ExprNamedCall>) : ExpressionPtr
        print("])")
        return expr
    def override preVisitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool)
        print("{arg.name}{arg.flags.moveSemantics ? "<-" : "="}")
    def override visitExprNamedCallArgument(expr:smart_ptr<ExprNamedCall>;arg:MakeFieldDeclPtr;last:bool)
        if !last
            print(",")
        return arg
// looks like call (debug,assert,verify,erase,find,key_exists,keys,values,invoke,memzero etc)
    def override preVisitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>)
        print("ExprLooksLikeCall {expr.name}(")
    def override visitExprLooksLikeCall(expr:smart_ptr<ExprLooksLikeCall>) : ExpressionPtr
        print(")")
        return expr
    def override visitExprLooksLikeCallArgument(expr:smart_ptr<ExprLooksLikeCall>;arg:ExpressionPtr;last:bool)
        if !last
            print(",")
        return arg
// call
    def override preVisitExprCall(expr:smart_ptr<ExprCall>)
        print("ExprCall {expr.name}(")
    def override visitExprCall(expr:smart_ptr<ExprCall>) : ExpressionPtr
        print(")")
        return expr
    def override preVisitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool)
        print(" ExprCallArgument ")
    def override visitExprCallArgument(expr:smart_ptr<ExprCall>;arg:ExpressionPtr;last:bool)
        if !last
            print(",")
        return arg
// null coaelescing
    def override preVisitExprNullCoalescingDefault(expr:smart_ptr<ExprNullCoalescing>;defval:ExpressionPtr)
        print(" ExprNullCoalescingDefault ?? ")
// at
    def override visitExprAt(expr:smart_ptr<ExprAt>) : ExpressionPtr
        print("]")
        return expr
    def override preVisitExprAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr)
        print(" ExprAtIndex[")
// safe at
    def override visitExprSafeAt(expr:smart_ptr<ExprSafeAt>) : ExpressionPtr
        print("]")
        return expr
    def override preVisitExprSafeAtIndex(expr:smart_ptr<ExprAt>;index:ExpressionPtr)
        print(" ExprSafeAtIndex?[")
// is
    def override preVisitExprIsType(expr:smart_ptr<ExprIs>;typeDecl:TypeDeclPtr): void
        print(" ExprIsType is type<{describe([decl=typeDecl,extra=false,contracts=true])}>")
// op2
    def override preVisitExprOp2(expr:smart_ptr<ExprOp2>)
        if !noBracket(expr)
            print(" ExprOp2(")
    def override visitExprOp2(expr:smart_ptr<ExprOp2>) : ExpressionPtr
        if !noBracket(expr)
            print(")")
        return expr
    def override preVisitExprOp2Right(expr:smart_ptr<ExprOp2>;right:ExpressionPtr)
        print(" ExprOp2Right {expr.op} ")
// op3
    def override preVisitExprOp3(expr:smart_ptr<ExprOp3>)
        if !noBracket(expr)
            print("VisitExprOp3(")
    def override visitExprOp3(expr:smart_ptr<ExprOp3>) : ExpressionPtr
        if !noBracket(expr)
            print(")")
        return expr
    def override preVisitExprOp3Left(expr:smart_ptr<ExprOp3>;left:ExpressionPtr)
        print(" ExprOp3Left ? ")
    def override preVisitExprOp3Right(expr:smart_ptr<ExprOp3>;right:ExpressionPtr)
        print("ExprOp3Right : ")
// copy
    def override preVisitExprCopyRight(expr:smart_ptr<ExprCopy>;right:ExpressionPtr)
        print(" ExprCopyRight = ")
// move
    def override preVisitExprMoveRight(expr:smart_ptr<ExprMove>;right:ExpressionPtr)
        print(" ExprMoveRight <- ")
// clone
    def override preVisitExprCloneRight(expr:smart_ptr<ExprClone>;right:ExpressionPtr)
        print(" ExprCloneRight := ")
// with
    def override preVisitExprWith(expr:smart_ptr<ExprWith>)
        print(" ExprWith with ")
    def override preVisitExprWithBody(expr:smart_ptr<ExprWith>;right:ExpressionPtr)
        print("\n")
// while
    def override preVisitExprWhile(expr:smart_ptr<ExprWhile>)
        print(" ExprWhile while ")
    def override preVisitExprWhileBody(expr:smart_ptr<ExprWhile>;right:ExpressionPtr)
        print("\n")
// try-catch
    def override preVisitExprTryCatch(expr:smart_ptr<ExprTryCatch>)
        print("try\n")
    def override preVisitExprTryCatchCatch(expr:smart_ptr<ExprTryCatch>;right:ExpressionPtr)
        print("{repeat("\t",tab)}recover\n")
// if-then-else
    def override preVisitExprIfThenElse(expr:smart_ptr<ExprIfThenElse>)
        print("if ")
    def override preVisitExprIfThenElseIfBlock(expr:smart_ptr<ExprIfThenElse>;ifBlock:ExpressionPtr)
        print("\n")
    def override preVisitExprIfThenElseElseBlock(expr:smart_ptr<ExprIfThenElse>;elseBlock:ExpressionPtr)
        print("{repeat("\t",tab)}")
        if elseBlock.__rtti=="ExprIfThenElse"
            print("else ")
        else
            print("else\n")
// for
    def override preVisitExprFor(expr:smart_ptr<ExprFor>)
        print(" ExprFor for ")
    def override preVisitExprForVariable(expr:smart_ptr<ExprFor>;svar:VariablePtr;last:bool)
        print("{svar.name}{last ? " in " : ","}")
    def override visitExprForSource(expr:smart_ptr<ExprFor>;source:ExpressionPtr;last:bool) : ExpressionPtr
        if !last
            print(",")
        return source
    def override preVisitExprForBody(expr:smart_ptr<ExprFor>)
        print("\n")
// make variant
    def override preVisitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>)
        print("ExprMakeVariant [[")
        if expr._type != null
            print(" ExprMakeVariant {describe(expr._type)} ")
    def override visitExprMakeVariant(expr:smart_ptr<ExprMakeVariant>) : ExpressionPtr
        print("]]")
        return expr
    def override preVisitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool)
        print(" ExprMakeVariantField {decl.name} {decl.flags.moveSemantics ? "<-" : "=" } ")
    def override visitExprMakeVariantField(expr:smart_ptr<ExprMakeVariant>;index:int;decl:MakeFieldDeclPtr;last:bool)
        if !last
            print("; ")
        return decl
// make structure
    def override preVisitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>)
        print("ExprMakeStruct [[")
        if expr._type != null
            print("{describe(expr._type)}{expr.makeStructFlags.useInitializer ? "()" : ""} ")
    def override visitExprMakeStruct(expr:smart_ptr<ExprMakeStruct>) : ExpressionPtr
        print("]]")
        return expr
    def override visitExprMakeStructIndex(expr:smart_ptr<ExprMakeStruct>;index:int;last:bool)
        if !last
            print("; ")
    def override preVisitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool)
        print("ExprMakeStructField {decl.name} {decl.flags.moveSemantics ? "<-" : "=" } ")
    def override visitExprMakeStructField(expr:smart_ptr<ExprMakeStruct>;index:int;decl:MakeFieldDeclPtr;last:bool)
        if !last
            print(", ")
        return decl
// make array
    def override preVisitExprMakeArray(expr:smart_ptr<ExprMakeArray>)
        print("ExprMakeArray [[")
        if expr._type != null
            print("{describe(expr._type)} ")
    def override visitExprMakeArray(expr:smart_ptr<ExprMakeArray>) : ExpressionPtr
        print("]]")
        return expr
    def override visitExprMakeArrayIndex(expr:smart_ptr<ExprMakeArray>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
        if !last
            print("; ")
        return init
// make tuple
    def override preVisitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>)
        print("ExprMakeTuple [[")
        if expr._type != null
            print("{describe(expr._type)} ")
    def override visitExprMakeTuple(expr:smart_ptr<ExprMakeTuple>) : ExpressionPtr
        print("]]")
        return expr
    def override visitExprMakeTupleIndex(expr:smart_ptr<ExprMakeTuple>;index:int;init:ExpressionPtr;last:bool) : ExpressionPtr
        if !last
            print(", ")
        return init
// array comprehension
    def override preVisitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>)
        print("{expr.generatorSyntax ? "[[" : "[\{"}")
    def override visitExprArrayComprehension(expr:smart_ptr<ExprArrayComprehension>) : ExpressionPtr
        print("{expr.generatorSyntax ? "]]" : "\}]"}")
        return expr
    def override preVisitExprArrayComprehensionSubexpr(expr:smart_ptr<ExprArrayComprehension>;subexrp:ExpressionPtr)
        print("; ")
    def override preVisitExprArrayComprehensionWhere(expr:smart_ptr<ExprArrayComprehension>;filter:ExpressionPtr)
        print("; where ")
// type info
    def override preVisitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>)
        print("ExprTypeInfo typeinfo({expr.trait}")
        if !empty(expr.subtrait)
            print("<{expr.subtrait}")
            if !empty(expr.extratrait)
                print(";{expr.extratrait}")
        print(">")
        if expr.subexpr == null
            print("type<{describe(expr.typeexpr)}>")
    def override visitExprTypeInfo(expr:smart_ptr<ExprTypeInfo>) : ExpressionPtr
        print(")")
        return expr
// ptr to ref
    def override preVisitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>)
        print(" ExprPtr2Ref deref(")
    def override visitExprPtr2Ref(expr:smart_ptr<ExprPtr2Ref>) : ExpressionPtr
        print(")")
        return expr
// label
    def override preVisitExprLabel(expr:smart_ptr<ExprLabel>)
        print("ExprLabel label {expr.labelName}:")
        if !empty(expr.comment)
            print("/*{expr.comment}*/")
// goto
    def override preVisitExprGoto(expr:smart_ptr<ExprGoto>)
        print("ExprGoto goto ")
        if expr.subexpr==null
            print("label {expr.labelName}")
// ref to value
    def override preVisitExprRef2Value(expr:smart_ptr<ExprRef2Value>)
        print("/*r2v*/ (")
    def override visitExprRef2Value(expr:smart_ptr<ExprRef2Value>) : ExpressionPtr
        print(")")
        return expr
// ref 2 ptr
    def override preVisitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>)
        print(" VisitExprRef2Ptr addr(")
    def override visitExprRef2Ptr(expr:smart_ptr<ExprRef2Ptr>) : ExpressionPtr
        print(")")
        return expr
// @@
    def override preVisitExprAddr(expr:smart_ptr<ExprAddr>)
        print("ExprAddr @@")
        if expr.funcType != null
            print("<{describe(expr.funcType)}>")
        print("{expr.target}")
// ascend
    def override preVisitExprAscend(expr:smart_ptr<ExprAscend>)
        if expr.ascType != null
            print("ExprAscend new<{describe(expr.ascType)}> ")
        else
            print("ExprAscend new ")
// cast
    def override preVisitExprCast(expr:smart_ptr<ExprCast>)
        if expr.castFlags.reinterpretCast
            print("ExprCast reinterpret")
        elif expr.castFlags.upcastCast
            print("ExprCast upcast")
        else
            print("ExprCast cast")
        print("<{describe(expr.castType)}> ")
// delete
    def override preVisitExprDelete(expr:smart_ptr<ExprDelete>)
        print("ExprDelete delete ")
        if expr.native
            print("/*native*/ ")
// var
    def override preVisitExprVar(expr:smart_ptr<ExprVar>)
        print("ExprVar {expr.name}")
// field
    def override visitExprField(expr:smart_ptr<ExprField>) : ExpressionPtr
        print(". ExprField {expr.name}")
        return expr
// safe field
    def override visitExprSafeField(expr:smart_ptr<ExprSafeField>) : ExpressionPtr
        print("?. ExprSafeField {expr.name}")
        return expr
// swizzle
    def override visitExprSwizzle(expr:smart_ptr<ExprSwizzle>) : ExpressionPtr
        print(". ExprSwizzle")
        let f2name = [[string "x";"y";"z";"w"]]
        for fch in expr.fields
            let f = int(fch)
            print("{f>=0 && f<=3 ? f2name[f] : "?"}")
        return expr
// is variant
    def override visitExprIsVariant(expr:smart_ptr<ExprIsVariant>) : ExpressionPtr
        print(" ExprIsVariant is {expr.name}")
        return expr
// as variant
    def override visitExprAsVariant(expr:smart_ptr<ExprAsVariant>) : ExpressionPtr
        print(" ExprAsVariant as {expr.name}")
        return expr
// safe as variant
    def override visitExprSafeAsVariant(expr:smart_ptr<ExprSafeAsVariant>) : ExpressionPtr
        print(" ExprSafeAsVariant ?as {expr.name}")
        return expr
// op1
    def override preVisitExprOp1(expr:smart_ptr<ExprOp1>) : void
        let op = "{expr.op}"
        if op!="+++" && op!="---"
            print(op)
        if !noBracket(expr)
            print("(")
    def override visitExprOp1(expr:smart_ptr<ExprOp1>) : ExpressionPtr
        let op = "ExprOp1 {expr.op}"
        if !noBracket(expr)
            print(")")
        if op=="+++" || op=="---"
            print("{slice(op,0,-1)}")
        return expr
// return
    def override preVisitExprReturn(expr:smart_ptr<ExprReturn>) : void
        print("ExprReturn return ")
        if expr.returnFlags.fromYield
            print("/* from yield */ ")
        if expr.returnFlags.moveSemantics
            print("<- ")
// yield
    def override preVisitExprYield(expr:smart_ptr<ExprYield>) : void
        print("ExprYield yield ")
        if expr.returnFlags.moveSemantics
            print("<- ")
// break
    def override preVisitExprBreak(expr:smart_ptr<ExprBreak>) : void
        print("ExprBreak break")
// continue
    def override preVisitExprContinue(expr:smart_ptr<ExprContinue>) : void
        print("ExprContinue continue")
// const ptr
    def override preVisitExprConstPtr(expr:smart_ptr<ExprConstPtr>) : void
        print("{expr.value}")
// const int 8
    def override preVisitExprConstInt8(expr:smart_ptr<ExprConstInt8>) : void
        print("{expr.value}")
// const int 16
    def override preVisitExprConstInt16(expr:smart_ptr<ExprConstInt16>) : void
        print("{expr.value}")
// const int 64
    def override preVisitExprConstInt64(expr:smart_ptr<ExprConstInt64>) : void
        print("{expr.value}")
// const int
    def override preVisitExprConstInt(expr:smart_ptr<ExprConstInt>) : void
        print("{expr.value}")
// const int2
    def override preVisitExprConstInt2(expr:smart_ptr<ExprConstInt2>) : void
        print("int2({expr.value})")
// const int3
    def override preVisitExprConstInt3(expr:smart_ptr<ExprConstInt3>) : void
        print("int3({expr.value})")
// const int4
    def override preVisitExprConstInt4(expr:smart_ptr<ExprConstInt4>) : void
        print("int4({expr.value})")
// const uint 8
    def override preVisitExprConstUInt8(expr:smart_ptr<ExprConstUInt8>) : void
        print("{expr.value}")
// const uint 16
    def override preVisitExprConstUInt16(expr:smart_ptr<ExprConstUInt16>) : void
        print("{expr.value}")
// const uint 64
    def override preVisitExprConstUInt64(expr:smart_ptr<ExprConstUInt64>) : void
        print("{expr.value}")
// const uint
    def override preVisitExprConstUInt(expr:smart_ptr<ExprConstUInt>) : void
        print("{expr.value}")
// const uint2
    def override preVisitExprConstUInt2(expr:smart_ptr<ExprConstUInt2>) : void
        print("uint2({expr.value})")
// const uint3
    def override preVisitExprConstUInt3(expr:smart_ptr<ExprConstUInt3>) : void
        print("uint3({expr.value})")
// const uint4
    def override preVisitExprConstUInt4(expr:smart_ptr<ExprConstUInt4>) : void
        print("uint4({expr.value})")
// const range
    def override preVisitExprConstRange(expr:smart_ptr<ExprConstRange>) : void
        print("range({expr.value})")
// const urnage
    def override preVisitExprConstURange(expr:smart_ptr<ExprConstURange>) : void
        print("urange({expr.value})")
// const bool
    def override preVisitExprConstBool(expr:smart_ptr<ExprConstBool>) : void
        print("{expr.value}")
// const float
    def override preVisitExprConstFloat(expr:smart_ptr<ExprConstFloat>) : void
        print("{expr.value}")
// const float2
    def override preVisitExprConstFloat2(expr:smart_ptr<ExprConstFloat2>) : void
        print("float2({expr.value})")
// const float3
    def override preVisitExprConstFloat3(expr:smart_ptr<ExprConstFloat3>) : void
        print("float3({expr.value})")
// const float4
    def override preVisitExprConstFloat4(expr:smart_ptr<ExprConstFloat4>) : void
        print("float4({expr.value})")
// const double
    def override preVisitExprConstDouble(expr:smart_ptr<ExprConstDouble>) : void
        print("{expr.value}lf")
// fake context
    def override preVisitExprFakeContext(expr:smart_ptr<ExprFakeContext>) : void
        print("__context__")
// fake line info
    def override preVisitExprFakeLineInfo(expr:smart_ptr<ExprFakeLineInfo>) : void
        print("__lineinfo__")
// string
    def override preVisitExprConstString(expr:smart_ptr<ExprConstString>) : void
        peek(expr.value) <| $(str)
            print("ExprConstString \"{escape(str)}\"")
// const enumeraiton
    def override preVisitExprConstEnumeration(expr:smart_ptr<ExprConstEnumeration>) : void
        let enumModule = expr.enumType._module
        if enumModule!=null && !empty(enumModule.name)
            print("{enumModule.name}::")
        print("{expr.enumType.name} {expr.value}")
// const bitfield
    def override preVisitExprConstBitfield(expr:smart_ptr<ExprConstBitfield>) : void
        var name : string
        if expr.bitfieldType!=null && !empty(expr.bitfieldType.alias)
            name = find_bitfield_name(expr.bitfieldType,expr.value)
        if !empty(name)
            print("{expr.bitfieldType.alias} {name}")
        else
            print("bitfield({expr.value})")