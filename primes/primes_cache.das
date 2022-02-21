module primes_cache

require ast_boost
require daslib/templates_boost

[function_macro(name="cached_primes")]
class CacheFunctionAnnotation : AstFunctionAnnotation
    def override apply(var func: FunctionPtr; var group: ModuleGroup; args: AnnotationArgumentList; var errors: das_string): bool
        var arg = find_arg("count", args)
        var count = arg as tInt

        let gen_fnname <- "{func.name}`compileTime"
        let gen_varname <- "{func.name}`compileTimeResult"
        var fn <- new [[Function() at=func.at, atDecl=func.at, name := gen_fnname]]
        fn.flags |= FunctionFlags generated
        fn.flags |= FunctionFlags init
        fn.flags |= FunctionFlags privateFunction
        fn.result <- new [[TypeDecl() baseType=Type tVoid, at=func.at]]

        var blk <- new [[ExprBlock() at=func.at]]
        var ff <- new [[ExprCall() name:=func.name]]
        ff.arguments |> emplace_new <| new [[ExprConstInt() value:=count]]
        ff.arguments |> emplace_new <| new [[ExprVar() name:=gen_varname]]
        blk.list |> emplace(ff)

        fn.body <- blk

        var intType <- new [[TypeDecl() baseType=Type tInt]]
        var intArrayType <- new [[TypeDecl() baseType=Type tArray, firstType <- intType]]
        add_global_var(compiling_module(), gen_varname, intArrayType, func.at, false)
        compiling_module() |> add_function(fn)
        return true