require macro_test
require daslib/ast_boost
require daslib/templates
require macro_print_ast
require ast_print

struct Vec2
    x, y : float

[memblock, dump_fields]
struct Memblock
    a: int?
    b: float?
    c: int?
    d: Vec2?

[memblock, dump_fields]
struct MemblockInheritor : Memblock
    e: int?

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

def debug_reflection
    let fd_gen <- typeinfo(ast_function @@init`struct`Memblock)
    var astVisitor2 = new PrintVisitorSource()
    var astVisitorAdapter2 <- make_visitor(*astVisitor2)
    print("---gen_text--------------\n")
    visit(fd_gen, astVisitorAdapter2)

def print_data
    unsafe
        var memblock: Memblock
        memblock |> init`struct`Memblock(5,5,5,1)
        memblock |> fillMemblock
        print("Memblock = {memblock}\n")

        var memblock2: MemblockInheritor
        memblock2 |> init`struct`MemblockInheritor(5,5,5,1,10)
        //memblock2 |> fillMemblock
        print("Memblock2 = {memblock2}\n")
[export]
def main
    debug_reflection()
    //print_data()
   

