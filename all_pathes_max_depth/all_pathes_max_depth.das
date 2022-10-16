//example, print all pathes in tree with max depth

require daslib/functional

struct Tree
    data : int
    left, right : Tree?

var tree = new [[ Tree data = 5,
    left = new [[Tree data = 1, 
        right = new [[Tree data = 2]]
    ]],
    right = new [[Tree data = 7,
        right = new [[Tree data = 10]]
    ]]
]]

//TODO: optimize
def clone_array(a: array<int>; newData:int)
    unsafe
        var newArr <- to_array(each(a))
        newArr |> push <| newData
        return <- newArr

def each_element(var tree:Tree?; path: array<int>; depth:int; blk:lambda<(what: int; path: array<int>; depth: int):void>)
    if tree.left != null
        each_element(tree.left, clone_array(path, tree.data), depth+1, blk)
    invoke(blk, tree.data, path, depth)
    if tree.right != null
        each_element(tree.right, clone_array(path, tree.data), depth+1, blk)

[export]
def main
    let startPath: array<int>
    var globalResult : table<int; array<array<int>>>
    unsafe
        tree |> each_element(startPath, 0) <| @[[&globalResult]](value: int; path: array<int>; depth: int)
            globalResult[depth] |> push_clone <| clone_array(path, value)
    //find max depth
    let maxDepth = reduce(keys(globalResult)) <| $(left, right : int)
        return left > right ? left : right
    //print all pathes
    print("All pathes with longest depth:\n")
    for path in globalResult[maxDepth]
        print("{path}\n")

//All pathes with longest depth:
//[[ 5; 1; 2]]
//[[ 5; 7; 10]]

