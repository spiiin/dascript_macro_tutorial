// reduce value, any invokable
def private reduce_while_any(it:iterator<auto(TT)>;functor; condition)
    var left : TT -&
    var first = true
    var iteration = 0
    for right in it
        if first
            first = false
            static_if typeinfo(can_copy type<TT>)
                left = right
            else
                left := right
        else
            if !invoke(condition, left, ++iteration)
                break
            static_if typeinfo(can_copy type<TT>)
                left = invoke(functor,left,right)
            else
                left := invoke(functor,left,right)
           
    if first
        panic("can't reduce empty sequence")
    return left

// reduce with lambda
def reduce_while(it:iterator<auto(TT)>;blk:lambda<(left,right:TT -&):TT const -&>;condition)
    //! iterates over `it` and yields the reduced (combined) result of `blk` for each element
    //! and previous reduction result
    return reduce_while_any(it,blk,condition)

// reduce with function
def reduce_while(it:iterator<auto(TT)>;blk:function<(left,right:TT -&):TT const -&>;condition)
    return reduce_while_any(it,blk,condition)

// reduce with block
def reduce_while(it:iterator<auto(TT)>;blk:block<(left,right:TT -&):TT const -&>;condition)
    return reduce_while_any(it,blk,condition)