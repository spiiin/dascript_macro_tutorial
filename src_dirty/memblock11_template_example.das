require ast
require daslib/ast_boost
require daslib/templates
require daslib/templates_boost

[export]
def test
    var expr <- quote() <|
        x ++
        print("{x}\n")

    print(describe(expr))

    var rules : Template
    //rules |> replaceVariable("x") <| ExpressionPtr(new [[ExprVar() name:="y"]])
    rules |> renameVariable("x", "y")

    apply_template(rules, expr.at, expr)

    print(describe(expr))
