module L = Language

let sOr (b1 : L.b_expr) (b2 : L.b_expr) : L.b_expr =
    L.Not(L.And(L.Not(b1), L.Not(b2)))

let sNe (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    L.Not(L.Eq(a1, a2))

let sLt (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    L.And(L.Le(a1, a2), (sNe a1 a2))

let sGt (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    L.Not(L.Le(a1, a2))

let sGe (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    sOr (sGt a1 a2) (L.Eq(a1, a2))

let sRepeat (b : L.b_expr) (st : L.stm) : L.stm =
    L.Comp(st, L.While(b, st))

let sFor (x : L.var) (a1 : L.a_expr) (a2 : L.a_expr) (st : L.stm) : L.stm =
    L.Comp(
        L.Assign(x, a1),
        L.While(L.Le(L.Var(x), a2), L.Comp(
            st,
            L.Assign(x, L.Sum(L.Var(x), L.Num(Z.one)))
        ))
    )
