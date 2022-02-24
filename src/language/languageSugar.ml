module L = Language

let sLt (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    L.And(L.Le(a1, a2), L.Ne(a1, a2))

let sGt (a1 : L.a_expr) (a2 : L.a_expr) : L.b_expr =
    L.And(L.Ge(a1, a2), L.Ne(a1, a2))

let rec sNot (b : L.b_expr) : L.b_expr =
    match b with
    | L.Bool (v) -> L.Bool(not v)
    | L.And (b1, b2) -> L.Or(sNot b1, sNot b2)
    | L.Or (b1, b2) -> L.And(sNot b1, sNot b2)
    | L.Eq (a1, a2) -> L.Ne(a1, a2)
    | L.Ne (a1, a2) -> L.Eq(a1, a2)
    | L.Le (a1, a2) -> sGt a1 a2
    | L.Ge (a1, a2) -> sLt a1 a2

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
