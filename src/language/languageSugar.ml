module L = Language

let rec sNot (b : L.b_expr) : L.b_expr =
    match b with
    | Bool (v)     -> Bool(not v)
    | And (b1, b2) -> Or(sNot b1, sNot b2)
    | Or (b1, b2)  -> And(sNot b1, sNot b2)
    | Eq (a1, a2)  -> Ne(a1, a2)
    | Ne (a1, a2)  -> Eq(a1, a2)
    | Le (a1, a2)  -> Gt(a1, a2)
    | Ge (a1, a2)  -> Lt(a1, a2)
    | Lt (a1, a2)  -> Ge(a1, a2)
    | Gt (a1, a2)  -> Le(a1, a2)

let sRepeat (b : L.b_expr) (st : L.stm) : L.stm =
    Comp(st, While(b, st))

let sFor (x : L.var) (a1 : L.a_expr) (a2 : L.a_expr) (st : L.stm) : L.stm =
    Comp(
        Assign(x, a1),
        While(Le(Var(x), a2), Comp(
            st,
            Assign(x, Sum(Var(x), Num(Z.one)))
        ))
    )
