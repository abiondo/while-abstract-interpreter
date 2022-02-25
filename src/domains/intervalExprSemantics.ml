module StateLat = NonrelStateLattice.Make(IntervalLattice)

type state = StateLat.t
type value = IntervalLattice.t

let rec abstract_a_expr (a : Language.a_expr) (s : state) : value =
    let binop e1 e2 f =
        let v1 = abstract_a_expr e1 s in
        let v2 = abstract_a_expr e2 s in
        match v1, v2 with
        | Bot, _ | _, Bot -> IntervalLattice.bot
        | Interval(a1, b1), Interval(a2, b2) -> f a1 b1 a2 b2
    in
    match a with
    | Num (n) -> Interval(Num(n), Num(n))
    | Var (x) -> StateLat.State.get x s
    | Sum (e1, e2) -> binop e1 e2 (fun a1 b1 a2 b2 ->
        ZInf.(Interval(a1 + a2, b1 + b2)))
    | Sub (e1, e2) -> binop e1 e2 (fun a1 b1 a2 b2 ->
        ZInf.(Interval(a1 - b2, b1 - a2)))
    | Mul (e1, e2) -> binop e1 e2 (fun a1 b1 a2 b2 ->
        let l = ZInf.([a1 * a2; a1 * b2; b1 * a2; b1 * b2]) in
        let a' = List.fold_left ZInf.min (List.hd l) (List.tl l) in
        let b' = List.fold_left ZInf.max (List.hd l) (List.tl l) in
        Interval(a', b'))

let rec abstract_b_expr (b : Language.b_expr) (s : state) : state =
    let rel e1 e2 f =
        let v1 = abstract_a_expr e1 s in
        let v2 = abstract_a_expr e2 s in
        match v1, v2 with
        | Bot, _ | _, Bot -> StateLat.bot
        | Interval(a1, b1), Interval(a2, b2) -> f a1 b1 a2 b2
    in
    match b with
    | Bool (v) -> if v then s else StateLat.bot
    | And (b1, b2) -> StateLat.glb (abstract_b_expr b1 s) (abstract_b_expr b2 s)
    | Or (b1, b2) -> StateLat.lub (abstract_b_expr b1 s) (abstract_b_expr b2 s)
    | Eq (e1, e2) -> abstract_b_expr (And(Le(e1, e2), Le(e2, e1))) s
    | Ne (e1, e2) -> s (* TODO *)
    | Le (e1, e2) -> rel e1 e2 (fun a1 b1 a2 b2 ->
        match e1, e2 with
        | Var(x), Var(y) -> if ZInf.(a1 > b2) then StateLat.bot else
                            StateLat.State.set x (Interval(a1, ZInf.min b1 b2))
                            (StateLat.State.set y (Interval(ZInf.max a1 a2, b2)) s)
        | Var(x), _ -> if ZInf.(a1 > b2) then StateLat.bot else
                       StateLat.State.set x (Interval(a1, ZInf.min b1 b2)) s
        | _ -> s)
    | Ge (e1, e2) -> abstract_b_expr (Le(e2, e1)) s
    | Lt (e1, e2) -> s (* TODO *)
    | Gt (e1, e2) -> s (* TODO *)
