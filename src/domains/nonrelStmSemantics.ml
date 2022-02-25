module Make
    (ValueLat : Domains.Lattice)
    (ExprSem : Domains.ExprSemantics
        with type state = NonrelStateLattice.Make(ValueLat).t 
        and type value = ValueLat.t)
= struct
    module StateLat = NonrelStateLattice.Make(ValueLat)
    module StateFix = Domains.Fixpoint(StateLat)

    type state = StateLat.t

    let rec abstract_stm (st : Language.stm) (s : state) : state =
        match st with
        | Skip             -> s
        | Assign (x, a)    -> let v = ExprSem.abstract_a_expr a s in
                              StateLat.State.set x v s
        | Comp (st1, st2)  -> abstract_stm st2 (abstract_stm st1 s)
        | If (b, st1, st2) ->
            let s_tt = ExprSem.abstract_b_expr b s in
            let s_ff = ExprSem.abstract_b_expr (LanguageSugar.sNot b) s in
            StateLat.lub (abstract_stm st1 s_tt) (abstract_stm st2 s_ff)
        | While (b, st)    ->
            let f s' =
                let s'_tt = ExprSem.abstract_b_expr b s' in
                StateLat.lub s (abstract_stm st s'_tt)
            in
                ExprSem.abstract_b_expr (LanguageSugar.sNot b) (StateFix.lfp f)
end
