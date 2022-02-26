module Make
    (ValueLat : Domains.Lattice)
    (ExprSem : Domains.ExprSemantics
        with type state = NonrelStateLattice.Make(ValueLat).t
        and type value = ValueLat.t)
= struct
    include NonrelStateLattice.Make(ValueLat)
    include NonrelStmSemantics.Make(ValueLat)(ExprSem)
end
