module StateLat = NonrelStateLattice.Make(IntervalLattice)
module StmSem = NonrelStmSemantics.Make(IntervalLattice)(IntervalExprSemantics)
