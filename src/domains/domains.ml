module type Lattice = sig
    type t
    val top : t
    val bot : t
    val lte : t -> t -> bool
    val lub : t -> t -> t
    val glb : t -> t -> t
    val widen : t -> t -> t
    val to_string : t -> string
end

module Fixpoint (L : Lattice) = struct
    let lfp (f : L.t -> L.t) : L.t =
        let rec iter y =
            let y' = f y in
            if L.lte y' y then y else iter (L.widen y y')
        in iter L.bot
end

module type StmSemantics = sig
    type state
    val abstract_stm : Language.stm -> state -> state
end

module type ExprSemantics = sig
    type state
    type value
    val abstract_a_expr: Language.a_expr -> state -> value
    val abstract_b_expr: Language.b_expr -> state -> state
end

module type Domain = sig
    module StateLat : Lattice
    module StmSem : StmSemantics with type state = StateLat.t
end
