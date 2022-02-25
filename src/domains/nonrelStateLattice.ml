module Make (ValueLat : Domains.Lattice) = struct
    module State = NonrelState.Make(ValueLat)
    module VarSet = Set.Make(struct
        type t = Language.var
        let compare = compare
    end)

    type t = State.t

    let top : t = Elm(State.StateMap.empty)
    let bot : t = Bot

    let vars (ms : ValueLat.t State.StateMap.t list) =
        let bs = List.concat
            (List.map (fun m -> State.StateMap.bindings m) ms) in
        let ks = List.map fst bs in
        VarSet.of_list ks

    let lte (s1 : t) (s2 : t) : bool =
        match s1, s2 with
        | Bot, _ -> true
        | Elm(_), Bot -> false
        | Elm(m1), Elm(m2) ->
            let vlte x = ValueLat.lte (State.get x s1) (State.get x s2) in
            VarSet.for_all vlte (vars [m1; m2])

    let lub (s1 : t) (s2 : t) : t =
        match s1, s2 with
        | Bot, s | s, Bot -> s
        | Elm(m1), Elm(m2) ->
            let vlub x = ValueLat.lub (State.get x s1) (State.get x s2) in
            let update x s = State.set x (vlub x) s in
            VarSet.fold update (vars [m1; m2]) top

    let glb (s1 : t) (s2 : t) : t =
        match s1, s2 with
        | Bot, _ | _, Bot -> bot
        | Elm(m1), Elm(m2) ->
            let vglb x = ValueLat.glb (State.get x s1) (State.get x s2) in
            let update x s = State.set x (vglb x) s in
            VarSet.fold update (vars [m1; m2]) top

    let widen (s1 : t) (s2 : t) : t =
        match s1, s2 with
        | Bot, s | s, Bot -> s
        | Elm(m1), Elm(m2) ->
            let vwiden x = ValueLat.widen (State.get x s1) (State.get x s2) in
            let update x s = State.set x (vwiden x) s in
            VarSet.fold update (vars [m1; m2]) top

    let to_string (s : t) : string =
        match s with
        | Bot     -> "bot"
        | Elm (m) -> let bs = State.StateMap.bindings m in
                     let ss = List.map (fun (k, v) ->
                        k ^ " -> " ^ (ValueLat.to_string v)) bs in
                     "{" ^ (String.concat "; " ss) ^ "}"
end
