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

    let binop (f : ValueLat.t -> ValueLat.t -> ValueLat.t)
              (m1 : ValueLat.t State.StateMap.t)
              (m2 : ValueLat.t State.StateMap.t) =
        let vf x = f (State.get x (Elm(m1))) (State.get x (Elm(m2))) in
        let update x s = State.set x (vf x) s in
        VarSet.fold update (vars [m1; m2]) top

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
        | Elm(m1), Elm(m2) -> binop ValueLat.lub m1 m2

    let glb (s1 : t) (s2 : t) : t =
        match s1, s2 with
        | Bot, _ | _, Bot -> bot
        | Elm(m1), Elm(m2) -> binop ValueLat.glb m1 m2

    let widen (s1 : t) (s2 : t) : t =
        match s1, s2 with
        | Bot, s | s, Bot -> s
        | Elm(m1), Elm(m2) -> binop ValueLat.widen m1 m2

    let to_string (s : t) : string =
        match s with
        | Bot     -> "bot"
        | Elm (m) -> let bs = State.StateMap.bindings m in
                     let ss = List.map (fun (k, v) ->
                        k ^ " -> " ^ (ValueLat.to_string v)) bs in
                     "{" ^ (String.concat "; " ss) ^ "}"

    let of_string (s : string) : t =
        let st = String.trim s in
        if st = "bot" then bot else
        let len = String.length s in
        if not (st.[0] = '{' && st.[len - 1] = '}') then
            failwith "Invalid state syntax ({})" else
        if len = 2 then top else
        let sm = String.sub st 1 (len - 2) in
        let ms = List.map String.trim (String.split_on_char ';' sm) in
        List.fold_left (fun acc var ->
            let ps = List.map String.trim (Str.split (Str.regexp_string "->") var) in
            if List.length ps != 2 then failwith "Invalid state syntax (->)"
            else State.set (List.nth ps 0) (ValueLat.of_string (List.nth ps 1)) acc
        ) top ms
end
