module Make (ValueLat : Domains.Lattice) = struct
    module StateMap = Map.Make(struct
        type t = Language.var
        let compare = compare
    end)

    type t = Bot | Elm of ValueLat.t StateMap.t

    let get (x : Language.var) (s : t) : ValueLat.t =
        match s with
        | Bot     -> ValueLat.bot
        | Elm (m) -> match StateMap.find_opt x m with
            | Some (y) -> y
            | None     -> ValueLat.top

    let set (x : Language.var) (y : ValueLat.t) (s : t) : t =
        if y = ValueLat.bot then Bot else
        match s with
        | Bot     -> Bot
        | Elm (m) -> Elm(if y = ValueLat.top then StateMap.remove x m
                         else StateMap.add x y m)
end
