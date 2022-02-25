type t = Bot | Interval of ZInf.t * ZInf.t

let top = Interval(NegInf, PosInf)
let bot = Bot

let lte (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Bot, _ -> true
    | _, Bot -> false
    | Interval(a1, b1), Interval(a2, b2) -> ZInf.(a1 >= a2 && b1 <= b2)

let lub (v1 : t) (v2 : t) : t =
    match v1, v2 with
    | Bot, v | v, Bot -> v
    | Interval(a1, b1), Interval(a2, b2) -> ZInf.(Interval(min a1 a2, max b1 b2))

let glb (v1 : t) (v2 : t) : t =
    match v1, v2 with
    | Bot, _ | _, Bot -> bot
    | Interval(a1, b1), Interval(a2, b2) ->
        let a' = ZInf.max a1 a2 in
        let b' = ZInf.min b1 b2 in
        if ZInf.(a' <= b') then Interval(a', b') else bot

let widen (v1 : t) (v2 : t) : t =
    match v1, v2 with
    | Bot, v | v, Bot -> v
    | Interval(a1, b1), Interval(a2, b2) -> Interval(
        (if ZInf.(a1 <= a2) then a1 else NegInf),
        (if ZInf.(b1 >= b2) then b1 else PosInf)
    )

let to_string (v : t) : string =
    match v with
    | Bot -> "bot"
    | Interval (a, b) ->
        "[" ^ (ZInf.to_string a) ^ ", " ^ (ZInf.to_string b) ^ "]"
