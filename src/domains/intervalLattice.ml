module ZI = ZInf

type t = Bot | Interval of ZI.t * ZI.t

let top = Interval(NegInf, PosInf)
let bot = Bot

let lte (v1 : t) (v2 : t) : bool =
    match v1, v2 with
    | Bot, _ -> true
    | _, Bot -> false
    | Interval(a1, b1), Interval(a2, b2) -> ZI.(a1 >= a2 && b1 <= b2)

let lub (v1 : t) (v2 : t) : t =
    match v1, v2 with
    | Bot, _ | _, Bot -> bot
    | Interval(a1, b1), Interval(a2, b2) -> ZI.(Interval(min a1 a2, max b1 b2))

let glb (v1 : t) (v2 : t) : t =
    match v1, v2 with
    | Bot, v | v, Bot -> v
    | Interval(a1, b1), Interval(a2, b2) ->
        let a' = ZI.max a1 a2 in
        let b' = ZI.min b1 b2 in
        if ZI.(a' <= b') then Interval(a', b') else bot

let to_string (v : t) : string =
    match v with
    | Bot -> "bot"
    | Interval (a, b) -> "[" ^ (ZI.to_string a) ^ ", " ^ (ZI.to_string b) ^ "]"
