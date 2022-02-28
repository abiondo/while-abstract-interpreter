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

let of_string (s : string) : t =
    let st = String.trim s in
    if st = "bot" then bot else
    let len = String.length s in
    if not (st.[0] = '[' && st.[len - 1] = ']') then
        failwith "Invalid interval syntax ([])" else
    let si = String.sub st 1 (len - 2) in
    let ns = List.map String.trim (String.split_on_char ',' si) in
    if List.length ns != 2 then failwith "Invalid interval syntax (,)" else
    let a = ZInf.of_string (List.nth ns 0) in
    let b = ZInf.of_string (List.nth ns 1) in
    match a, b with
    | _, NegInf -> failwith "Invalid interval bounds (upper -inf)"
    | PosInf, _ -> failwith "Invalid interval bounds (lower +inf)"
    | _ -> if ZInf.(a > b) then failwith "Invalid interval bounds (a > b)"
           else Interval(a, b)
