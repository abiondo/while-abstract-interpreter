type t = NegInf | Num of Z.t | PosInf

let to_string a =
    match a with
    | NegInf -> "-inf"
    | PosInf -> "+inf"
    | Num (n) -> Z.to_string n

let min a b =
    match a, b with
    | NegInf, _ | _, NegInf -> NegInf
    | PosInf, n | n, PosInf -> n
    | Num(na), Num(nb) -> if Z.(na < nb) then a else b

let max a b =
    match a, b with
    | NegInf, n | n, NegInf -> n
    | PosInf, _ | _, PosInf -> PosInf
    | Num(na), Num(nb) -> if Z.(na > nb) then a else b

let compare a b =
    if a = b then 0 else
    if min a b = a then -1
    else 1

let add a b =
    match a, b with
    | NegInf, PosInf | PosInf, NegInf -> failwith "Undefined"
    | NegInf, _ | _, NegInf -> NegInf
    | PosInf, _ | _, PosInf -> PosInf
    | Num(na), Num(nb) -> Num(Z.(na + nb))

let sub a b =
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> failwith "Undefined"
    | NegInf, _ | _, PosInf -> NegInf
    | PosInf, _ | _, NegInf -> PosInf
    | Num(na), Num(nb) -> Num(Z.(na - nb))

let mul (a : t) (b : t) : t =
    match a, b with
    | Num(n), _ when n = Z.zero -> Num(Z.zero)
    | _, Num(n) when n = Z.zero -> Num(Z.zero)
    | NegInf, PosInf | PosInf, NegInf -> NegInf
    | NegInf, NegInf | PosInf, PosInf -> PosInf
    | NegInf, Num(n) | Num(n), NegInf -> if Z.(n > zero) then NegInf else PosInf
    | PosInf, Num(n) | Num(n), PosInf -> if Z.(n > zero) then PosInf else NegInf
    | Num(na), Num(nb) -> Num(Z.(na * nb))

let (+) = add
let (-) = sub
let ( * ) = mul

module Compare = struct
    let (=) a b = compare a b = 0
    let (<) a b = compare a b < 0
    let (>) a b = compare a b > 0
    let (<=) a b = compare a b <= 0
    let (>=) a b = compare a b >= 0
    let (<>) a b = compare a b != 0
end
