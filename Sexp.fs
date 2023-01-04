module Sexp

type t =
    | Cell of t * t
    | Val of string
    | SpecForm of string
    | Nil

let car (cell: t) =
    match cell with
    | Cell(a, _) -> Some(a)
    | _ -> None

let cdr (cell: t) =
    match cell with
    | Cell(_, b) -> Some(b)
    | _ -> None

let cons (a: t, b: t) = Cell(a, b)
