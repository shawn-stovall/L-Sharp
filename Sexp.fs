module Sexp

type t<'a, 'b> =
    | Cell of 'a * 'b
    | Nil

let car (cell: t<'a, 'b>) =
    match cell with
    | Cell(a, _) -> Some(a)
    | _ -> None

let cdr (cell: t<'a, 'b>) =
    match cell with
    | Cell(_, b) -> Some(b)
    | _ -> None

let cons (a: 'a, b: 'b) = Cell(a, b)
