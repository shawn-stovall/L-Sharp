// SpecialForms are forms that are not, or will not be, definable
// natively in the language.  They either have side-effects or no
// type.
module SpecialForms

open Sexp

type expected =
    | Val
    | Sexp
    | Fun
    | Identifier

// Forms used for verification
let forms: (string * t<expected, t<expected, t<'a, 'b>>>) list =
    [ ("define", Cell(Identifier, Cell(Val, Nil))) ]
