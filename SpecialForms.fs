// SpecialForms are forms that are not, or will not be, definable
// natively in the language.  They either have side-effects or no
// type.
module SpecialForms

open Sexp

type t =
    | Val
    | Sexp
    | Fun
    | Identifier

// Forms used for verification
let forms: (string list) = [ "define"; "fun"; "let"; "if"; "cond" ]
