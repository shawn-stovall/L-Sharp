module Parser

open Sexp

let toList = Array.toList

let is_balanced (str_sexp: string) : bool =
    let rec iter (str: string, stack: char list, quote_flag: bool) : bool =
        if str = "" then
            if stack.IsEmpty then true else false
        else
            match (str[0], str[1..]) with
            | ('\"', tl) -> iter (tl, stack, not quote_flag)
            | ('\\', tl) when quote_flag && tl.Length >= 2 -> iter (tl[1..], stack, quote_flag)
            | ('(', tl) when not quote_flag -> iter (tl, '(' :: stack, quote_flag)
            | ('{', tl) when not quote_flag -> iter (tl, '{' :: stack, quote_flag)
            | ('[', tl) when not quote_flag -> iter (tl, '[' :: stack, quote_flag)
            | (')', tl) when not quote_flag ->
                if stack.Head = '(' then
                    iter (tl, stack.Tail, quote_flag)
                else
                    false
            | ('}', tl) when not quote_flag ->
                if stack.Head = '{' then
                    iter (tl, stack.Tail, quote_flag)
                else
                    false
            | (']', tl) when not quote_flag ->
                if stack.Head = '[' then
                    iter (tl, stack.Tail, quote_flag)
                else
                    false
            | (_, tl) -> iter (tl, stack, quote_flag) in iter (str_sexp, [], false)

let strip_parens (str: string) : string =
    if str[0] = '(' || str[0] = '{' || str[0] = '[' then
        str[1 .. (str.Length - 2)]
    else
        str

let list_of_sexp_str (sexp_str: string) : string list =
    let stripped = strip_parens sexp_str in stripped.Split(' ') |> toList

let rec sexp_of_sexp_string_list (ssexp_list: string list) =
    match ssexp_list with
    | [] -> Nil
    | hd :: tl ->
        if hd[0] = '(' || hd[0] = '[' || hd[0] = '{' then
            Sexp.cons (sexp_of_sexp_string_list (list_of_sexp_str hd), sexp_of_sexp_string_list tl)
        else
            Sexp.cons (Val(hd), sexp_of_sexp_string_list tl)

let paren_list_of_string (raw_inpt: string) : string list =
    let inpt = raw_inpt.TrimEnd() in inpt.Split ' ' |> toList

let split_str_by_sexp (str: string) : string * string =
    let rec iter (str_t: string * string, paren_count: int) : string * string =
        if paren_count = 0 then
            match str_t with
            | (accum, "") -> (accum, "")
            | (accum, rest) when rest[0] = '(' || rest[0] = '[' || rest[0] = '{' ->
                iter ((accum + string rest[0], rest[1..]), (paren_count + 1))
            | (accum, rest) when rest[0] = ' ' -> (accum, rest[1..])
            | (accum, rest) -> iter ((accum + string rest[0], rest[1..]), paren_count)
        else
            match str_t with
            | (accum, "") -> invalidArg "rest" "rest should not be empty when counting parens"
            | (accum, rest) when rest[0] = '(' || rest[0] = '[' || rest[0] = '{' ->
                iter ((accum + string rest[0], rest[1..]), (paren_count + 1))
            | (accum, rest) when rest[0] = ')' || rest[0] = ']' || rest[0] = '}' ->
                iter ((accum + string rest[0], rest[1..]), (paren_count - 1))
            | (accum, rest) -> iter ((accum + string rest[0], rest[1..]), paren_count) in iter (("", str), 0)
