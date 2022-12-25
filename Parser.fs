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

let paren_list_of_string (raw_inpt: string) : string list =
    let inpt = raw_inpt.TrimEnd() in inpt.Split ' ' |> toList
