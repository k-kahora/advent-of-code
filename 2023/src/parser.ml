open Angstrom;;

let test_input = "Card   3: 31 97 18 93 71 54 24 12 19 87 | 59 96 72 94  4 51 74 84 47 10 57 89 65 37 39 16 31 91 26 85 44 30 24 40  2"

type card = {winning : string list; lotto : string list}

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_digit = function '0'..'9' -> true | _ -> false


let whitespace = take_while is_whitespace
let take_till_after (c:char) = 
  take_till (fun a -> a = c) *> char c <?> (Format.sprintf "No %c found" c)
let lister = sep_by whitespace @@ take_while1 is_digit
let after_semi = take_till_after ':' *> whitespace *> lister 
let after_pipe = take_till_after '|' *> whitespace *> lister

let line_parse = 
  let* _ = take_till_after ':' *> whitespace in 
  let* winning_numbers = lister in 
  let* _ = take_till_after '|' *> whitespace in 
  let* lotto_numbers = lister in
  return { winning = winning_numbers; lotto = lotto_numbers}

let result = match Angstrom.parse_string ~consume:Prefix line_parse test_input with
    | Ok res -> res
    | Error err -> Fmt.failwith "No semi colon found" err


let () = List.iter (Format.printf "\n%s") result.winning
let () = Format.printf "\n"
let () = List.iter (Format.printf "\n%s") result.lotto

(* let digit = take_while1 is_digit (\*>>| int_of_string*\) <?> "Input is crap" *)
(* let whitespace = take_while is_whitespace *)
(* let digit_white = whitespace *> take 4 *)
(* let first_digit = take_till is_digit *)


(* let remove_whitespace str = whitespace *> take_till is_whitespace <* whitespace *)


(* let digit_p = Angstrom.parse_string ~consume:Prefix digit_white "    444     "  *)
(* let digit_r = match digit_p with  *)
(* | Ok res -> res *)
(* | Error err -> Fmt.failwith "using exceptions baby!%s" err *)
(* let () = print_string digit_r *)

(* let white_p = Angstrom.parse_string ~consume:All whitespace "  "  *)
(* let white_r = match white_p with  *)
(* | Ok res -> res *)
(* | Error err -> Fmt.failwith "using exceptions baby!%s" err *)
(* let () = print_string "Success" *)

