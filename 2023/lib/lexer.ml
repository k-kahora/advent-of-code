open Angstrom

let newline_char = function '\n' | '\r' -> true | _ -> false

let is_whitespace = function
  (* \x20 -> ascii space character *)
  (* \x0a -> ascii newline character *)
  (* \x0d -> carriage retunr character or \r *)
  (* \x09 -> horizontal tab character *)
  | '\x20' | '\x0d' | '\x09' ->
      true
  | _ ->
      false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace_newline = function
  (* \x20 -> ascii space character *)
  (* \x0a -> ascii newline character *)
  (* \x0d -> carriage retunr character or \r *)
  (* \x09 -> horizontal tab character *)
  | '\x20' | '\x0a' | '\x0d' | '\x09' ->
      true
  | _ ->
      false

(* Angstrom parsers general purpose *)

let whitespace = take_while is_whitespace

let digit = take_while1 is_digit >>| int_of_string

let newline = take_till newline_char *> char '\n'

let wmatch a = whitespace *> a <* whitespace
