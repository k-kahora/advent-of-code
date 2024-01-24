open Angstrom;;
open Format;;
let test_input = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"

let () = print_endline "We are up"
type pipeline = 
| Finish
| Step of (int -> int) * pipeline
let pipe = Step ((fun a -> a + 3), Finish)


type mapping_scheme = {
source_start : int; source_end : int; dest_start : int
}
let newline = 
  choice [
    string "\r\n" *> return ();  (* CRLF *)
    char '\n' *> return ()       (* LF *)
  ]
let newline_char = function
'\n' | '\r' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false

let is_whitespace_no_newline = function
  (* \x20 -> ascii space character *)
  (* \x0a -> ascii newline character *)
  (* \x0d -> carriage retunr character or \r *)
  (* \x09 -> horizontal tab character *)
  | '\x20' (*| '\x0a' | *) | '\x0d' | '\x09' -> true
  | _ -> false
let digit = take_while1 is_digit
let whitespace = take_while is_whitespace_no_newline
let newline = take_till newline_char *> char '\n'

(* Parse out the seeds *)
(* parse until the string map:\n *)
(* parse the matrix seperated by the map: keyword*)
let one_newline = char '\n' >>= fun _ -> peek_char_fail >>= fun a -> if is_digit a then return () else fail "epic fail"

let matrix_parse = 
  let row_parse = sep_by whitespace (digit >>| int_of_string) in
  sep_by one_newline row_parse

(* Unclean cod leaves an empty list at the start *)
let seed_parse = 
  let dubble = newline *> newline *> any_char in  (* double_newline followed by any char *)
  let map_seperate = many_till any_char (string "map:\n") in
  let* seeds = string "seeds:" *> whitespace *> (sep_by whitespace (digit >>| int_of_string)) in
  let* matrix = Angstrom.sep_by map_seperate matrix_parse in
  return (seeds,matrix)

let t_input = "
seeds: 10 50 30 40

seed-to-soil map:
10 560 5
12 52 80
3 30 38
"

let seeds,matrix = match parse_string ~consume:Prefix seed_parse test_input  with
  | Ok x -> x
  | Error _ -> failwith "Big error"
let () = List.iter (printf "%d ") seeds
let () = printf "\n"
let dummy x = print_endline x
(* int list list list *)


