(* REMINDER Next time working on this try to give it the Advent.input_lines to parse *)
(* REMINDER also track how long it is takinag you work in this*)
open Angstrom;;
open Format;;
let input = Advent.read_file_as_string "2023/inputs/input5"
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


let digit = take_while1 Lexer.is_digit
let whitespace = take_while Lexer.is_whitespace
let newline = take_till Lexer.newline_char *> char '\n'
let one_newline = char '\n' >>= fun _ -> peek_char_fail >>= fun a -> if Lexer.is_digit a then return () else fail "epic fail"

let matrix_parse = 
  let row_parse = sep_by whitespace (digit >>| int_of_string) in
  sep_by one_newline row_parse

(* Unclean code leaves an empty list at the start *)
let seed_parse = 
  let dubble = newline *> newline *> any_char in  (* double_newline followed by any char *)
  let map_seperate = many_till any_char (string "map:\n") in
  let* seeds = string "seeds:" *> whitespace *> (sep_by whitespace (digit >>| int_of_string)) in
  let* matrix = Angstrom.sep_by map_seperate matrix_parse in
  return (seeds,matrix)

let seeds,matrix = match parse_string ~consume:Prefix seed_parse input  with
  | Ok x -> x
  | Error _ -> failwith "Big error"
let wow = print_endline "hello"
let wow1 = print_endline "hello"
let wow2 = print_endline "hello"
