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

type 'a state =
| Processed of 'a
| Passing of 'a

type pipeline = 
| Finish
| Step of (int state -> int state) * pipeline


type mapping_scheme = {
start : int; finish : int; dest : int
}

let print_map map' = printf "start: %d, finish: %d, dest: %d" map'.start map'.finish map'.dest

(* let digit = take_while1 Lexer.is_digit *)
(* let whitespace = take_while Lexer.is_whitespace *)
(* let newline = take_till Lexer.newline_char *> char '\n' *)
let one_newline = char '\n' >>= fun _ -> peek_char_fail >>= fun a -> if Lexer.is_digit a then return () else fail "epic fail"

let matrix_parse = 
  let row_parse = sep_by Lexer.whitespace (Lexer.digit >>| int_of_string) in
  sep_by one_newline row_parse

(* Unclean code leaves an empty list at the start *)
let seed_parse = 
  let map_seperate = many_till any_char (string "map:\n") in
  let* seeds = string "seeds:" *> Lexer.whitespace *> (sep_by Lexer.whitespace (Lexer.digit >>| int_of_string)) in
  let* matrix = Angstrom.sep_by map_seperate matrix_parse in
  return (seeds,matrix)

let seeds,matrix = match parse_string ~consume:Prefix seed_parse input  with
  | Ok x -> x
  | Error _ -> failwith "Big error"

let map_scheme = function
  | [] -> {start = 0; dest = 0; finish = 0}
  | dest_start :: source_start :: range :: _ ->
      {start = source_start; dest = dest_start; finish = source_start + range}
  | _ -> failwith "Input is not valid"

(* Return a functin int -> int *)
let pipemaker acc nxt = 
  let {start; dest; finish} = nxt in
  let next = (fun a -> 
    match a with 
    | Processed a -> Processed a
    | Passing a -> 
       if a <= finish && a >= start then Processed ( a - start + dest ) else Passing a
  ) in
  Step (next,acc)
  
let init = Step ((fun a -> a), Finish)
let m_input = 
[[49; 53; 8;];
[0; 11; 42;];
[42; 0; 7;];
[57; 7; 4;]]
let test_input = Core.List.map ~f:map_scheme m_input
let () = List.iter print_map test_input
let folder = Core.List.fold ~init:init ~f:pipemaker test_input
let rec result (input:int state) (line:pipeline) = match line with
| Step (f, rest) -> result (f input) rest
| Finish -> input

let res = result (Passing 81) folder
let () = match res with 
| Passing a -> printf "\n%d" a
| Processed a -> printf "\n%d" a



