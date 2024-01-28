open Angstrom;;
open Lexer;;
open Format;;
module Ang = Angstrom
module Lex = Lexer;;

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

let reverse_pipeline pipe = 
  let rec rev acc pipe' = 
    match pipe' with
    | Step (f, rest) -> rev (Step (f,acc)) rest 
    | Finish -> acc
in
rev Finish pipe



type mapping_scheme = {
start : int; finish : int; dest : int
}

let print_map map' = printf "start: %d, finish: %d, dest: %d" map'.start map'.finish map'.dest

(* let digit = take_while1 Lexer.is_digit *)
(* let whitespace = take_while Lexer.is_whitespace *)
(* let newline = take_till Lexer.newline_char *> char '\n' *)
let one_newline = char '\n' >>= fun _ -> peek_char_fail >>= fun a -> if Lex.is_digit a then return () else fail "epic fail"

let matrix_parse = 
  let row_parse = sep_by Lex.whitespace (Lex.digit >>| int_of_string) in
  sep_by one_newline row_parse

(* Unclean code leaves an empty list at the start *)
let seed_parse = 
  let map_seperate = many_till any_char (string "map:\n") in
  let* seeds = string "seeds:" *> Lex.whitespace *> (sep_by Lex.whitespace (Lex.digit >>| int_of_string)) in
  let* matrix = Ang.sep_by map_seperate matrix_parse in
  return (seeds,matrix)

let seeds,matrix = match parse_string ~consume:Prefix seed_parse test_input  with
  | Ok x -> x
  | Error _ -> failwith "Big error"

let map_scheme = function
  | [] -> {start = (-1); dest = (-1); finish = (-1)}
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
(* NOTE Clean up all this test code here *)
(* The code for each matrix pipeline is done  *)
(* Final thins should involve looping throug each matrix applying the number to the pipeline *)
(* Pass that input to the next, so a big fold *)
let matrix_to_pipe = Core.List.map ~f:map_scheme (* test_input needs to run on everty matrix *)
let folder = Core.List.fold ~init:init ~f:pipemaker (* folder is a pipeline *)
(* run a fold over the whole input returning a pipeline *)
let rec process_pipe (input:int state) (line:pipeline) = match reverse_pipeline line with
| Step (f, rest) -> process_pipe (f input) rest
| Finish -> input

let part1 matrix' =
  let nxt_pipe = matrix_to_pipe matrix' |> folder in
  nxt_pipe
  
let reset_pipe (st: int state) : int state = 
match st with
| Passing a -> printf "%d\n" a;Passing a
| Processed a -> printf "%d\n" a;Passing a



let final_pipeline = Core.List.map ~f:part1 matrix
let result = Core.List.fold ~f:(fun state pipe -> (process_pipe state pipe) |> reset_pipe) ~init:(Passing 14) final_pipeline

let () = match result with 
| Passing a -> printf "\n%d" a
| Processed a -> printf "\n%d" a



