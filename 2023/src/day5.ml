open Angstrom;;
open Lexer;;
open Format;;
module Ang = Angstrom
module Lex = Lexer;;

let input = Advent.read_file_as_string "2023/inputs/input5"

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

let seeds,matrix = match parse_string ~consume:Prefix seed_parse input  with
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
| Passing a -> Passing a
| Processed a -> Passing a



let final_pipeline = Core.List.map ~f:part1 matrix
let result = Core.List.fold ~f:(fun state pipe -> (process_pipe state pipe) |> reset_pipe) final_pipeline


let final = Core.List.fold ~init:max_int ~f:(fun min seed -> 
                let ext = function 
                  | Passing a -> a
                  | Processed a -> a
                in
                let location = result ~init:(Passing seed) |> ext in
                if location < min then location else min
              ) seeds

let () = printf "Result: %d, " final



