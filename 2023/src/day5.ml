open Angstrom
open Format
open Lexer
module Ang = Angstrom
module Lex = Lexer

let input = Advent.read_file_as_string "2023/inputs/input5"

type 'a state = Processed of 'a | Passing of 'a

type pipeline = Finish | Step of (int state -> int state) * pipeline

let reverse_pipeline pipe =
  let rec rev acc pipe' =
    match pipe' with
    | Step (f, rest) ->
        rev (Step (f, acc)) rest
    | Finish ->
        acc
  in
  rev Finish pipe

type mapping_scheme = {start: int; finish: int; dest: int}

(* Parse the input string *)

let parse_input input' =
  let one_newline =
    char '\n'
    >>= fun _ ->
    peek_char_fail
    >>= fun a -> if Lex.is_digit a then return () else fail "epic fail"
  in
  let matrix_parse =
    let record_parse =
      let* dest = Lex.wmatch Lex.digit in
      let* source = Lex.wmatch Lex.digit in
      let* range = Lex.wmatch Lex.digit in
      return {start= source; dest; finish= source + range}
    in
    let row_parse = sep_by Lex.whitespace Lex.digit in
    sep_by one_newline record_parse
  in
  (* Unclean code leaves an empty list at the start *)
  let seed_parse =
    let map_seperate = many_till any_char (string "map:\n") in
    let* seeds =
      string "seeds:" *> Lex.whitespace *> sep_by Lex.whitespace Lex.digit
    in
    let* matrix = Ang.sep_by map_seperate matrix_parse in
    return (seeds, matrix)
  in
  match parse_string ~consume:Prefix seed_parse input' with
  | Ok x ->
      x
  | Error _ ->
      failwith "Big error"

(* run a fold over the whole input returning a pipeline *)

(* run through the pipeline *)

let part1 matrix' =
  let init = Step ((fun a -> a), Finish) in
  let map_scheme = function
    | [] ->
        {start= -1; dest= -1; finish= -1}
    | dest_start :: source_start :: range :: _ ->
        {start= source_start; dest= dest_start; finish= source_start + range}
    | _ ->
        failwith "Input is not valid"
  in
  let pipemaker acc nxt =
    let {start; dest; finish} = nxt in
    let next a =
      match a with
      | Processed a ->
          Processed a
      | Passing a ->
          if a <= finish && a >= start then Processed (a - start + dest)
          else Passing a
    in
    Step (next, acc)
  in
  let nxt_pipe =
    Core.List.map ~f:map_scheme matrix' |> Core.List.fold ~init ~f:pipemaker
  in
  nxt_pipe

(* Run the folds neccesary *)
let seeds, matrix = parse_input input

(* if a <= finish && a >= start then Processed (a - start + dest) *)
(* else Passing a *)
let map_seed seed =
  let open Core in
  List.fold matrix ~init:max_int ~f:(fun acc map ->
      let location =
        List.find map ~f:(fun r -> seed <= r.finish && seed >= r.start)
      in
      match location with
      | Some x ->
          min acc (seed - x.start + x.dest)
      | None ->
          acc )

(* let map_seed seed = *)
(*   let reset_pipe (st : int state) : int state = *)
(*     match st with Passing a -> Passing a | Processed a -> Passing a *)
(*   in *)
(*   let rec process_pipe (input : int state) (line : pipeline) = *)
(*     match reverse_pipeline line with *)
(*     | Step (f, rest) -> *)
(*         process_pipe (f input) rest *)
(*     | Finish -> *)
(*         input *)
(*   in *)
(*   let process_all_pipes = *)
(*     Core.List.fold *)
(*       ~f:(fun state pipe -> process_pipe state pipe |> reset_pipe) *)
(*       (Core.List.map ~f:part1 matrix) *)
(*   in *)
(*   let ext = function Passing a -> a | Processed a -> a in *)
(*   process_all_pipes ~init:(Passing seed) |> ext *)

let final sd =
  Core.List.fold ~init:max_int ~f:(fun acc seed -> min acc @@ map_seed seed) sd
