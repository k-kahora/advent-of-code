open Angstrom
open Format
open Lexer
module Ang = Angstrom
module Lex = Lexer

let input = Advent.read_file_as_string "2023/inputs/input5"

let test_input = Advent.read_file_as_string "2023/inputs/test_input_day5"

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

let seeds, matrix = parse_input input

let map_seed seed =
  let open Core in
  List.fold matrix ~init:seed ~f:(fun acc map ->
      let location =
        List.find map ~f:(fun r -> acc <= r.finish && acc >= r.start)
      in
      match location with Some x -> acc - x.start + x.dest | None -> acc )

let part1 seeds' =
  let open Core in
  List.fold seeds' ~init:max_int ~f:(fun mini seed ->
      min mini @@ map_seed seed )

let () = Format.printf "Result 318728750 -> %d" @@ part1 seeds
