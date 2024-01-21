open Angstrom;;

let test_input = "Card   3: 31 97 18 93 71 54 24 12 19 87 | 59 96 72 94  4 51 74 84 47 10 57 89 65 37 39 16 31 91 26 85 44 30 24 40  2"

let input_list = Advent.read_file "inputs/input4"

module IntSet = Set.Make(Int)
type card = {winning : IntSet.t; lotto : IntSet.t}


let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_digit = function '0'..'9' -> true | _ -> false

let whitespace = take_while is_whitespace

let take_till_after (c:char) = 
  take_till (fun a -> a = c) *> char c <?> (Format.sprintf "No %c found" c)


let lister = sep_by whitespace @@ (take_while1 is_digit >>| int_of_string)

let line_parse = 
  let* _ = take_till_after ':' *> whitespace in 
  let* winning_numbers = lister in 
  let* _ = take_till_after '|' *> whitespace in 
  let* lotto_numbers = lister in
  return { winning = IntSet.of_list winning_numbers; lotto = IntSet.of_list lotto_numbers}


let calc_result (crd:card) = 
  let res = IntSet.inter crd.winning crd.lotto |> IntSet.cardinal in 
  match res with 
  | 0 -> 0
  | score -> Core.Int.pow 2 (res - 1)

let part1 acc nxt = 
  let lotto_card = match Angstrom.parse_string ~consume:Prefix line_parse nxt with
    | Ok res -> res
    | Error err -> Fmt.failwith "No semi colon found" err
  in
  let score = calc_result lotto_card in
  acc + score

let result = Core.List.fold ~init:0 ~f:part1 input_list 

let () = Format.printf "Result->17803 -- %d \n" result