open Format;;
open Set;;
let input_list = Advent.read_file "inputs/input4"
let test_input = "Card   3: 31 97 18 93 71 54 24 12 19 87 | 59 96 72 94  4 51 74 84 47 10 57 89 65 37 39 16 31 91 26 85 44 30 24 40  2
"

let index_of (ch:char) (str:string) : int = 
  Core.String.find_mapi str ~f:(fun idx c -> if c = ch then Some idx else None) |> Option.get

let get_lotto_num start last str = 
  Core.String.slice str start last |>
  Core.String.split_on_chars ~on:[' '] |>
  Core.List.filter ~f:(fun s -> "" <> String.trim s) |> 
  Core.List.map ~f:(fun a -> int_of_string a) 

module IntSet = Set.Make(Int)

let add_to_set ls int_set = 
  Core.List.fold ~init:int_set ~f:(fun acc nxt -> IntSet.add nxt acc) ls
  
let count = ref 0

let rec calc_score_ref ?(acc = 1) = function
    | 0 -> 0
    | 1 -> acc
    | x -> calc_score_ref ~acc:(acc * 2) (x - 1)

let part1 acc nxt = 
let start_index = index_of ':' nxt in 
let middle_index = index_of '|' nxt in
let end_index = Core.String.length nxt in
let winning_numbers = get_lotto_num (start_index + 1) middle_index nxt in 
let lotto_numbers = get_lotto_num (middle_index + 1) (end_index) nxt in
let winning_score =  
    let winning_set = add_to_set winning_numbers IntSet.empty in
    let calc_score acc' nxt' = 
      if IntSet.mem nxt' winning_set then count := !count + 1 else ();
      if IntSet.mem nxt' winning_set then acc' * 2 else acc' 
    in
    let _ = Core.List.fold ~init:1 ~f:calc_score lotto_numbers in
    calc_score_ref !count
in
let temp = !count in
count := 0;
((fst acc + winning_score), temp :: (snd acc ))

let cards = []

let () = printf "=================Part1=================\n"
let part2_tup = Core.List.fold ~f:part1 ~init:(0,[]) input_list 
let () = (fun (a,_) -> printf "Score: %d" a) part2_tup
let () = printf "=================Part2=================\n"
let () = List.iter (printf "%d\n") (snd part2_tup)
