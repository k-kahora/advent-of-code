let () = Advent.read_lines "Hello Player"
let () = Advent.more_bs "What the hell"
let () = Core.List.iter ["1"; "2"] ~f:(Format.printf "%s")
let input_list = Advent.read_file "inputs/input4"
let () = Core.List.iter ~f:(Format.printf "%s\n") input_list
