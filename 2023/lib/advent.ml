let print_list ls =
  Format.printf "[" ;
  Core.List.iter ls ~f:(fun a -> Format.printf "%s; " a) ;
  Format.printf "]"

let read_file file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
      let x = In_channel.input_all channel in
      Core.String.split_lines x )

let read_file_as_string file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
      In_channel.input_all channel )

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let to_sequence start finish =
  let f nxt = if nxt > finish then None else Some (nxt, nxt + 1) in
  Seq.unfold f start
