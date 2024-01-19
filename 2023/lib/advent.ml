let read_lines (line:string) : unit = 
  print_string line


let more_bs (bs:string) : unit =
    print_endline bs


let read_file file = Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    Core.String.split_lines x)
