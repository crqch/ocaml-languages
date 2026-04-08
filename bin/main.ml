let welcome () =
  print_string
{|
 ####   ###   #      ####
#      #   #  #     #
#      #####  #     #
 ####  #   #  #####  ####
|}

let rec prompt () =
  print_newline ();
  print_string "> ";
  flush stdout;
  match read_line () with
  | "" -> prompt ()
  | s -> s

let rec repl () =
  prompt () |> Calc.Eval.interp |> print_int |> repl

let () =
  welcome ();
  try repl () with
    End_of_file -> print_endline "bye!"
