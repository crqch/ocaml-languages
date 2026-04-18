
let rec fib x =
    match x with
    | 0 -> 1
    | 1 -> 1
    | w -> fib (w-2) + fib (w-1);;


let () =
  () |> read_line |> int_of_string |> fib |> string_of_int |> print_endline
