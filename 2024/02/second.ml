#load "str.cma"

let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let parse_line str : int list =
  Str.split (Str.regexp "[ ]+") str |> List.map int_of_string

let safe_up (b, x) x' =
  let b' = b && x' >= x + 1 && x' <= x + 3 in
  (b', x')

let safe_down (b, x) x' =
  let b' = b && x' <= x - 1 && x' >= x - 3 in
  (b', x')

let is_safe = function
  | [] -> true
  | x :: xs ->
      let up, _ = List.fold_left safe_up (true, x) xs in
      let down, _ = List.fold_left safe_down (true, x) xs in
      up || down

let drop1 xs =
  let rec drop1' rhead res = function
    | x :: xs ->
        let res' = [ List.rev_append rhead xs ] @ res in
        let rhead' = x :: rhead in
        drop1' rhead' res' xs
    | [] -> res
  in
  drop1' [] [] xs

let is_safe' xs =
  let any = List.fold_left ( || ) false in
  let dxs = xs :: drop1 xs in
  dxs |> List.map is_safe |> any

let () =
  let file = Sys.argv.(1) in
  let lines = read_lines file |> List.map parse_line in
  let safe = lines |> List.map is_safe in
  Printf.printf "%d\n" (safe |> List.filter (fun x -> x) |> List.length);
  let safe' = lines |> List.map is_safe' in
  Printf.printf "%d\n" (safe' |> List.filter (fun x -> x) |> List.length)
