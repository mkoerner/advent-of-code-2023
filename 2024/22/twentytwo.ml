let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let mix x y = x lxor y
let prune x = x mod 16777216

let next secret =
  let secret' = secret |> mix (64 * secret) |> prune in
  let secret'' = secret' |> mix (secret' / 32) |> prune in
  let secret''' = secret'' |> mix (2048 * secret'') |> prune in
  secret'''

let rec pow f n s =
  if n <= 0 then s
  else
    let s' = f s in
    pow f (n - 1) s'

let solve file =
  file |> read_lines
  |> List.filter (( <> ) "")
  |> List.map int_of_string
  |> List.map (pow next 2000)
  |> List.fold_left ( + ) 0
