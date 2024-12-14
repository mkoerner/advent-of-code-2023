#load "str.cma"

let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

type robot = { p : int * int; v : int * int }
type bathroom = Bathroom of int * int
type quadrant = TL | TR | BL | BR

let parse2 str : int * int =
  let (_ :: v :: _) = String.split_on_char '=' str in
  let (x :: y :: _) = String.split_on_char ',' v in
  (int_of_string x, int_of_string y)

let parse_line (str : string) =
  let (p :: v :: _) = Str.split (Str.regexp "[ ]+") str in
  { p = parse2 p; v = parse2 v }

let to_string { p = x, y; v = vx, vy } = Printf.sprintf "%d,%d %d,%d" x y vx vy

let ( % ) a b =
  let m = a mod b in
  if m < 0 then m + b else m

let forward (Bathroom (w, h)) steps { p = x, y; v = vx, vy } =
  let x' = (x + (steps * vx)) % w in
  let y' = (y + (steps * vy)) % h in
  { p = (x', y'); v = (vx, vy) }

let quadrant (Bathroom (w, h)) { p = x, y; v = vx, vy } =
  let w_half = w / 2 in
  let h_half = h / 2 in
  if x < w_half && y < h_half then Some TL
  else if x < w_half && y >= h - h_half then Some BL
  else if x >= w - w_half && y >= h - h_half then Some BR 
  else if x >= w - w_half && y < h_half then Some TR
  else None

let safety_factor qs =
  (qs |> List.filter (fun q -> q = Some BL) |> List.length)
  * (qs |> List.filter (fun q -> q = Some BR) |> List.length)
  * (qs |> List.filter (fun q -> q = Some TL) |> List.length)
  * (qs |> List.filter (fun q -> q = Some TR) |> List.length)

let () =
  let w, h = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) in
  let steps = int_of_string Sys.argv.(3) in
  let file = Sys.argv.(4) in
  let robots = read_lines file |> List.map parse_line in
  let bathroom = Bathroom (w, h) in
  let robots' = robots |> List.map (forward bathroom steps) in
  let qs = robots' |> List.map (quadrant bathroom) in
  Printf.printf "%d\n" (safety_factor qs)
