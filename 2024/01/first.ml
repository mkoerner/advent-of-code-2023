#load "str.cma";;

module IntMap = Map.Make (Int)

let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let parse s : (int * int) option = 
  match Str.split (Str.regexp "[ ]+") s with
  | x :: y :: [] -> Some(int_of_string x, int_of_string y)
  | _ -> None    

let update_inc = function
  | None -> Some 1
  | Some count -> Some (count + 1)

let rec counts = function
  | [] -> IntMap.empty
  | x::xs ->
    let map = counts xs in
    let map' = IntMap.update x update_inc map in
    map'

let () =
  let file = Sys.argv.(1) in
  let lines = read_lines file in
  let lines = List.filter_map parse lines in
  let xs, ys = List.split lines in
  let d : int = List.fold_left2 (fun acc x y -> acc + Int.abs (x - y)) 0 (List.sort compare xs) (List.sort compare ys) in
  Printf.printf "%d\n" d;
  let cmap = counts ys in
  let lookup x = IntMap.find_opt x cmap |> Option.value ~default:0 in
  let sim = List.fold_left (fun acc x -> acc + x * lookup x) 0 xs in
  Printf.printf "%d\n" sim;
  