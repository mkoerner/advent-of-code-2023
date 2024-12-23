let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let parse_line line =
  line |> String.split_on_char '-' |> function
  | a :: b :: _ -> [ (a, b); (b, a) ]
  | _ -> failwith "parse error"

let rec combinations =
  let combinations' x xs = xs |> List.map (fun x' -> (x, x')) in
  function x :: xs -> combinations' x xs @ combinations xs | [] -> []

let conn_map connl =
  let update m (a, b) =
    m
    |> StringMap.update a (function
         | None -> Some (StringSet.singleton b)
         | Some set -> Some (set |> StringSet.add b))
  in
  List.fold_left update StringMap.empty connl

let extend z (x, y) = (x, y, z)

let order (x, y, z) =
  [ x; y; z ] |> List.sort compare |> function
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith ""

let in_graph graph (n1, n2, n3) =
  graph |> StringMap.find_opt n1
  |> Option.map (StringSet.find_opt n2)
  |> Option.join |> Option.is_some
  && graph |> StringMap.find_opt n2
     |> Option.map (StringSet.find_opt n3)
     |> Option.join |> Option.is_some
  && graph |> StringMap.find_opt n3
     |> Option.map (StringSet.find_opt n1)
     |> Option.join |> Option.is_some

let solve file =
  let conn =
    file |> read_lines
    |> List.filter (( <> ) "")
    |> List.map parse_line |> List.flatten |> List.sort_uniq compare |> conn_map
  in
  let is_candidate s m =
    s |> String.starts_with ~prefix:"t" && m |> StringSet.cardinal > 1
  in
  let cand = conn |> StringMap.filter is_candidate |> StringMap.to_list in
  cand
  |> List.map (fun (a, t) ->
         t |> StringSet.to_list |> combinations
         |> List.map (extend a)
         |> List.map order)
  |> List.flatten |> List.sort_uniq compare
  |> List.filter (in_graph conn)
  |> List.length
