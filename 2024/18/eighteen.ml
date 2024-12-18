let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let read_problem file =
  let read_int s = s |> String.trim |> int_of_string in
  let read_drop d =
    d |> String.split_on_char ',' |> List.map read_int |> function
    | [ x; y ] -> (x, y)
    | _ -> failwith "parse error"
  in
  read_lines file |> List.map read_drop

let drop map (x, y) = map.(y).(x) <- true
let in_bound (w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

let update_dist dist size (x, y) =
  let update_dist' (x', y') =
    if in_bound size (x', y') then dist.(y').(x') else Int.max_int
  in
  dist.(y).(x) <-
    (update_dist' (x - 1, y)
    |> min (update_dist' (x + 1, y))
    |> min (update_dist' (x, y - 1))
    |> min (update_dist' (x, y + 1)))
    + 1

let discover bytes seen size (x, y) =
  let discover' (x', y') =
    if in_bound size (x', y') && (not bytes.(y').(x')) && not seen.(y').(x')
    then (
      seen.(y').(x') <- true;
      [ (x', y') ])
    else []
  in
  discover' (x - 1, y)
  @ discover' (x + 1, y)
  @ discover' (x, y - 1)
  @ discover' (x, y + 1)

let solve ps size n =
  let h, w = size in
  let bytes = Array.make_matrix h w false in
  let dist = Array.make_matrix h w Int.max_int in
  let seen = Array.make_matrix h w false in
  ps |> List.to_seq |> Seq.take n |> Seq.iter (drop bytes);
  dist.(0).(0) <- 0;
  seen.(0).(0) <- true;
  let stack = ref (discover bytes seen size (0, 0)) in
  while not (List.is_empty !stack) do
    let pos = List.hd !stack in
    stack := List.tl !stack;
    update_dist dist size pos;
    stack := List.append !stack (discover bytes seen size pos)
  done;
  dist.(h - 1).(w - 1)

let bisect min max cond =
  let c_min = cond min in
  let c_max = cond max in
  let rec bisect' min' max' c_min' c_max' =
    assert (min' < max');
    assert (c_min' = true && c_max' = false);
    (* check for solution point *)
    if min' + 1 = max' then max'
    else
      let mid = min' + ((max' - min') / 2) in
      let c_mid = cond mid in
      if c_mid = true then bisect' mid max' c_mid c_max'
      else bisect' min' mid c_min' c_mid
  in
  bisect' min max c_min c_max

let () =
  Sys.argv |> function
  | [| _; file; w; h; n |] ->
      let drops = read_problem file in
      let result =
        solve drops (int_of_string w, int_of_string h) (int_of_string n)
      in
      Printf.printf "%d\n" result;
      let nt =
        bisect 0 (List.length drops) (fun n ->
            solve drops (int_of_string w, int_of_string h) n < Int.max_int)
      in
      let x, y = List.nth drops (nt - 1) in
      Printf.printf "%d,%d\n" x y
  | _ -> failwith "wrong number of arguments"
