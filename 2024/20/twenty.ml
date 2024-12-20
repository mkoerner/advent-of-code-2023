let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let read_map (smap : string list) =
  let s = ref (0, 0) in
  let e = ref (0, 0) in
  let smap' = smap |> List.filter (fun l -> l != "") in
  let h = smap' |> List.length in
  let w = smap' |> List.hd |> String.length in
  let map = Array.make_matrix h w ' ' in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let c = String.get (List.nth smap i) j in
      if c = 'S' then s := (i, j) else ();
      if c = 'E' then e := (i, j) else ();
      map.(i).(j) <- (if c = '#' then '#' else '.')
    done
  done;
  (map, (h, w), !s, !e)

let size map = (map |> Array.length, map.(0) |> Array.length)
let in_bounds (h, w) (i, j) = j >= 0 && j < w && i >= 0 && i < h
let up ?(d = 1) (i, j) = (i - d, j)
let down ?(d = 1) (i, j) = (i + d, j)
let left ?(d = 1) (i, j) = (i, j - d)
let right ?(d = 1) (i, j) = (i, j + d)

let get map def pos =
  if in_bounds (size map) pos then
    let i, j = pos in
    map.(i).(j)
  else def

let step map dis pos =
  let u = up pos in
  let d = down pos in
  let l = left pos in
  let r = right pos in
  let pos' =
    if get map '#' u <> '#' && get dis None u |> Option.is_none then u
    else if get map '#' d <> '#' && get dis None d |> Option.is_none then d
    else if get map '#' l <> '#' && get dis None l |> Option.is_none then l
    else if get map '#' r <> '#' && get dis None r |> Option.is_none then r
    else failwith "unexpected"
  in
  let d = get dis None pos |> Option.get |> fun x -> x + 1 in
  dis.(fst pos').(snd pos') <- Some d;
  pos'

let shortcuts dis pos =
  let c = get dis None pos |> Option.get in
  let get df =
    let v = get dis None (df pos) in
    if v |> Option.is_some then
      let saved = c - Option.get v in
      if saved > 2 then [ (pos, df pos, c - Option.get v - 2) ] else []
    else []
  in
  get (up ~d:2) @ get (down ~d:2) @ get (left ~d:2) @ get (right ~d:2)

let go file =
  let map, size, s, e =
    file |> read_lines |> List.filter (fun l -> l <> "") |> read_map
  in
  let dis = Array.make_matrix (fst size) (snd size) None in
  dis.(fst s).(snd s) <- Some 0;
  let curr = ref s in
  let saved = ref [] in
  while !curr <> e do
    curr := step map dis !curr;
    saved := !saved @ shortcuts dis !curr
  done;
  !saved

let () =
  Sys.argv |> function
  | [| _; file; t |] ->
      let saved = go file in
      let threshold = int_of_string t in
      saved
      |> List.filter (fun (_, _, s) -> s >= threshold)
      |> List.length |> Printf.printf "%d\n"
  | _ -> failwith "argument error"
