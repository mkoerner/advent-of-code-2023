let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

type pos = int * int
type map = char array array
type direction = R | L | U | D
type score = int
type state = score * pos * direction
type state_list = state list

let read_map (smap : string list) =
  let s = ref (0, 0) in
  let e = ref (0, 0) in
  let smap' = smap |> List.filter (fun l -> l != "") in
  let h = smap' |> List.length in
  let w = smap' |> List.hd |> String.length in
  let map = Array.make_matrix h w ' ' in
  let vis = Array.make_matrix h w false in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let c = String.get (List.nth smap i) j in
      if c = 'S' then s := (i, j) else ();
      if c = 'E' then e := (i, j) else ();
      map.(i).(j) <- (if c = '#' then '#' else '.')
    done
  done;
  (map, !s, !e, vis)

(* simple heap based on sorted lists *)
let cmp (a, _, _) (b, _, _) = compare a b
let top (sl : state_list) = List.hd sl
let pop (sl : state_list) = List.tl sl
let ins (s : state_list) (sl : state_list) : state_list = List.merge cmp s sl
let pos_of (_, p, _) = p
let turns = function R -> (U, D) | L -> (U, D) | U -> (R, L) | D -> (R, L)

let go (i, j) = function
  | R -> (i, j + 1)
  | L -> (i, j - 1)
  | U -> (i - 1, j)
  | D -> (i + 1, j)

let get map (i, j) = map.(i).(j)

let gen_moves (map : map) vis (state : state) : state_list =
  let s, p, d = state in
  vis.(fst p).(snd p) <- true;
  let l, r = turns d in
  let m_fwd = if get map (go p d) = '.' && not (get vis (go p d)) then [ (s + 1, go p d, d) ] else [] in
  let m_l = if get map (go p l) = '.' && not (get vis (go p l)) then [ (s + 1000, p, l) ] else [] in
  let m_r = if get map (go p r) = '.' && not (get vis (go p r)) then [ (s + 1000, p, r) ] else [] in
  m_fwd @ m_l @ m_r

let run_on file =
  let lines = read_lines file in
  let map, s, e, vis = read_map lines in
  let sl = ref [ (0, s, R) ] in
  let next = ref (top !sl) in
  sl := pop !sl;
  while pos_of !next <> e do
    let sl' = gen_moves map vis !next in
    sl := ins sl' !sl;
    next := top !sl;
    sl := pop !sl;
  done;
  let score, _, _ = !next in
  Printf.printf "%d\n" score

  let () = run_on Sys.argv.(1)