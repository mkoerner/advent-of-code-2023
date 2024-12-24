let read_lines file : string list =
  In_channel.with_open_text file In_channel.input_lines

let split_on cond xs =
  let rec loop rh = function
    | [] -> (List.rev rh, [])
    | x :: xs -> if cond x then (List.rev rh, x :: xs) else loop (x :: rh) xs
  in
  loop [] xs

let read_binding line =
  let name = String.sub line 0 3 in
  let value = String.sub line 5 1 in
  (name, value = "1")

type op = And | Or | Xor
type value = Name of string | Value of bool
type expr = op * value * value * value
type binding = string * bool

let op_fun = function And -> ( && ) | Or -> ( || ) | Xor -> ( <> )

let read_expr line : expr =
  let read_op = function
    | "AND" -> And
    | "OR" -> Or
    | "XOR" -> Xor
    | _ -> failwith "parse error"
  in
  line |> String.split_on_char ' ' |> function
  | [ lhs; op; rhs; "->"; out ] -> (op |> read_op, Name lhs, Name rhs, Name out)
  | _ -> failwith "parse error"

let sub_lhs ((n, b) : binding) = function
  | op, Name n', rhs, out when n' = n -> (op, Value b, rhs, out)
  | e -> e

let sub_rhs ((n, b) : binding) = function
  | op, lhs, Name n', out when n' = n -> (op, lhs, Value b, out)
  | e -> e

let subs' (es : expr list) (b : binding) =
  es |> List.map (fun e -> e |> sub_lhs b |> sub_rhs b)

(* substitute bindings in expressions *)
let subs (es : expr list) (bs : binding list) =
  bs |> List.fold_left (fun e b -> subs' e b) es

let eval' : expr -> expr * binding list = function
  | op, Value l, Value r, Name out ->
      let b = (op_fun op) l r in
      ((op, Value l, Value r, Value b), [ (out, b) ])
  | e -> (e, [])

(** evaluate expressions and replace names with values in output. Returns
    updated expressions and new bindings *)
let eval (es : expr list) =
  let es', bs = es |> List.map eval' |> List.split in
  (es', bs |> List.concat)

let reverse a b = -1 * compare a b

let rec to_decimal ?(acc = 0) = function
  | b :: bs ->
      let acc' = (2 * acc) + if b then 1 else 0 in
      to_decimal ~acc:acc' bs
  | [] -> acc

let solve file =
  let binds', exprs' = file |> read_lines |> split_on (( = ) "") in
  let newbs = ref (binds' |> List.map read_binding) in
  let binds = ref !newbs in
  let exprs = ref (exprs' |> List.tl |> List.map read_expr) in
  while !newbs |> List.is_empty |> not do
    let exprs' = subs !exprs !newbs in
    let exprs'', newbs' = eval exprs' in
    exprs := exprs'';
    newbs := newbs';
    binds := !binds @ !newbs
  done;
  !binds
  |> List.filter (fun b -> b |> fst |> String.starts_with ~prefix:"z")
  |> List.sort reverse |> List.split |> snd |> to_decimal
