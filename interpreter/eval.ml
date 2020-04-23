open Ast

let parse (s: string) : program =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf


type prim_move =
  | M
  | R
  | L
  | C of int

type move_stream = Cons of command * move_stream Lazy.t | End

type env = (string * command list) list

let empty = End

let rec prepend env (lst : command list) str =
  match lst with
  | [] -> str
  | App (id) :: t -> prepend env (List.assoc id env) (prepend env t str)
  | h::t -> Cons(h, lazy (prepend env t str))

let rec list_to_stream env lst = prepend env lst End

let hd str = match str with 
  | End -> failwith ("Empty stream, cannot apply hd. ")
  | Cons (Move,_) -> M
  | Cons (Right,_) -> R
  | Cons (Left,_) -> L
  | Cons (Color(i),_) -> C (i)
  | _ -> failwith ("Error function application. ")

let tl str = match str with 
  | End -> failwith ("Empty stream, cannot apply tl. ")
  | Cons (_,t) -> Lazy.force t

let rec defs_to_env ds = 
  match ds with
  | [] -> []
  | Def(d,e)::t -> (d,e)::(defs_to_env t)

(* TODO: Func to check infinite mutual recursion in function applications. *)
let rec app_to_lst x acc =
  match x with
  |[] -> acc
  |App (id)::t -> app_to_lst t (id::acc)
  |_::t -> app_to_lst t acc

let rec def_to_lst ds acc =
  match ds with
  |[] -> acc
  |Def (id,_)::t -> def_to_lst t (id::acc)

let has_def (Prog(ds, x)) =
  let app_lst = app_to_lst x [] in
  let def_lst = def_to_lst ds [] in
  let ast_ok = List.fold_left 
      (fun bool elt -> bool && (List.mem elt def_lst)) true app_lst in
  if ast_ok then Prog(ds, x)
  else failwith "All function applications are not defined."

(** precondition: correct program. "f = [f]; [f]" not allowed. *)
let init_stream prog = 
  match prog with
  | Prog ([], x) -> list_to_stream [] x
  | Prog (ds, x) -> list_to_stream (defs_to_env ds) x

let rec take_aux n x lst = 
  if n = 0 then lst
  else match x with
    | End -> lst
    | Cons(h,t) -> take_aux (n-1) (Lazy.force t) (h::lst) 

let take n s = List.rev (take_aux n s [])