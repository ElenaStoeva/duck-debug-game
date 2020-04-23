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
and list_to_stream (env : env) = function
  | [] -> End
  | App (id) :: t -> prepend env (List.assoc id env) (list_to_stream env t)
  | h :: t -> Cons (h, lazy (list_to_stream env t))

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

(** precondition: correct program *)
let init_stream prog = 
  match prog with
  | Prog ([], x) -> list_to_stream [] x
  | Prog (ds, x) -> list_to_stream (defs_to_env ds) x