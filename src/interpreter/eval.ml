
exception Empty_stream

exception Malformed_stream

type prim_move =
  | M
  | R
  | L
  | C of int

type move_stream = Cons of Ast.command * move_stream Lazy.t | End

type env = (string * Ast.command list) list

let parse (s: string) : Ast.program =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let empty = End

let prim_of x = match x with
  | Ast.Move -> M
  | Ast.Right -> R
  | Ast.Left -> L
  | Ast.Color (i) -> C(i)
  | _ -> failwith "There is no primitive of that value."

let rec prepend env (lst : Ast.command list) str =
  match lst with
  | [] -> str
  | App (id) :: t -> prepend env (List.assoc id env) (prepend env t str)
  | h::t -> Cons(h, lazy (prepend env t str))

let rec list_to_stream env lst = prepend env lst End

let hd str = match str with 
  | End -> raise Empty_stream
  | Cons (App(_),_) -> raise Malformed_stream
  | Cons (x,_) -> prim_of x

let tl str = match str with 
  | End -> raise Empty_stream
  | Cons (_,t) -> Lazy.force t

let rec defs_to_env ds = 
  match ds with
  | [] -> []
  | Ast.Def(d,e)::t -> (d,e)::(defs_to_env t)

(* TODO: Func to check infinite mutual recursion in function applications. and unrecognized color *)
let rec app_to_lst x acc =
  match x with
  | [] -> acc
  | Ast.App (id)::t -> app_to_lst t (id::acc)
  | _::t -> app_to_lst t acc

let rec def_to_lst ds acc =
  match ds with
  | [] -> acc
  | Ast.Def (id,_)::t -> def_to_lst t (id::acc)

let has_def (Ast.Prog(ds, x)) =
  let app_lst = app_to_lst x [] in
  let def_lst = def_to_lst ds [] in
  let ast_ok = List.fold_left 
      (fun bool elt -> bool && (List.mem elt def_lst)) true app_lst in
  if ast_ok then Ast.Prog(ds, x)
  else failwith "All function applications are not defined."

(** precondition: correct program. "f = [f]; [f]" not allowed. *)
let init_stream prog = 
  match prog with
  | Ast.Prog ([], x) -> list_to_stream [] x
  | Ast.Prog (ds, x) -> list_to_stream (defs_to_env ds) x

let rec take_aux n x lst = 
  if n = 0 then lst
  else match x with
    | End -> lst
    | Cons(h,t) -> take_aux (n-1) (Lazy.force t) ((prim_of h)::lst) 

let take n s = List.rev (take_aux n s [])