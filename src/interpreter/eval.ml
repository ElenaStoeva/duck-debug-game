
exception Empty_stream

exception Malformed_stream

exception Undefined_function

exception Undefined_color

type prim_move =
  | M
  | R
  | L
  | C of int

type move_stream = Cons of Ast.command * move_stream Lazy.t | End

(** [env] is the type of definition environment for functions. *)
type env = (string * Ast.command list) list

let parse (s: string) : Ast.program =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let empty = End

(** [prim_of x] is the primitive command equivalent of a command.
    Requires that [x] is not an application. *)
let prim_of x = match x with
  | Ast.Move -> M
  | Ast.Right -> R
  | Ast.Left -> L
  | Ast.Color (i) -> C(i)
  | _ -> failwith "There is no primitive of that value."

(** [preprend env lst str] is the move stream with commands in [lst] prepended
    to [str]. Substitutes a function definition if the command is [App(_)] from
    the environment [env]. *)
let rec prepend env (lst : Ast.command list) str =
  match lst with
  | [] -> str
  | App (id) :: t -> prepend env (List.assoc id env) (prepend env t str)
  | h::t -> Cons(h, lazy (prepend env t str))

(** [list_to_stream e l] is the move stream of the commands in [lst]. It 
    consists of only primitive commands, substituting function application
    using environment [env]. *)
let rec list_to_stream env lst = prepend env lst End

let hd str = match str with 
  | End -> raise Empty_stream
  | Cons (App(_),_) -> raise Malformed_stream
  | Cons (x,_) -> prim_of x

let tl str = match str with 
  | End -> raise Empty_stream
  | Cons (_,t) -> Lazy.force t

(** [take_aux n x l] is the first [n] elements of [x] prepended to [lst] in
    reverse order. *)
let rec take_aux n x lst = 
  if n = 0 then lst
  else match x with
    | End -> lst
    | Cons(h,t) -> take_aux (n-1) (Lazy.force t) ((prim_of h)::lst) 

let take n s = List.rev (take_aux n s [])

(** [defs_to_env ds] is the environment that includes the function definitions
    in [ds]. *)
let rec defs_to_env ds = 
  match ds with
  | [] -> []
  | Ast.Def(d,e)::t -> (d,e)::(defs_to_env t)

(** [app_to_lst x a] is [a] prepended with function identifiers in [x]. *)
let rec app_to_lst x acc =
  match x with
  | [] -> acc
  | Ast.App (id)::t -> app_to_lst t (id::acc)
  | _::t -> app_to_lst t acc

(** [def_to_lst d a aa] is [a,aa] where [a] is prepended with function 
    identifiers defined in [d] and [aa] prepended with function identifiers
    in function definitions in [d]. *)
let rec def_to_lst ds acc acc2 =
  match ds with
  | [] -> acc,acc2
  | Ast.Def (id,cs)::t -> def_to_lst t (id::acc) ((app_to_lst cs []) @ acc2)

(** [has_def p] is [p] if all function applications in [p] are defined, 
    else raises exception. Raises [Undefined_function] if a applied function
    is not defined. *)
let has_def (Ast.Prog(ds, x)) =
  let app_temp = app_to_lst x [] in
  let def_lst, app_temp2 = def_to_lst ds [] [] in
  let app_lst = app_temp @ app_temp2 in
  let ast_ok = List.fold_left 
      (fun bool elt -> bool && (List.mem elt def_lst)) true app_lst in
  if ast_ok then Ast.Prog(ds, x)
  else raise Undefined_function

(** [has_color p] is [p] if all color commands are valid. Raises 
    [Undefined_color] if not. *)
let has_color (Ast.Prog(ds, x)) =
  let f = function Ast.Color(i) -> 0 <= i && i <= 2 | _ -> true in
  if List.for_all f x then Ast.Prog(ds, x)
  else raise Undefined_color

(* TODO: Func to check infinite mutual recursion in function applications. 
   And unrecognized color *)

(** [check_ast prog] is prog after performing semantic checking.
    Raises [Undefined_function] is there is an unbound function application.
    Raises [Undefined_color] if the color command used is not defined. *)
let check_ast prog = prog |> has_def |> has_color

let init_stream prog = 
  match prog with
  | Ast.Prog ([], x) -> list_to_stream [] x
  | Ast.Prog (ds, x) -> list_to_stream (defs_to_env ds) x