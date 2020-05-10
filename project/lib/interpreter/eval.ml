
exception Empty_stream

exception Malformed_stream

type prim_move =
  | M
  | R
  | L
  | C of int

type move_stream = Cons of Ast.command * move_stream Lazy.t | End

(** [env] is the type of definition environment for functions.
    [(f, vs, cs)] represents the function with identifier [f], variables 
    names [vs], and commands [cs]. *)
type env = (string * string list * Ast.command list) list

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

(** [insert_command var arg cs acc] is the list of commands after the variable 
    with identifier [var] is substituted with arguments [arg] in commands [cs]
    and added to [acc] in reverse order. *)
let rec insert_command
    (var : string)
    (arg : Ast.command list)
    (cs : Ast.command list)
    (acc : Ast.command list) : Ast.command list = 
  match cs with
  | [] -> acc
  | Ast.VarApp(i)::t -> begin
      if i = var then insert_command var arg t (List.rev (arg) @ acc)
      else insert_command var arg t (VarApp(i)::acc)
    end
  | Ast.FunApp(id, cslst)::t -> begin
      let f = fun cs' -> (List.rev (insert_command var arg cs' [])) in
      let cslst' = List.map f cslst in
      let acc' = Ast.FunApp(id, cslst') :: acc in
      insert_command var arg t acc'
    end
  | h::t -> insert_command var arg t (h::acc)


(** [substitute env id cs] is the list of commands that the function with 
    identifier [id] is called with arguments [cs] using the definition in
    environment [env]. Requires that the number of required variables match
    in number. *)
let substitute 
    (env : env)
    (id : string)
    (clst : Ast.command list list) : Ast.command list =
  let _,vs,cs = List.find (fun (i,_,_) -> i = id) env in
  let rec subs_aux vs clst cs =
    match vs, clst with
    | [],[] -> cs
    | hv::tv, hcl::tcl -> begin
        insert_command hv hcl cs [] |> List.rev |> subs_aux tv tcl
      end
    | _ -> failwith "number of arguments dont match" in
  subs_aux vs clst cs

(** [preprend env lst str] is the move stream with commands in [lst] prepended
    to [str]. Substitutes a function definition if the command is a function 
    application from the environment [env]. 
    Requires that each function is defined. *)
let rec prepend 
    (env : env) 
    (lst : Ast.command list)
    (str : move_stream) : move_stream =
  match lst with
  | [] -> str
  | FunApp (id, clst) :: t -> begin match substitute env id clst with
      | [] -> prepend env t str
      | FunApp (i,c) :: tt -> prepend env (FunApp (i,c) :: tt) str
      | VarApp (_) :: _ -> failwith "variables are only allowed in definions"
      | hh :: tt -> Cons(hh, lazy (prepend env (tt @ t) str))
    end
  | VarApp (_) :: _ -> failwith "variables are only allowed in definions"
  | h::t -> Cons(h, lazy (prepend env t str))

(** [list_to_stream e l] is the move stream of the commands in [lst]. It 
    consists of only primitive commands, substituting function application
    using environment [env]. Requires that each function is defined. *)
let list_to_stream (env : env) (lst : Ast.command list) : move_stream =
  prepend env lst End

let hd str = match str with 
  | End -> raise Empty_stream  
  | Cons (VarApp(_),_) -> raise Malformed_stream
  | Cons (FunApp(_),_) -> raise Malformed_stream
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

(** [vars_to_lst x a] is the variable identifiers in [x] added to [a] in
    reverse order. *)
let rec vars_to_lst x acc =
  match x with
  | [] -> acc
  | Ast.Var (id)::t -> vars_to_lst t (id::acc)

(** [defs_to_env ds] is the environment that includes the function definitions
    in [ds]. *)
let rec defs_to_env ds : (string * string list * Ast.command list) list =
  match ds with
  | [] -> []
  | Ast.Def(id,vs,cs)::t -> begin 
      (id, (List.rev(vars_to_lst vs [])), cs) :: (defs_to_env t)
    end

let init_stream prog = match prog with 
  | Ast.Prog (ds, x) -> list_to_stream (defs_to_env ds) x