exception Undefined_function

exception Undefined_color

(** [app_to_lst x a] is [a] prepended with function identifiers in [x]. *)
let rec app_to_lst x acc =
  match x with
  | [] -> acc
  | Ast.ConstApp (id)::t -> app_to_lst t (id::acc)
  | Ast.FunApp (id,_)::t -> app_to_lst t (id::acc)
  | _::t -> app_to_lst t acc

(** [def_to_lst d a aa] is [a,aa] where [a] is prepended with function 
    identifiers defined in [d] and [aa] prepended with function identifiers
    in function definitions in [d]. *)
let rec def_to_lst ds acc acc2 =
  match ds with
  | [] -> acc,acc2
  | Ast.Def(id,_,cs)::t -> def_to_lst t (id::acc) ((app_to_lst cs []) @ acc2)
  | Ast.Const(id,cs)::t -> def_to_lst t (id::acc) ((app_to_lst cs []) @ acc2)

(** [has_def p] is [p] if all function applications in [p] are defined, 
    else raises exception. Raises [Undefined_function] if a applied function
    is not defined. *)
let has_fun_def (Ast.Prog(ds, x)) =
  let app_temp = app_to_lst x [] in
  let def_lst, app_temp2 = def_to_lst ds [] [] in
  let app_lst = app_temp @ app_temp2 in
  let ast_ok = List.fold_left 
      (fun bool elt -> bool && (List.mem elt def_lst)) true app_lst in
  if ast_ok then Ast.Prog(ds, x)
  else raise Undefined_function

(** [has_color p] is [p] if all color commands are valid in the function
    definitions and the main command list. 
    Raises [Undefined_color] if not. *)
let has_color (Ast.Prog(ds, x)) =
  let f = function Ast.Color(i) -> 1 <= i && i <= 3 | _ -> true in
  if List.for_all f x 
  then Ast.Prog(ds, x)
  else if begin
    let lst d = match d with Ast.Const(_,cs) -> cs | Ast.Def(_,_,cs) -> cs in
    List.for_all (fun d -> List.for_all f (lst d)) ds end 
  then Ast.Prog(ds, x)
  else raise Undefined_color

(* TODO: Func to check infinite mutual recursion in function applications. *)

(* let has_var_def = failwith "Unimplemented" *)

(** [check_ast prog] is prog after performing semantic checking.
    Raises [Undefined_function] is there is an unbound function application.
    Raises [Undefined_color] if the color command used is not defined. *)
let check_ast prog = prog |> has_fun_def |> has_color