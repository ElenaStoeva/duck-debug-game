(** Evaluation interface for interpreting the game language. *)

(** Raised if operation does not apply to an empty stream. *)
exception Empty_stream

(** Raised if stream has non-evaluated values. *)
exception Malformed_stream

(** Raised if function applied has no definition. *)
exception Undefined_function

(** Raised if the color command is not valid. *)
exception Undefined_color

(** Type of a primitive move. *)
type prim_move =
  | M
  | R
  | L
  | C of int

(** Type of an possibly infinite lazy primitive move stream. *)
type move_stream = Cons of Ast.command * move_stream Lazy.t | End

(** [parse s] is the abstract syntax tree created after parsing [s]. *)
val parse : string -> Ast.program

(** [empty] is an empty move stream. *)
val empty : move_stream

(** [hd m] is the next element of move stream [m]. 
    Raises [Empty_stream] is [m] if empty and raises [Malformed_stream] if
    [m]] has non-evaluated values. *)
val hd : move_stream -> prim_move

(** [hd m] is the remaining move stream after removing the first element of 
    move stream [m]. Raises [Empty_stream] is [m] is empty. *)
val tl : move_stream -> move_stream

(** [take n m] is the list of first [n] primitive moves in move stream [m]. *)
val take : int -> move_stream -> prim_move list

(** [init_stream p] is the move stream created by evaluation program [p].
    Requires that [p] passes [check_ast]. *)
val init_stream : Ast.program -> move_stream

(** [check_ast prog] is prog after performing semantic checking.
    Raises [Undefined_function] is there is an unbound function application.
    Raises [Undefined_color] if the color command used is not defined. *)
val check_ast : Ast.program -> Ast.program

