(** Semantic checking module to enforce program correctness. *)

(** Raised if function applied has no definition. *)
exception Undefined_function

(** Raised if the color command is not valid. *)
exception Undefined_color

(** [check_ast prog] is prog after performing semantic checking.
    Raises [Undefined_function] is there is an unbound function application.
    Raises [Undefined_color] if the color command used is not defined. *)
val check_ast : Ast.program -> Ast.program
