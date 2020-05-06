%{
  open Ast
%}

%token MOVE
%token RIGHT
%token LEFT
%token <int> COLOR
%token DEFINE
%token <string> ID
%token EOF
%token LEFTBRAKET
%token RIGHTBRAKET
%token VARSEP
%token ENDDEF

%start <Ast.program> prog

%%

prog:
  | ds = definition*; cs = command*; EOF { Prog (ds, cs) }
  ;

definition:
  | id = ID; DEFINE; cs = command*; ENDDEF { Const (id, cs) }
  | id = ID; vs = variable*; DEFINE; cs = command*; ENDDEF { Def (id, vs, cs) }
  ;

variable:
  | VARSEP; id = ID; { Var (id) }
  ;

apps:
  | VARSEP; cs = command* { cs }
  ;

command:
  | MOVE { Move }
  | RIGHT { Right }
  | LEFT { Left }
  | c = COLOR { Color (c) }
  | LEFTBRAKET; id = ID; RIGHTBRAKET { ConstApp (id) }
  | LEFTBRAKET; id = ID; args = apps*; RIGHTBRAKET { FunApp (id, args) }
  | id = ID { VarApp (id) }
  ;