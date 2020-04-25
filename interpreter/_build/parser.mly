%{
  open Ast
%}

%token MOVE
%token RIGHT
%token LEFT
%token <int> COLOR
%token DEFINE
%token <string> FUNC
%token EOF
%token LEFTBRAKET
%token RIGHTBRAKET
%token ENDDEF

%start <Ast.program> prog

%%

prog:
  | ds = definition*; cs = command*; EOF { Prog (ds, cs) }
  ;

definition:
  | id = FUNC; DEFINE; cs = command*; ENDDEF { Def (id, cs) }
  ;

command:
  | MOVE { Move }
  | RIGHT { Right }
  | LEFT { Left }
  | c = COLOR { Color (c) }
  | LEFTBRAKET; id = FUNC; RIGHTBRAKET { App (id) }
  ;
