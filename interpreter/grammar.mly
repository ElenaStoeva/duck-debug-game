%{
  open Ast
%}

%token MOVE
%token RIGHT
%token LEFT
%token COLOR
%token DEFINE
%token APPLY
%token FUNC
%token EOF

%nonassoc DEFINE

%start <Ast.expr> prog

/* %%

prog:
| e = expr; EOF { e }

expr:
| 
| */


