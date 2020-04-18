open Ast

let parse (s: string) : program =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf
