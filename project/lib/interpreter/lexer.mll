{
  open Parser
}

let whitespace = [' ' '\t']+
let id = ['a'-'z']
let color = ['0'-'9']

rule read =
  parse
  | whitespace { read lexbuf }
  | id { ID (Lexing.lexeme lexbuf) }
  | color { COLOR (int_of_string (Lexing.lexeme lexbuf)) }
  | "=" { DEFINE }
  | "[" { LEFTBRAKET }
  | "]" { RIGHTBRAKET}
  | "M" { MOVE }
  | "R" { RIGHT }
  | "L" { LEFT }
  | "-" { VARSEP }
  | ";" { ENDDEF }
  | eof { EOF }
