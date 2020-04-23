{
  open Parser
}

let whitespace = [' ' '\t']+
let func = ['a'-'z']
let color = ['0'-'9']

rule read =
  parse
  | whitespace { read lexbuf }
  | func { FUNC (Lexing.lexeme lexbuf) }
  | color { COLOR (int_of_string (Lexing.lexeme lexbuf)) }
  | "=" { DEFINE }
  | "[" { LEFTBRAKET }
  | "]" { RIGHTBRAKET}
  | "M" { MOVE }
  | "R" { RIGHT }
  | "L" { LEFT }
  | ";" { DEFEND }
  | eof { EOF }
