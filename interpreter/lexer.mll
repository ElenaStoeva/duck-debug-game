{
  open Parser
}

let whitespace = [' ' '\t']+
let func = ['a'-'z']
let color = ['0'-'9']
(* let app = [] *)

rule read =
  parse
  | whitespace { read lexbuf }
  | func { FUNC }
  | color { COLOR }
  | app { APPLY }
  | "=" { DEFINE }
  | "M" { MOVE }
  | "R" { RIGHT }
  | "L" { LEFT }
  | eof { EOF }