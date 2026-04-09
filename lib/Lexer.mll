{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let number = digit+
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  | "*" { MULT }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | "fst" { FST }
  | "snd" { SND }
  | "match" { MATCH }
  | "with" { WITH }
  | "sum" { SUM }
  | "to" { TO }
  | "->" { ARR_TO }
  | number { INT (Lexing.lexeme lexbuf
                  |> int_of_string) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | "," { COMMA }
  | "(" { LPAR }
  | ")" { RPAR }
  | eof { EOF }
