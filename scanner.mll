(* Ocamllex scanner for See++ *)
(*     Authors: TODO *) 

{ open Parserseepp }

let digit = ['0' - '9']
let digits = digit+
let append = "-> append()"
let appendCircle = "-> append().circle"

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { single lexbuf }            (* Single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "char"   { CHAR }
| "String" { STRING }
| "Point"  { POINT }
| "Pixel"  { PIXEL }
| "Circle" { CIRCLE }
| "Canvas" { CANVAS }
| "CanvasCircle" { CANVASCIRCLE }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| append   { SHOEHORN }
| appendCircle { SHOEHORNCIRCLE}
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| ''' (_ as ch) ''' { CHAR_LITERAL(ch) }
| '"' ([^ '"']* as str) '"' { STRING_LITERAL(str) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and single = parse
  '\n' { token lexbuf }
| _    { single lexbuf }
