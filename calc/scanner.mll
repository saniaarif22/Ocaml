{ open Parser }

let letter = ['a'-'z']
oca
rule token =
parse 
| [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { EQUALS }
| ',' { COMMA }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| letter as lit { VARIABLE(int_of_char lit - 97) }
| eof { EOF }