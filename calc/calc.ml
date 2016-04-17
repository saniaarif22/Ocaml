open Ast

let rec eval table = function
Lit x -> x
| Binop(e1, op, e2) ->
let v1 = eval table e1 in 
let v2 = eval table e2 in
(match op with
Add -> v1 + v2
| Sub -> v1 - v2
| Mul -> v1 * v2
| Div -> v1 / v2)
| Var v -> table.(v)
| Asn (i, j) -> Array.set table i (eval table j);
    eval table j
| Seq (e1, e2) -> let _ = eval table e1 in
    eval table e2
;;
let _ =
let lexbuf = Lexing.from_channel stdin in
let expr = Parser.expr Scanner.token lexbuf in
let result = eval (Array.make 26 0) expr in
print_endline (string_of_int result)
;;