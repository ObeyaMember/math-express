open MathExpress.Lexer;;
(* let my_expression = "152 * 13 + 7987" *)
(* let tokenized_expression = tokenize my_expression;; *)
let tokenized_list = tokenize (explode "2 * 5 / 10") in
(* print_token_list  tokenized_list; *)
print_endline (string_of_int (parse tokenized_list))
