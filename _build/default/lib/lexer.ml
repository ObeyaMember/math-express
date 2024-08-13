type token = 
  | TokenInt of string
  | BinaryOper of string
  | TokenSpace
  | TokenNone

  

let rec explode s = 
  if String.length s = 0 then []
  else if String.length s = 1 then [String.make 1 s.[0]] else (String.make 1 s.[0])::explode (String.sub s 1 (String.length s - 1))

let rec print_token_list l = 
  
  match l with
  | [] -> print_endline "Empty or End"
  | hd::tl -> 
    match hd with
    | TokenInt value -> print_string ("TokenInt " ^ value ^ "; "); print_token_list tl
    | BinaryOper oper -> print_string ("BinaryOper " ^ oper ^ "; "); print_token_list tl
    | TokenSpace -> print_string ("TokenSpace" ^ "; "); print_token_list tl
    | TokenNone -> print_string ("TokenNone" ^ "; "); print_token_list tl
    (* | _ -> print_string ("Anormal" ^ "; "); print_token_list tl;; *)
    (* print_string "helo agaib";; *)
      

let get_token a = 
  match a with
  | "1" -> (* print_endline a; *)TokenInt a
  | "2" -> (* print_endline a; *)TokenInt a
  | "3" -> (* print_endline a; *)TokenInt a
  | "4" -> (* print_endline a; *)TokenInt a
  | "5" -> (* print_endline a; *)TokenInt a
  | "6" -> (* print_endline a; *)TokenInt a
  | "7" -> (* print_endline a; *)TokenInt a
  | "8" -> (* print_endline a; *)TokenInt a
  | "9" -> (* print_endline a; *)TokenInt a
  | "0" -> (* print_endline a; *)TokenInt a
  | "+" -> (* print_endline a; *)BinaryOper a
  | "-" -> (* print_endline a; *)BinaryOper a
  | "*" -> (* print_endline a; *)BinaryOper a
  | "/" -> (* print_endline a; *)BinaryOper a 
  | " " -> (* print_endline "Space"; *)TokenSpace
  | _ -> raise (Failure "Invalid token given.")

let is_token_int toke = 
  match toke with 
  | TokenInt _ -> true
  | _ -> false

let get_toke_int_val_int toke = (* as integer *)
  match toke with
  | TokenInt value -> int_of_string value
  | _ -> raise (Failure "Non-integer token given to get_toke_int_val_int")

let multiply_tokens_int t1 t2 = get_toke_int_val_int t1 * get_toke_int_val_int t2
let divide_tokens_int t1 t2 = get_toke_int_val_int t1 / get_toke_int_val_int t2



let tokenize expression = 
  (* let exploded_expr = (explode expression) in *)

  let rec aux tokenAcc = function
    | [] -> (match tokenAcc with | TokenNone -> [] | _-> [tokenAcc])
    | hd::tl -> 
      match tokenAcc with
      | TokenNone -> aux (get_token hd) tl
      | TokenInt value -> if (get_token hd) = TokenInt hd then aux (TokenInt (value ^ hd)) tl else tokenAcc::aux (get_token hd) tl
      | BinaryOper _-> if (get_token hd) = BinaryOper hd then raise (Failure "Consec operators!") else tokenAcc::aux (get_token hd) tl
      | TokenSpace -> aux (get_token hd) tl
  in (aux TokenNone expression)

(* let parse token_list = 

  let rec aux term oper = function 
    | [] -> 0s
    | hd::tl -> 
      match term with
      | TokenNone -> 
        match oper with 
        | TokenNone -> match hd with | TokenInt _ -> aux hd TokenNone tl | BinaryOper _ -> raise (Failure "shouldnt cuz binary") | _ -> raise (Failure "unexprected")
        | BinaryOper "+" -> match hd with  | TokenInt _ -> aux hd oper tl | BinaryOper _ -> raise (Failure "WHY1") | _ -> raise (Failure "HMM1")
        | BinaryOper "-" -> match hd with  | TokenInt _ -> aux hd oper tl | BinaryOper _ -> raise (Failure "WHY2") | _ -> raise (Failure "HMM2")
        | BinaryOper "*" -> match hd with  | TokenInt _ -> aux hd oper tl | BinaryOper _ -> raise (Failure "WHY3") | _ -> raise (Failure "HMM3") 
        | BinaryOper "/" -> match hd with  | TokenInt _ -> aux hd oper tl | BinaryOper _ -> raise (Failure "WHY4") | _ -> raise (Failure "HMM4") 
        | TokenSpace -> raise (Failure "NOO space")
        | TokenInt _ -> raise (Failure "NO ints allowed on oper argument aux func")
      | TokenInt value -> 
        match oper with 
        | TokenNone -> raise (Failure "Consecutive numbers with no operator.")
        | BinaryOper "+" -> match hd with | TokenInt _-> int_of_string value + aux hd TokenNone tl | _ -> raise (Failure "int needed after operator!")
        | BinaryOper "-" -> match hd with | TokenInt _ -> int_of_string value - aux hd TokenNone tl | _ -> raise (Failure "int needed after operator!")
        | BinaryOper "*" -> match hd with | TokenInt value2 -> aux (TokenInt (string_of_int ((int_of_string value) * (int_of_string value2)))) TokenNone tl | _ -> raise (Failure "int needed after operator!")
        | BinaryOper "/" -> match hd with | TokenInt value2 -> aux (TokenInt (string_of_int ((int_of_string value) / (int_of_string value2)))) TokenNone tl | _ -> raise (Failure "int needed after operator!")
        | _ -> raise (Failure "aux func took non operaot as oper argument")
      | _ -> raise (Failure "aux func took non int as term argument")
      in aux TokenNone TokenNone token_list *)


let parse token_list = 

  let rec aux kept = function
  | [] -> (match kept with | TokenInt value -> int_of_string value | _ -> raise (Failure "Last token is not an int"))
  | last::[] -> (match kept with | TokenNone -> (match last with | TokenInt _  -> aux last [] | _-> raise (Failure "why?")) | _ -> raise (Failure "Consec token at end"))
  | hd::tl -> (
    match kept with
    | TokenNone -> (match hd with | TokenInt _ -> aux hd tl | _ -> raise (Failure "Expected int token in rec"))
    | TokenInt value -> (if hd = BinaryOper "+" then (int_of_string value) + aux TokenNone tl else
                          if hd = BinaryOper "-" then (int_of_string value) - aux TokenNone tl else
                            if hd = BinaryOper "*" then 
                              (match tl with 
                              | [] -> raise (Failure "Oper last") 
                              | h::t -> 
                                (match h with 
                                | TokenInt _ -> aux (TokenInt (string_of_int(multiply_tokens_int kept h))) t 
                                | _ -> raise (Failure "Needed int for *"))) else
                                  if hd = BinaryOper "/" then 
                                    (match tl with 
                                    | [] -> raise (Failure "Oper last") 
                                    | h::t -> 
                                      (match h with 
                                      | TokenInt _ -> aux (TokenInt (string_of_int(divide_tokens_int kept h))) t 
                                      | _ -> raise (Failure "Needed int for *"))) else raise (Failure "Unexpected char for BinaryOper token"))
    | _ -> raise (Failure "Arg kept supposed to be Int or None"))
                  
in aux TokenNone token_list
    

