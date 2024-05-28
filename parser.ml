open Types
open Utils

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions - Takes in a token list and outputs an expression equivalent*)

let rec parse_expr toks =
  let op = lookahead toks in match op with 
  |Some Tok_Let -> letExpr toks
  |Some Tok_If -> ifExpr toks
  |Some Tok_Fun -> functionExpr toks
  |_ -> orExpr toks
  

and orExpr toks = let (t1, e) = andExpr toks in match lookahead t1 with 
  |Some Tok_Or -> let t2 = match_token t1 Tok_Or in
                  let (t3, f) = orExpr t2 in
                  (t3, Binop(Or, e, f))
  |_ -> (t1, e)

and andExpr toks = let (t1, e) = equalityExpr toks in match lookahead t1 with 
|Some Tok_And -> let t2 = match_token t1 Tok_And in
                  let (t3, f) = andExpr t2 in
                  (t3, Binop(And, e, f))
|_ -> (t1, e)

and equalityExpr toks = let (t1, e) = relationalExpr toks in match lookahead t1 with 
|Some Tok_Equal -> let t2 = match_token t1 Tok_Equal in
                    let (t3, f) = equalityExpr t2 in
                    (t3, Binop(Equal, e, f))
|Some Tok_NotEqual -> let t2 = match_token t1 Tok_NotEqual in
                      let (t3, f) = equalityExpr t2 in
                      (t3, Binop(NotEqual, e, f))
|_ -> (t1, e)

and relationalExpr toks = let (t1, e) = additiveExpr toks in match lookahead t1 with 
|Some Tok_Less -> let t2 = match_token t1 Tok_Less in
                  let (t3, f) = relationalExpr t2 in
                  (t3, Binop(Less, e, f))
|Some Tok_Greater -> let t2 = match_token t1 Tok_Greater in
                  let (t3, f) = relationalExpr t2 in
                  (t3, Binop(Greater, e, f))
|Some Tok_LessEqual -> let t2 = match_token t1 Tok_LessEqual in
                  let (t3, f) = relationalExpr t2 in
                  (t3, Binop(LessEqual, e, f))
|Some Tok_GreaterEqual -> let t2 = match_token t1 Tok_GreaterEqual in
                  let (t3, f) = relationalExpr t2 in
                  (t3, Binop(GreaterEqual, e, f))
|_ -> (t1, e)

and additiveExpr toks = let (t1, e) = multiplicativeExpr toks in match lookahead t1 with 
|Some Tok_Add -> let t2 = match_token t1 Tok_Add in
                  let (t3, f) = additiveExpr t2 in
                  (t3, Binop(Add, e, f))
|Some Tok_Sub -> let t2 = match_token t1 Tok_Sub in
                  let (t3, f) = additiveExpr t2 in
                  (t3, Binop(Sub, e, f))
|_ -> (t1, e)

and multiplicativeExpr toks = let (t1, e) = concatExpr toks in match lookahead t1 with 
|Some Tok_Mult -> let t2 = match_token t1 Tok_Mult in
                  let (t3, f) = multiplicativeExpr t2 in
                  (t3, Binop(Mult, e, f))
|Some Tok_Div -> let t2 = match_token t1 Tok_Div in
                  let (t3, f) = multiplicativeExpr t2 in
                  (t3, Binop(Div, e, f))
|_ -> (t1, e)

and concatExpr toks = let (t1, e) = unaryExpr toks in match lookahead t1 with 
|Some Tok_Concat -> let t2 = match_token t1 Tok_Concat in
                let (t3, f) = concatExpr t2 in
                (t3, Binop(Concat, e, f))
|_ -> (t1, e)

and unaryExpr toks = match lookahead toks with 
|Some Tok_Not -> let t2 = match_token toks Tok_Not in
                let (t3, f) = unaryExpr t2 in
                (t3, Not(f))
|_ -> appExpr toks

and appExpr toks = let (t1, e) = selectExpr toks in match lookahead t1 with
|Some Tok_Int x -> let (t2, f) = primaryExpr t1
                    in (t2, App(e, f)) 
|Some Tok_Bool x -> let (t2, f) = primaryExpr t1
                    in (t2, App(e, f)) 
|Some Tok_String x -> let (t2, f) = primaryExpr t1
                      in (t2, App(e, f)) 
|Some Tok_ID x -> let (t2, f) = primaryExpr t1
                  in (t2, App(e, f)) 
|Some Tok_LParen -> let (t2, f) = primaryExpr t1
                    in (t2, App(e, f)) 
|Some Tok_LCurly -> let (t2, f) = primaryExpr t1
                    in (t2, App(e, f)) 
|_ -> (t1, e)

and selectExpr toks = let (t1, e) = primaryExpr toks in match lookahead t1 with 
|Some Tok_Dot -> let t2 = match_token t1 Tok_Dot in (match lookahead t2 with 
  |Some Tok_ID x -> let t3 = match_token t2 (Tok_ID x) in 
                    (t3, Select(Lab x, e))
  |_ -> raise (InvalidInputException "2"))
|_ -> (t1, e)

and primaryExpr toks = match lookahead toks with 
|Some (Tok_Int x)-> let t2 = match_token toks (Tok_Int x) in
                (t2, (Int(x)))
|Some (Tok_Bool x)-> let t2 = match_token toks (Tok_Bool x) in
                (t2, (Bool(x)))
|Some (Tok_String x)-> let t2 = match_token toks (Tok_String x) in
                (t2, (String(x)))
|Some (Tok_ID x)-> let t2 = match_token toks (Tok_ID x) in
                (t2, (ID(x)))
|Some Tok_LParen -> let t2 = match_token toks Tok_LParen in
                let (t3, f) = parse_expr t2 in
                let t4 = match_token t3 Tok_RParen in
                (t4, f)
|Some Tok_LCurly -> recordExpr toks
|_ -> raise (InvalidInputException "19")

and recordExpr toks = match lookahead toks with 
|Some Tok_LCurly -> let t1 = match_token toks Tok_LCurly in (match lookahead t1 with 
  |Some Tok_RCurly -> let t2 = match_token t1 Tok_RCurly in
                    (t2, Record[])
  |_ -> let (t3, f) = recordBodyExpr t1 in
        let t4 = match_token t3 Tok_RCurly in 
        (t4, f))
|_ -> raise (InvalidInputException "3")

and recordBodyExpr toks = match lookahead toks with 
|Some (Tok_ID x) -> let t1 = match_token toks (Tok_ID x) in (match lookahead t1 with 
  |Some Tok_Equal -> let t2 = match_token t1 Tok_Equal in 
                      let (t3, f) = parse_expr t2 in 
                      (match lookahead t3 with 
    |Some Tok_Semi -> let t4 = match_token t3 Tok_Semi in 
                      let (t5,g) = recordBodyExpr t4 in 
                      (match g with 
                      |Record l -> (t5, Record((Lab x, f)::l))
                      |_ -> raise (InvalidInputException "21"))
    |_ -> (t3, Record[(Lab x, f)]))                     
  |_ -> raise (InvalidInputException "4"))
|_ -> raise (InvalidInputException "20")


and letExpr toks = match lookahead toks with 
|Some Tok_Let -> let t1 = match_token toks Tok_Let in 
                (match lookahead t1 with
  |Some Tok_Rec -> let t2 = match_token t1 Tok_Rec in 
                  (match lookahead t2 with 
    |Some Tok_ID x -> let t3 = match_token t2 (Tok_ID x) in 
                      (match lookahead t3 with 
      |Some Tok_Equal -> let t4 = match_token t3 Tok_Equal in 
                        let (t5, f) = parse_expr t4 in 
                        (match lookahead t5 with
        |Some Tok_In -> let t6 = match_token t5 (Tok_In) in 
                        let (t7, g) = parse_expr t6 in 
                        (t7, Let(x, true, f, g))
        |_ -> raise (InvalidInputException "5"))
      |_ -> raise (InvalidInputException "6"))
    |_ -> raise (InvalidInputException "7"))

  |Some Tok_ID x -> let t2 = match_token t1 (Tok_ID x) in 
          (match lookahead t2 with 
    |Some Tok_Equal -> let t3 = match_token t2 Tok_Equal in 
        let (t4, f) = parse_expr t3 in 
        (match lookahead t4 with
      |Some Tok_In -> let t5 = match_token t4 (Tok_In) in 
          let (t6, g) = parse_expr t5 in 
          (t6, Let(x, false, f, g))
      |_ -> raise (InvalidInputException "8"))
    |_ -> raise (InvalidInputException "9"))
  |_ -> raise (InvalidInputException "10"))
|_ -> raise (InvalidInputException "11")

and functionExpr toks = match lookahead toks with 
|Some Tok_Fun -> let t1 = match_token toks Tok_Fun in 
                (match lookahead t1 with
  |Some Tok_ID x -> let t2 = match_token t1 (Tok_ID x) in 
                    (match lookahead t2 with
    |Some Tok_Arrow -> let t3 = match_token t2 Tok_Arrow in 
                        let (t4, e) = parse_expr t3 in 
                        (t4, Fun(x, e))
    |_ -> raise (InvalidInputException "12"))
  |_ -> raise (InvalidInputException "13"))
|_ -> raise (InvalidInputException "14")

and ifExpr toks = match lookahead toks with 
|Some Tok_If -> let t1 = match_token toks Tok_If in 
                let (t2, e) = parse_expr t1 in 
                (match lookahead t2 with
  |Some Tok_Then -> let t3 = match_token t2 Tok_Then in 
                    let (t4, f) = parse_expr t3 in 
                    (match lookahead t4 with
    |Some Tok_Else -> let t5 = match_token t4 Tok_Else in 
                      let (t6, g) = parse_expr t5 in 
                      (t6, If(e,f,g))
    |_ -> raise (InvalidInputException "15"))
  |_ -> raise (InvalidInputException "16"))
|_ -> raise (InvalidInputException "17")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match lookahead toks with 
  |Some Tok_Def -> defMutop toks 
  |Some Tok_DoubleSemi -> noOp toks
  |_ -> exprMutop toks

and defMutop toks = match lookahead toks with 
|Some Tok_Def -> let t1 = match_token toks Tok_Def in
                  (match lookahead t1 with 
  |Some Tok_ID x -> let t2 = match_token t1 (Tok_ID x) in
                    (match lookahead t2 with 
    |Some Tok_Equal -> let t3 = match_token t2 Tok_Equal in 
                        let (t4, e) = parse_expr t3 in 
                        (match lookahead t4 with
      |Some Tok_DoubleSemi -> let t5 = match_token t4 Tok_DoubleSemi in
                              (t5, Def(x, e))
      |_ -> raise (InvalidInputException "a"))
    |_ -> raise (InvalidInputException "b"))
  |_ -> raise (InvalidInputException "c"))
|_ -> raise (InvalidInputException "d")


and exprMutop toks = let (t1, e) = parse_expr toks in match lookahead t1 with 
|Some Tok_DoubleSemi -> let t2 = match_token t1 Tok_DoubleSemi in
                        (t2, Expr(e))
|_ -> raise (InvalidInputException "e")

and noOp toks = match lookahead toks with 
|Some Tok_DoubleSemi -> let t1 = match_token toks Tok_DoubleSemi in
                        ([], NoOp)
|_ -> raise (InvalidInputException "f")
