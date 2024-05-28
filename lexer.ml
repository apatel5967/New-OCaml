open Types

(* Part 1: Lexer - Goes through the input string and creates a list of tokens *)

let tokenize input = 
  let rec help i s = 
    if i >= String.length s then [] else
      if Str.string_match (Str.regexp "[0-9]+") s i then
        let token = Str.matched_string s in
        let len = String.length token in
        Tok_Int (int_of_string token) :: help (i + len) s else
      if Str.string_match (Str.regexp "(-[0-9]+)") s i then
        let token = Str.matched_string s in
        let len = String.length token in
        Tok_Int (int_of_string (String.sub token 1 ((String.length token)-2))) :: help (i + len) s else
      if Str.string_match (Str.regexp "true\\|false") s i then
        let token = Str.matched_string s in
        let len = String.length token in
        Tok_Bool (bool_of_string token) :: help (i + len) s else
      if Str.string_match (Str.regexp "\"[^\"]*\"") s i then
        let token = Str.matched_string s in
        let len = String.length token in
        Tok_String (String.sub token 1 ((String.length token)-2)) :: help (i + len) s else
      if Str.string_match (Str.regexp "(") s i then  
        Tok_LParen :: help (i + 1) s else 
      if Str.string_match (Str.regexp ")") s i then
        Tok_RParen :: help (i + 1) s else 
      if Str.string_match (Str.regexp "{") s i then
        Tok_LCurly :: help (i + 1) s else 
      if Str.string_match (Str.regexp "}") s i then
        Tok_RCurly :: help (i + 1) s else 
      if Str.string_match (Str.regexp "\\.") s i then
        Tok_Dot :: help (i + 1) s else 
      if Str.string_match (Str.regexp "=") s i then
        Tok_Equal :: help (i + 1) s else 
      if Str.string_match (Str.regexp "<>") s i then
        Tok_NotEqual :: help (i + 2) s else 
      if Str.string_match (Str.regexp ">") s i then
        Tok_Greater :: help (i + 1) s else 
      if Str.string_match (Str.regexp "<") s i then
        Tok_Less :: help (i + 1) s else 
      if Str.string_match (Str.regexp ">=") s i then
        Tok_GreaterEqual :: help (i + 2) s else 
      if Str.string_match (Str.regexp "<=") s i then
        Tok_LessEqual :: help (i + 2) s else
      if Str.string_match (Str.regexp "||") s i then
        Tok_Or :: help (i + 2) s else 
      if Str.string_match (Str.regexp "&&") s i then
        Tok_And :: help (i + 2) s else 
      if Str.string_match (Str.regexp "not") s i then
        Tok_Not :: help (i + 3) s else 
      if Str.string_match (Str.regexp "if") s i then
        Tok_If :: help (i + 2) s else 
      if Str.string_match (Str.regexp "then") s i then
        Tok_Then :: help (i + 4) s else 
      if Str.string_match (Str.regexp "else") s i then
        Tok_Else :: help (i + 4) s else 
      if Str.string_match (Str.regexp "let") s i then
        Tok_Let :: help (i + 3) s else 
      if Str.string_match (Str.regexp "def") s i then
        Tok_Def :: help (i + 3) s else 
      if Str.string_match (Str.regexp "in") s i then
        Tok_In :: help (i + 2) s else 
      if Str.string_match (Str.regexp "rec") s i then
        Tok_Rec :: help (i + 3) s else 
      if Str.string_match (Str.regexp "fun") s i then
        Tok_Fun :: help (i + 3) s else 
      if Str.string_match (Str.regexp "->") s i then
        Tok_Arrow :: help (i + 2) s else 
      if Str.string_match (Str.regexp ";;") s i then
        Tok_DoubleSemi :: help (i + 2) s else 
      if Str.string_match (Str.regexp ";") s i then
        Tok_Semi :: help (i + 1) s else 
      if Str.string_match (Str.regexp "+") s i then
        Tok_Add :: help (i + 1) s else 
      if Str.string_match (Str.regexp "-") s i then
        Tok_Sub :: help (i + 1) s else 
      if Str.string_match (Str.regexp "*") s i then
        Tok_Mult :: help (i + 1) s else 
      if Str.string_match (Str.regexp "/") s i then
        Tok_Div :: help (i + 1) s else 
      if Str.string_match (Str.regexp "\\^") s i then
        Tok_Concat :: help (i + 1) s else 
      if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") s i then
        let token = Str.matched_string s in
        let len = String.length token in
        Tok_ID token :: help (i + len) s else
      if Str.string_match (Str.regexp " \\|\t\\|\n") s i then 
        help (i + 1) s
      else raise (InvalidInputException "tokenize error")
in help 0 input 