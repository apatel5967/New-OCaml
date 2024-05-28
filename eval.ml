open Types

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 3: Evaluating expressions - Takes in the expression and an environment, and returns the result of it*)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec eval_expr env e = match e with 
|Int _ | Bool _ | String _ -> e
|ID x -> lookup env x
|Not x -> let v = eval_expr env x in (match v with 
          |Bool b -> Bool (not b)
          |_ -> raise (TypeError "1"))
|Binop (op,x,y) -> let x1 = eval_expr env x in 
                    let y1 = eval_expr env y in 
                    (match op with 
          |Add -> (match x1,y1 with
            |Int a, Int b -> Int (a + b) 
            |_, _ -> raise (TypeError "2"))
          |Sub -> (match x1, y1 with
            |Int a, Int b -> Int (a - b) 
            |_, _ -> raise (TypeError "3"))
          |Mult -> (match x1,y1 with
            |Int a, Int b -> Int (a * b) 
            |_, _ -> raise (TypeError "4"))
          |Div -> (match x1,y1 with
            |Int a, Int b -> if b = 0 then raise (DivByZeroError) else Int(a/b) 
            |_, _ -> raise (TypeError "5"))
          |Greater -> (match x1,y1 with
            |Int a, Int b -> if a > b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "6"))
          |Less -> (match x1,y1 with
            |Int a, Int b -> if a < b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "7"))
          |GreaterEqual -> (match x1,y1 with
            |Int a, Int b -> if a >= b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "8"))
          |LessEqual -> (match x1,y1 with
            |Int a, Int b -> if a <= b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "9"))
          |Concat -> (match x1,y1 with
            |String a, String b -> String(a ^ b)
            |_, _ -> raise (TypeError "10"))
          |Equal -> (match x1,y1 with
            |Int a, Int b -> if a = b then Bool (true) else Bool (false) 
            |String a, String b -> if a = b then Bool (true) else Bool (false) 
            |Bool a, Bool b -> if a = b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "11"))
          |NotEqual -> (match x1,y1 with
            |Int a, Int b -> if a <> b then Bool (true) else Bool (false) 
            |String a, String b -> if a <> b then Bool (true) else Bool (false) 
            |Bool a, Bool b -> if a <> b then Bool (true) else Bool (false) 
            |_, _ -> raise (TypeError "12"))
          |Or -> (match x1,y1 with 
            |Bool true, Bool b -> Bool true
            |Bool a, Bool true -> Bool true
            |Bool a, Bool b -> if (a || b) then Bool (true) else Bool (false)
            |_, _ -> raise (TypeError "13"))
          |And -> (match x1,y1 with 
            |Bool false, Bool b -> Bool false
            |Bool a, Bool false -> Bool false
            |Bool a, Bool b -> if a && b then Bool (true) else Bool (false)
            |_, _ -> raise (TypeError "14"))
          |_ -> raise (TypeError "15"))
|If (x,y,z) -> let x1 = eval_expr env x in 
                (match x1 with 
          |Bool b -> if b = true then eval_expr env y else eval_expr env z
          |_ -> raise (TypeError "16"))
|Let (v, b, x, y) -> if b then 
                    let temp = extend_tmp env v in 
                    let x1 = eval_expr temp x in 
                    update temp v x1; eval_expr temp y
                    else 
                    let x1 = eval_expr env x in 
                    let env1 = extend env v x1 in 
                    eval_expr env1 y
|Fun (v, x) -> Closure(env, v, x)
|App (x, y) -> let close = eval_expr env x in 
              let y1 = eval_expr env y in
              (match close with 
          |Closure(new_env, v, x) -> let x1 = extend new_env v y1 in eval_expr x1 x
          |_ -> raise (TypeError "17")) 
|Record lst -> Record(lst)
|Select (lab, x) -> (match (eval_expr env x) with
                    | Record x -> let rec brap lst = (match lst with
                      |[] -> raise (SelectError "18")
                      |(l, v) :: t -> if (l = lab) then v else brap t)
                    in eval_expr env (brap x)
                    | _ -> raise (SelectError "19"))

|_ -> raise (TypeError "20")


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
|Def(v, x) -> let temp = extend_tmp env v in 
              let x1 = eval_expr temp x in 
              update temp v x1;(temp, Some(x1))
|Expr (x) -> (env, Some(eval_expr env x)) 
|NoOp -> (env, None)
                

