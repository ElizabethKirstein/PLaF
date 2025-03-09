open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | Emptylist _ -> return (ListVal [])
  | Cons (e1, e2) ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    (match v2 with
    | ListVal lst -> return (ListVal (v1 :: lst))
    | _ -> error "Second argument of cons must be a list!")
  | Hd e ->
    eval_expr e >>= fun v ->
    (match v with
    | ListVal (h :: _) -> return h
    | ListVal [] -> error "Cannot take head of an empty list!"
    | _ -> error "Argument of hd must be a list!")
  | Tl e ->
    eval_expr e >>= fun v ->
    (match v with
    | ListVal (_ :: t) -> return (ListVal t)
    | ListVal [] -> error "Cannot take tail of an empty list!"
    | _ -> error "Argument of tl must be a list!")
  | IsEmpty e ->
    eval_expr e >>= fun v ->
    (match v with
    | ListVal [] -> return (BoolVal true)
    | ListVal _ -> return (BoolVal false)
    | _ -> error "Argument of empty? must be a list!")
  | Tuple es ->
    eval_exprs es >>= fun vals ->
    return (TupleVal vals)
  | Untuple (ids, e1, e2) ->
    eval_expr e1 >>= fun v1 ->
    (match v1 with
    | TupleVal vals ->
      if List.length ids = List.length vals then
        extend_env_list ids vals >>= fun _ -> eval_expr e2
      else
        error "extend_env_list: Arguments do not match parameters!"
    | _ -> error "Expected a tuple!")

and eval_exprs : expr list -> (exp_val list) ea_result = fun es ->
  match es with
  | [] -> return []
  | h :: t ->
    eval_expr h >>= fun v ->
    eval_exprs t >>= fun vs ->
    return (v :: vs)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


