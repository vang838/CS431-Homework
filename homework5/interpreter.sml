datatype expr = Int of int
    | Var of string
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Let of string * expr * expr
    | Lambda of string * expr
    | Apply of expr * expr
    | Tuple of expr list;

datatype value = IntV of int
    | TupleV of value list
    | Closure of string * expr * (string * value) list;

type env = (string * value) list;

fun eval (Int n) env = IntV n
    | eval (Var x) env = lookup x env
    | eval (Add(e1,e2)) env =
        case (eval e1 env, eval e2 env) of
            (IntV n1, IntV n2) = IntV (n1 + n2)
            | _ => raise TypeError "Add operation expects two integers"
    | eval (Sub(e1, e2)) env =
        case (eval e1 env, eval e2 env) of
            (IntV n1, IntV n2) = IntV (n1 - n2)
            | _ => raise TypeError "Subtraction operation expects two integers" 
