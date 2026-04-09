(* Problem 1 - Representing Arithmetic Expressions*)
datatype expr =
    Const of int
    | Var of string
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Neg of expr; (* Separate constructor for unary negation creates a cleaner AST than using subtraction by 0 *) 

(* Problem 2 - Pretty Printer *)
fun toString (Const n) = Int.toString n
    | toString (Var x) = x
    | toString (Add(e1,e2)) = "(" ^ toString e1 ^ " + " ^ toString e2 ^")"
    | toString (Sub(e1,e2)) = "(" ^ toString e1 ^ " - " ^ toString e2 ^")"
    | toString (Mul(e1,e2)) = "(" ^ toString e1 ^ " * " ^ toString e2 ^")"
    | toString (Div(e1,e2)) = "(" ^ toString e1 ^ " / " ^ toString e2 ^")"
    | toString (Neg e) = "(~" ^ toString e ^ ")";

(* Problem 3 - Evaluation with Environment *)
exception DividedByZero;
exception UndefinedVariable;

fun eval (Const n) env = n
    | eval (Var x) env = env x 
    | eval (Add(e1,e2)) env = eval e1 env + eval e2 env
    | eval (Sub(e1,e2)) env = eval e1 env - eval e2 env
    | eval (Mul(e1,e2)) env = eval e1 env * eval e2 env
    | eval (Div(e1,e2)) env = 
        let
            val v1 = eval e1 env
            val v2 = eval e2 env
        in
            if v2 = 0 then raise DividedByZero
            else v1 div v2
        end
    | eval (Neg e) env = ~(eval e env);

(* Problem 4 - Symbolic Simplification *)
fun simplify (Const n) =
    