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
fun simplify (Const n) = Const n
    | simplify (Var x) = Var x
    | simplify (Add(e1,e2)) =
        let 
            val res1 = simplify e1
            val res2 = simplify e2
        in
            case (res1, res2) of
                (Const 0, e) => e
                | (e, Const 0) => e
                | (Const a, Const b) => Const(a+b)
                | _ => Add(res1, res2)
        end
    | simplify (Sub(e1,e2)) =
        let
            val res1 = simplify e1
            val res2 = simplify e2
        in
            case (res1, res2) of
                (Const 0, e) => simplify (Neg e)
                | (e, Const 0) => e
                | (Const a, Const b) => Const(a-b)
                | _ => Sub(res1, res2)
        end
    | simplify (Mul(e1, e2)) =
        let
            val res1 = simplify e1
            val res2 = simplify e2
        in
            case (res1, res2) of
                (_, Const 0) => Const 0
                | (Const 0, _) => Const 0
                | (e, Const 1) => e
                | (Const 1, e) => e
                | (Const a, Const b) => Const(a*b)
                | _ => Mul(res1, res2)
        end
    | simplify (Div(e1, e2)) =
        let
            val res1 = simplify e1
            val res2 = simplify e2
        in
            case (res1, res2) of
                (e, Const 1) => e
                | (Const a, Const b) =>
                    if b = 0 then raise DividedByZero
                    else Const(a div b)
                | _ => Div(res1, res2)
        end
    | simplify (Neg e) =
        let
            val result = simplify e 
        in 
            case result of
                Const c => Const (~c)
                | Neg e' => e'
                | _ => Neg result
        end;

val t1 = simplify (Sub (Const 10, Const 4));   (* Const 6 *)
val t2 = simplify (Sub (Const 3, Const 5));    (* Const (~2) *)
val t3 = simplify (Sub (Var "x", Const 0));    (* Var "x" *)
val t4 = simplify (Sub (Const 0, Var "x"));    (* Neg (Var "x") *)
val t5 = simplify (Mul (Const 0, Var "y"));   (* Const 0 *)

(* Problem 5 - Reflection and Analysis 
    1. By using algebraic data types for symbolic expressions, we make the entire structure in the language explicitly in one place rather 
    than spreading the structure across multiple classes like in Java.  
    Adding new operations within an ADT is easier since we can write over existing constructors but with adding new expression forms, we need
    update every function that may use "expr".

    2. By explicitly creating cases for pattern matching, we remove reliance on long if-else chains that can miss specific conditions if they are
    placed in the wrong order

    3. If the expression type for exponentiation were to be added, a constructor would need to be created in the expr datatype.  Then a case would
    need to be added in functions toString, eval, and simplify.
*)

(* Bonus Question: Function derivative : expr -> string -> expr *)
fun derivative (Const _) x = Const 0
    | derivative (Var y) x =
        if y = x then Const 1
        else Const 0
    | derivative (Add(e1, e2)) x =
        Add(derivative e1 x, derivative e2 x)
    | derivative (Sub(e1, e2)) x =
        Sub(derivative e1 x, derivative e2 x)
    | derivative (Mul(e1, e2)) x =
        Add(Mul(derivative e1 x, e2), Mul(e1, derivative e2 x))
    | derivative (Div(e1, e2)) x =
        Div( Sub( Mul(derivative e1 x, e2), Mul(e1, derivative e2 x)), Mul(e2, e2))
    | derivative (Neg e) x =
        Neg (derivative e x);

val d1 = derivative (Add (Var "x", Const 3)) "x"; (* Add (Const 1, Const 0) *)
val d2 = derivative (Add (Var "x", Const 3)) "x"; (* Const 1 *)
val d3 = derivative (Mul (Var "x", Var "x")) "x";  (* Add (Var "x", Var "x") *)
val d4 = derivative (Div (Var "x", Const 2)) "x";  (* Div (Const 2, Const 4) -> then simplify may reduce further depending on rules *)