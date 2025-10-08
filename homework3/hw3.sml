type event = string * int * int * int;

(* Accessors *)
fun getName(e : event) =
    let val (name, _, _, _) = e
    in name end;

fun getDuration(e : event) =
    let val (_, duration, _, _) = e
    in duration end;

fun getAttendees(e : event) =
    let val (_, _, attendees, _) = e
    in attendees end;

fun getCost(e : event) =
    let val (_, _, _, cost) = e
    in cost end;

(* Basic Functions *)
fun isShortEvent(e : event) =
    getDuration e < 60

fun isLargeEvent(e : event) =
    getAttendees e > 50

fun isLowCost (e : event) =
    getCost e < 100

(* Logical Combinators *)
fun andCond (f : event -> bool) (g : event -> bool) : event -> bool =
    fn (e : event) => f e andalso g e;

fun orCond (f : event -> bool) (g : event -> bool) =
    fn (e : event) => f e orelse g e;

fun notCond (f : event -> bool) =
    fn (e : event) => not (f e);

(* Curried Conditions *)
fun longerThan (n : int) : event -> bool =
    fn (e : event) => getDuration e > n;

fun fewerThan(n : int) : event -> bool =
    fn (e : event) => getAttendees e < n;

fun costBetween(low : int) (high : int ) : event -> bool =
    fn(e : event) =>
        let val c = getCost e
        in c >= low andalso c <= high end;

(* Rule Set *)
fun allTrue (conds : (event -> bool) list) (e : event) =
    case conds of
        [] => true
        | (f::fs) => (f e) andalso (allTrue fs e);

(* Tests copied from hw3 pdf *)
val event1 : event = ("Workshop", 120, 25, 300);
val event2 : event = ("Seminar", 45, 80, 200);
val event3 : event = ("Hackathon", 480, 150, 1000);

val ruleSet1 = [longerThan 60, costBetween 0 500];
val ruleSet2 = [isLargeEvent, costBetween 800 2000];
val ruleSet3 = [fewerThan 200, isLargeEvent, costBetween 500 1500];

val _ = print ("RuleSet1 on Workshop: " ^
    Bool.toString (allTrue ruleSet1 event1) ^ "\n");

val _ = print ("RuleSet2 on Seminar: " ^
    Bool.toString (allTrue ruleSet2 event2) ^ "\n");

val _ = print ("RuleSet3 on Hackathon: " ^
    Bool.toString (allTrue ruleSet3 event3) ^ "\n");
