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

fun isLongEvent(e : event) =
    getDuration e > 60

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
        | (f:fs) => (f e) andalso (allTrue fs e);

val event1 : event = ("Workshop", 120, 25, 300);
val event2 : event = ("Seminar", 45, 80, 200);
val event3 : event = ("Hackathon", 480, 150, 1000);
val ruleSet1 = [longerThan 60, costBetween 0 500];
val ruleSet2 = [isLargeEvent, costBetween 800 2000];
val _ = print ("RuleSet1 on Workshop: " ^
Bool.toString (allTrue ruleSet1 event1) ^ "\n");
