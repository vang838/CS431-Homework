type event = string * int * int * int;

fun getName(e:event) =
    let val = (name,_,_,_) = e
    in name
    end;

fun getDuration(e:event) =
    let val = (_,duration,_,_) = e
    in duration
    end;

fun getAttendees(e:event) =
    let val = (_,_,attendees,_) = e
    in attendees
    end;

fun getCost(e:event) =
    let val = (_,_,_,cost) = e
    in cost
    end;

fun isShortEvent(e:event) =
    getDuration e < 60

fun isLargeEvent(e:event) =
    getAttendees e > 50

fun isLowCost(e:event) =
    getCost e > 500

fun andCond(f:event -> bool)(g:event -> bool) =
    fn(e:event) => f e andalso g e;

fun orCond(f:event -> bool)(g:event -> bool) =
    fn(e:event) => f e orelse f g 

fun notCond(f:event -> bool) =
    fn(e:event) => not(fe)

(* Tests copied from hw3 pdf *)
val event1 : event = ("Workshop", 120, 25, 300);
val event2 : event = ("Seminar", 45, 80, 200);

val ruleSet1 = [longerThan 60, costBetween 0 500];
val ruleSet2 = [isLargeEvent, costBetween 800 2000];

val _ = print ("RuleSet1 on Workshop: " ^
    Bool.toString (allTrue ruleSet1 event1) ^ "\n");

val _ = print ("RuleSet2 on Seminar: " ^
    Bool.toString (allTrue ruleSet2 event2) ^ "\n");
