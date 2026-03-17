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

