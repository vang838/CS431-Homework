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

