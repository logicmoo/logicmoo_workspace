--# -path=.:../Common:prelude

concrete AgendaSystemSem of AgendaSystem = GodisSystemSem, BookingSem ** open Prolog in {

lin

-- Short answers

event = pp1 "event";
time = pp1 "time";
date = pp1 "date";

-- Predicates


date_to_add_Q = pWhQ "date_to_add";
date_to_add_P = pp1 "date_to_add";


time_to_add_Q = pWhQ "time_to_add";
time_to_add_P = pp1 "time_to_add";


event_to_add_Q = pWhQ "event_to_add";
event_to_add_P = pp1 "event_to_add";



-- Actions

agenda_add = pp0 "add_event";

agenda_delete = pp0 "delete_event";

agenda_move = pp0 "change_date";

agenda_change_time = pp0 "change_time";

agenda_change_date = pp0 "change_date";


}
