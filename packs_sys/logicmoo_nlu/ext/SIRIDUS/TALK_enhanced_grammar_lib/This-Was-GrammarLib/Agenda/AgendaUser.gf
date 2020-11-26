--# -path=.:../Common:prelude

abstract AgendaUser = GodisUser, Booking ** {

fun

-- short answers

time : Time -> ShortAns;
event : Event -> ShortAns;
date : Date -> ShortAns;

-- "Meeting at five"
event_time : Event -> Time -> ShortAns;


-- ### "Meeting on friday"

event_date : Event -> Date -> ShortAns;


-- #### "Meeting on friday at five"
-- #### "Meeting at five on friday"

event_date_time : Event -> Date -> Time -> ShortAns;


-- #### "At five on friday"
-- #### "on friday at five"

time_date : Time -> Date -> ShortAns;


-- #### "Fridays meeting"





-- predicate


-- U: How is the schedule for monday?
how_is_schedule : Date -> Question;

-- U: When is event?
when_is_event_day : Event -> Date -> Question;


-- U: What is the date today?

what_is_the_date : Question;


-- U: Am I booked Date Time?

am_I_booked : Date -> Time -> Question;


-- U: What day is the Event?

what_day_is_event : Event -> Question;


-- U: What day is the Event at Time?

what_day_is_event_at_time : Event -> Time -> Question;



-- actions


-- ### flytta DET/DEN --> request(change_date) answer(X^ref(X))
-- ### ändra tiden på DEN/DET --> request(change_time) answer(X^ref(X))
-- ### ta bort DET/DEN-->  request(delete_event) answer(X^ref(X))
-- ### 
-- ### eller
-- ### 
-- ### lägg till ett möte DEN DAGEN --> request(delete_event) answer(X^date(X)))






-- action add

add : Action;
add__event : Event -> Action;

add__event_date : Event -> Date -> Action;
add__event_time : Event -> Time -> Action;
add__event_time_date : Event -> Time -> Date -> Action;


-- action delete
----- ### delete__event_time : Event -> Time -> Action;


delete : Action;


-- action move

move_booking : Action;

-- action change_time
----- ### change_time_event/date/time "I want to change the time for the lecture on tuesday at five"

change_time : Action;

-- action change_date
----- ### change_date_event/date/time "I want to change the date for the meeting on wednesday the fifth"

change_date : Action;



-- ##### action go_to "Go to march", "go to monday the second"

-- behöver lägga till ett "go to" verb!

}




