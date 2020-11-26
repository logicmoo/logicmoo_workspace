--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common

incomplete concrete AgendaSystemI of AgendaSystem = 
    GodisSystemI ** open GodisLang, Grammar, AgendaLexicon in {

lin

------------------------------------------------------------------------------
-- Predicates and Questions 


-- S: What date?
date_to_add_Q = isDoing ** which_N_do_you_want_to_V2 date_N add_V2;
-- U: DATE
date_to_add_P x = isDoing ** you_want_to_VP (ComplV2 add_V2 x);


-- S: What time?
time_to_add_Q = isDoing ** which_N_do_you_want_to_V2 time_N add_V2 ;
-- U: TIME
time_to_add_P x = isDoing ** you_want_to_VP (ComplV2 add_V2 x);

-- S: What type of booking?
event_to_add_Q = isDoing ** which_N_do_you_want_to_V2 event_N add_V2;
-- U: EVENT
event_to_add_P x = isDoing ** you_want_to_VP (ComplV2 add_V2 x);

{-
-- U: When is the EVENT?
time_for_event_Q = ;
-- S: DATE
time_for_event_P x = ;


-- U: Do I have anything booked DATE?
booking_on_date_Q : Question;
-- ??? S: you have nothing/EVENT,TIME booked
booking_on_date_P : Preposition;
-}


------------------------------------------------------------------------------

-- agenda_add

-- agenda_add     = sf ["add a booking to the agenda"] ["added a booking to the agenda"];
agenda_add     = hasDone ** ComplV3 add_V3 (indef_N_sg booking_N) (the_N_sg agenda_N);


-- agenda_delete
agenda_delete = hasDone ** ComplV3 delete_V3 (indef_N_sg booking_N) (the_N_sg agenda_N);

-- agenda_move

agenda_move = hasDone ** V2_the_N move_V2 booking_N;

-- agenda_change_time

agenda_change_time = hasDone ** V2_the_N change_V2 time_N;
agenda_change_time__booking = hasDone ** ComplV3 change_V3 (the_N_sg time_N) (indef_N_sg booking_N);
agenda_change_date__booking = hasDone ** ComplV3 change_V3 (the_N_sg date_N) (indef_N_sg booking_N);

-- agenda_change_date

agenda_change_date = hasDone ** V2_the_N change_V2 date_N;



}
