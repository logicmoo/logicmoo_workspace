--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete AgendaUserEng of AgendaUser = GodisUserEng, BookingEng ** 
    open CatEng, Prelude, GodisLangEng, AgendaSystemEng, ResEng, AgendaLexiconEng in {

oper 

E_at_T : NP -> NP -> Str
    = \e,t -> variants{e.s!Nom ++ "at" ++ t.s!Nom;
                       e.s!Nom ++ t.s!Nom ++ "oclock"};

E_on_D : NP -> NP -> Str
   = \e,d -> variants{e.s!Nom ++ "on" ++ d.s!Nom;
		      e.s!Nom ++ d.s!Nom};

-- short answers

lin

event x = ansNP x;
time x = ansNP x;
date x = ansNP x;
event_time x y = ss (E_at_T x y);

-- predicates
-- U: How is the schedule for monday?
how_is_schedule d = ss ( ["how is my scehdule on"] ++ d.s!Nom) ;

-- U: When is event?
when_is_event_day e d = ss ( ["when is"] ++ e.s!Nom ++ ["on"] ++ d.s!Nom );

--U: What is the date today?

what_is_the_date = ss (["what day is it today"]);


-- U: Am I booked Date Time?

am_I_booked d t = ss ( ["am i booked"] ++ d.s!Nom ++ "at" ++ t.s!Nom);


-- U: What day is the Event?

what_day_is_event e = ss ( ["when is"] ++ e.s!Nom);


-- U: What day is the Event at Time?

what_day_is_event_at_time e t = ss (["what date is"] ++ e.s!Nom ++ ["at"] ++ t.s!Nom);



-- actions

-- action add
lin

add = variants{ reqVP agenda_add;
 		req1x "add" (optStr ["a booking"] ++ to_agenda) };


add__event y = req1x "add" (y.s!Nom ++ to_agenda);

add__event_time x y = req1x "add" (E_at_T x y ++ to_agenda);
add__event_date e d = req1x "add" (E_on_D e d ++ to_agenda);
add__event_time_date e t d = req1x "add" ((E_at_T e t) ++ "on" ++ d.s!Nom ++ to_agenda);


oper to_agenda : Str = optStr ["in the agenda"];

-- action delete
lin

delete = reqVP agenda_delete;


-- action move

move_booking = reqVP agenda_move;

-- action change_time

change_time = variants { reqVP agenda_change_time;
                         reqVP agenda_change_time__booking};

-- action change_date

change_date = variants { reqVP agenda_change_date;
                         reqVP agenda_change_date__booking};




}

