
:- style_check(-discontiguous).

has_household(timSmith,household1).
has_household(elaineSmith,household1).

has_relative(timSmith,billSmith).
has_relative(timSmith,kateSmith).
has_friend(timSmith,bobGoulda).
has_relative(elaineSmith,sarahSteinman).
has_friend(elaineSmith,jessicaMasterman).

lives_nearby(timSmith,billSmith).
lives_nearby(timSmith,bobGoulda).
lives_nearby(elaineSmith,jessicaMasterman).

age(bobGoulda,50).
has_underlying_chronic_health_condition(bobGoulda).
age(timSmith,45).
age(elaineSmith,50).
age(jessicaMasterman,60).

%% types(room,cleaningSpray,cleaningWipe,bathroom,householdItem,gloves,facemask,person,quarter,shoppingBags).
