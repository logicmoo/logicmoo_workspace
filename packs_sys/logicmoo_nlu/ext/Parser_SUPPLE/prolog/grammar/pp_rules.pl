% Prepositional Phrase rules
%

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.


% Best Parse Categories
best_parse_cats([pp,bnp]).

% PP semantics conventions:
%     sem:E^X^[[R,E,X],NPSem]
%     X is the entity from the NP
%     E is the entity to which the PP will attach
%     R is the mroot of the PP
%     NOTE: the triple [R,E,X] MUST occur first in the sem list --
%     higher level semantic rules use this convention to check R

/*
%% PP -> IN BNP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [in(m_root:R),
     bnp(sem:X^NPSem)]).

%% PP -> TO BNP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [to(m_root:R),
     bnp(sem:X^NPSem)]).
*/

%% PP -> IN NP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [in(m_root:R),
     np(sem:X^NPSem)]).

%% PP -> TO NP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [to(m_root:R),
     np(sem:X^NPSem)]).

%in -> next to
rule(in(m_root:next_to),
    [jj(m_root:next),
     to(m_root:to)]).
%in -> adjacent to
rule(in(m_root:adjacent_to),
    [jj(m_root:adjacent),
     to(m_root:to)]).
%in -> close to
rule(in(m_root:close_to),
    [jj(m_root:close),
     to(m_root:to)]).
%in -> near
%rule(in(m_root:in),
%    [in(m_root:near)]).





