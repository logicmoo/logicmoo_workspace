% Sentence rules
%

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.


% Best Parse Categories
best_parse_cats([s,sbar,relc,nfvp,nfvpcore,fvp,np,pp]).
%


%% S -> NP FVP
rule(s(sem:Event^Agent^[NPSem,VPSem,[lsubj,Event,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     fvp(voice:active,person:P,number:N,sem:Event^VPSem)]).

%% Brill tagger tags subject demonstrative pronouns as DT's
%% S -> DT(this/these) FVP
rule(s(sem:Event^Subj^[[pronoun,Subj,R],[number,Subj,DN],VPSem,
		       [realisation,Subj,Edge],[lsubj,Event,Subj]]),
    [dmp(edge:Edge,m_root:R,number:DN),
     fvp(voice:active,person:P,number:N,sem:Event^VPSem)]).

rule(dmp(s_form:F,number:sing,m_root:'this'),
     [dt(s_form:F,m_root:'this')]).
rule(dmp(s_form:F,number:plural,m_root:'these'),
     [dt(s_form:F,m_root:'these')]).

%% S -> NP passive-VP
rule(s(sem:Event^Agent^[NPSem,VPSem,[lobj,Event,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     fvp(voice:passive,person:P,number:N,sem:Event^VPSem)]).
 
%% S -> SBAR , S
% After leaving Sheffield, Kevin became rich.
rule(s(sem:E2^Agent^[SSem,SBAR,[lsubj,E1,Agent]]),
    [sbar(sem:E1^SBAR,vform:gform),
     comma(_), 
     s(sem:E2^Agent^SSem)
]).

% Though envied by his friends, Kevin became rich.
rule(s(sem:E2^Agent^[SSem,SBAR,[lobj,E1,Agent]]),
    [sbar(sem:E1^SBAR,vform:nform),
     comma(_),
     s(sem:E2^Agent^SSem)
]).
% Though Kevin left Sheffield, he became rich
rule(s(sem:E2^Agent^[SSem,SBAR]),
    [sbar(sem:SBAR),
     comma(_),
     s(sem:E2^Agent^SSem)
]).

%% S -> NP, SBAR , VP
% Kevin, after leaving Sheffield, became rich.
rule(s(sem:E2^Agent^[NPSem,VPSem,SBAR,[lsubj,E1,Agent],[lsubj,E2,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:E1^SBAR,vform:gform),
     comma(_),
     fvp(voice:active,person:P,number:N,sem:E2^VPSem)
]).
% Kevin, though envied by his friends, became rich.
rule(s(sem:E2^Agent^[NPSem,VPSem,SBAR,[lobj,E1,Agent],[lsubj,E2,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:E1^SBAR,vform:nform),
     comma(_),
     fvp(voice:active,person:P,number:N,sem:E2^VPSem)
]).
% Kevin, though he left Sheffield, became rich.
rule(s(sem:Event^Agent^[NPSem,VPSem,SBAR,[lsubj,Event,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:SBAR),
     comma(_),
     fvp(voice:active,person:P,number:N,sem:Event^VPSem)
]).
%% S -> NP, SBAR , VP
% Kevin, after leaving Sheffield, was knighted.
rule(s(sem:E2^Agent^[NPSem,VPSem,SBAR,[lsubj,E1,Agent],[lobj,E2,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:E1^SBAR,vform:gform),
     comma(_),
     fvp(voice:passive,person:P,number:N,sem:E2^VPSem)
]).
% Kevin, though envied by his friends, was knighted.
rule(s(sem:E2^Agent^[NPSem,VPSem,SBAR,[lobj,E1,Agent],[lobj,E2,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:E1^SBAR,vform:nform),
     comma(_),
     fvp(voice:passive,person:P,number:N,sem:E2^VPSem)
]).
% Kevin, though he left Sheffield, was knighted.
rule(s(sem:Event^Agent^[NPSem,VPSem,SBAR,[lobj,Event,Agent]]),
    [np(person:P,number:N,sem:Agent^NPSem),
     comma(_),
     sbar(sem:SBAR),
     comma(_),
     fvp(voice:passive,person:P,number:N,sem:Event^VPSem)
]).

%% S -> S, SBAR 
% Kevin became rich, after leaving Sheffield.
rule(s(sem:E2^Agent^[SSem,SBAR,[lsubj,E1,Agent]]),
    [s(sem:E2^Agent^SSem),
     comma(_),
     sbar(sem:E1^SBAR,vform:gform)
]).
% Kevin became rich, though envied by his friends.
rule(s(sem:E2^Agent^[SSem,SBAR,[lobj,E1,Agent]]),
    [s(sem:E2^Agent^SSem),
     comma(_),
    sbar(sem:E1^SBAR,vform:nform)
]).

% Kevin became rich, though he left Sheffield.
rule(s(sem:E2^Agent^[SSem,SBAR]),
    [s(sem:E2^Agent^SSem),
     comma(_),
     sbar(sem:SBAR)
]).


%% S -> Q
%rule(s(sem:QSem),
%     [q(sem:QSem)]).

% SBAR rules
%% SBAR -> SUBORD S
%rule(sbar(sem:E2^E^[[R,E2,E],[event,E2]], Sen]),
rule(sbar(sem:E2^E^[[R,E2,E]|Sen]),
    [subord(m_root:R),
     s(sem:E^Sen)]).

%% SBAR -> SUBORD NFVP(vform:gform)
% since leaving the country 
rule(sbar(sem:E2^E^[[R,E2,E]|VSem],vform:gform),
    [subord(m_root:R),
     nfvp(vform:gform,sem:E^VSem)]).
%% SBAR -> SUBORD NFVP(vform:nform)
% though defeated by his enemies
rule(sbar(sem:E2^E^[[R,E2,E]|VSem],vform:nform),
    [subord(m_root:R),
     nfvp(vform:nform,sem:E^VSem)]).

rule(subord(m_root:'that'),
    [in(m_root:'that')]).
rule(subord(m_root:'because'),
    [in(m_root:'because')]).
rule(subord(m_root:'by'),
    [in(m_root:'by')]).
rule(subord(m_root:'since'),
    [in(m_root:'since')]).
rule(subord(m_root:'if'),
    [in(m_root:'if')]).
rule(subord(m_root:'while'),
    [in(m_root:'while')]).
rule(subord(m_root:'though'),
    [in(m_root:'though')]).
rule(subord(m_root:'as'),
    [in(m_root:'as')]).
rule(subord(m_root:'whether'),
    [in(m_root:'whether')]).
rule(subord(m_root:'although'),
    [in(m_root:'although')]).
rule(subord(m_root:'after'),
    [in(m_root:'after')]).
rule(subord(m_root:'before'),
    [in(m_root:'before')]).
rule(subord(m_root:'on'),
    [in(m_root:'on')]).
rule(subord(m_root:'upon'),
    [in(m_root:'upon')]).
rule(subord(m_root:'when'),
    [wrb(m_root:'when')]).
rule(subord(m_root:'where'),
    [wrb(m_root:'where')]).
		       


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% rules which may have an S as an object

%only certain verbs take sententential complements.  These
%need to be explicitly marked strans here.
%consider using vpcore here prevents passive sentences like 
%It was said by John "go to the store".
rule(vp_strans(person:P,number:N,tense:T,voice:V, sem:S),
    [vpcore(m_root:say,person:P,number:N,tense:T,voice:V,sem:S)]).


%% VP -> VP_STRANS S
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^X^[VSem,[lobj,E,X],NPSem,[realisation,E,Edge]]),
    [vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem),
     s(sem:X^NPSem)]).

%% VP -> VPCORE SUBORD("that") S
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^[VSem,SSem,[proposition,E2],
	      [main_event,E2,E],[realisation,E,Edge]]),
    [vpcore(person:P,number:N,tense:T,voice:V,sem:E^VSem),
     subord(m_root:'that'),
     s(sem:E2^SSem)]).

%said "Sent"
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^[VSem,SSem,[proposition,E2],
	      [main_event,E2,E],[realisation,E,Edge]]),
    [vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem),
     sym(s_form:'``'),
     s(sem:E2^SSem),
     sym(s_form:'\'\'')
]).

%"Sent" said John
rule(s(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^Agent^[SSem,VSem,NPSem,[lsubj,E,Agent],[proposition,E2],
		    [main_event,E2,E],[realisation,E,Edge]]),
    [sym(s_form:'``'),
     s(sem:E2^SSem),
     sym(s_form:'\'\''),
     vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem),
     np(person:P,number:N,sem:Agent^NPSem)
]).

%"Sent," said John
rule(s(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^Agent^[SSem,VSem,NPSem,[lsubj,E,Agent],[proposition,E2],
		    [main_event,E2,E],[realisation,E,Edge]]),
    [sym(s_form:'``'),
     s(sem:E2^SSem),
     comma(s_form:','),
     sym(s_form:'\'\''),
     vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem),
     np(person:P,number:N,sem:Agent^NPSem)
]).

%"Sent" John said
rule(s(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^Agent^[SSem,NPSem,VSem,[lsubj,E,Agent],[proposition,E2],
		    [main_event,E2,E],[realisation,E,Edge]]),
    [sym(s_form:'``'),
     s(sem:E2^SSem),
     sym(s_form:'\'\''),
     np(person:P,number:N,sem:Agent^NPSem),
     vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem)
]).

%"Sent," John said
rule(s(edge:Edge,person:P,number:N,tense:T,voice:V,
    sem:E^E2^Agent^[SSem,NPSem,VSem,[lsubj,E,Agent],[proposition,E2],
		    [main_event,E2,E],[realisation,E,Edge]]),
    [sym(s_form:'``'),
     s(sem:E2^SSem),
     comma(s_form:','),
     sym(s_form:'\'\''),
     np(person:P,number:N,sem:Agent^NPSem),
     vp_strans(person:P,number:N,tense:T,voice:V,sem:E^VSem)
]).
