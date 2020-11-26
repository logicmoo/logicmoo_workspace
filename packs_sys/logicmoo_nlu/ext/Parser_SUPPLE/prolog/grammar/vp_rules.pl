% Verb Phrase rules
%

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

% Best Parse Categories
best_parse_cats([fvp,nfvp,np,pp]).
%

%% FVP -> VP(tensed)
rule(fvp(edge:Edge,person:P,number:N,tense:past,voice:V,m_root:R,
    sem:E^[VSem,[realisation,E,Edge]]),
    [vp(m_root:R,voice:V,person:P,number:N,tense:past,sem:E^VSem)]).
rule(fvp(edge:Edge,person:P,number:N,tense:present,voice:V,m_root:R,
    sem:E^[VSem,[realisation,E,Edge]]),
    [vp(m_root:R,voice:V,person:P,number:N,tense:present,sem:E^VSem)]).

%% NFVP -> VP(untensed)
% may overgenerate for -ing forms when preceding auxiliaries are not
% caught
rule(nfvp(edge:Edge,person:P,number:N,tense:none,voice:V,vform:F,m_root:R,
    sem:E^[VSem,[realisation,E,Edge]]),
    [vp(m_root:R,voice:V,person:P,number:N,vform:F,tense:none,sem:E^VSem)]).

rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,m_root:R,
    sem:E^[VSem,[realisation,E,Edge]]),
    [vpcore(m_root:R,voice:V,person:P,number:N,tense:T,vform:base,sem:E^VSem)]).

%attach NP complement as object
%% VP -> VPCORE NP
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:active,m_root:R,
    sem:E^X^[VSem,[lobj,E,X],NPSem,[realisation,E,Edge]]),
    [vpcore(voice:active,m_root:R,person:P,number:N,tense:T,sem:E^VSem),
     np(sem:X^NPSem)]).

% attach VPINF complement as object -- note there is no way to link
% subjects here -- must be done in DI (which should also note that
% the complement event has a time(E,none) attribute
%% VP -> VPCORE VP(to)
%  planned to launch
%  is scheduled to launch
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,m_root:R,
    sem:E1^E2^[VSem,[infcomp,E1,E2],InfSem,[realisation,E,Edge]]),
    [vpcore(voice:V,m_root:R,person:P,number:N,tense:T,sem:E1^VSem),
     nfvp(vform:base,sem:E2^InfSem)]).

%% VP -> be PREMOD
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:active,m_root:R,
    sem:E^X^[VSem,[adj,E,X],NPSem,[realisation,E,Edge],[realisation,X,AdjEdge]]),
    [vpcore(voice:active,m_root:be,person:P,number:N,tense:T,sem:E^VSem),
     premod(edge:AdjEdge,sem:X^NPSem)]).

%% VP -> passive-VPCORE PP
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:passive,m_root:R,
    sem:E^Subj^[VSem,[lsubj,E,Subj],[[by,E,Subj],PPNPSem],
    [realisation,E,Edge]]),
    [vpcore(voice:passive,m_root:R,person:P,number:N,tense:T,sem:E^VSem),
     pp(sem:E^Subj^[[by,E,Subj],PPNPSem])]).


%% VP -> VP CC VP
rule(vp(edge:Edge,sem:E^X^[VPSem1,VPSem2,[coord,E,X],[realisation,E,Edge]]),
    [vp(sem:E^VPSem1),
     cc(_),
     vp(sem:X^VPSem2)]).

%% VP -> TO VP
%rule(vp(sem:VPSem),
%    [to(_),
%     vp(sem:VPSem)]).


%%attach a PP complement, binding the PP to the verb
%%VP -> VPCORE PP
rule(vp(edge:Edge,person:P,number:N,tense:T,voice:V,m_root:R,
    sem:E^Head^[VSem,PPSem,[realisation,E,Edge]]),
    [vpcore(voice:V,m_root:R,person:P,number:N,tense:T,sem:E^VSem),
     pp(sem:E^Head^PPSem)]).

% look inside pp if necessary...
%    pp(sem:E^Head^[[of,E,Head]|PPSem])

