:- if((prolog_load_context(source,S),prolog_load_context(file,S))).

:- module(lps_syntax,[
     op(900,fy,(not)), 
     op(1200,xfx,(then)),
     op(1185,fx,(if)),
     op(1190,xfx,(if)),
     op(1100,xfy,else), 
     op(1050,xfx,(terminates)),
     op(1050,xfx,(initiates)),
     op(1050,xfx,(updates)),
% Rejected    (      op(1050,fx,impossible), 
     op(1050,fx,(observe)),
     op(1050,fx,(false)),
     op(1050,fx,initially),
     op(1050,fx,fluents),
     op(1050,fx,events),
     op(1050,fx,prolog_events),
     op(1050,fx,actions),
     op(1050,fx,unserializable),
% notice ',' has priority 1000
     op(999,fx,update),
     op(999,fx,initiate),
     op(999,fx,terminate),
     op(997,xfx,in),
     op(995,xfx,at),
     op(995,xfx,during),
     op(995,xfx,from), 
     op(994,xfx,to), % from's priority higher
     op(1050,xfy,::),

% lps.js syntax extras
     op(1200,xfx,(<-)),
     op(1050,fx,(<-)),
% -> is already defined as 1050, xfy, which will do given that lps.js does not support if-then-elses
     op(700,xfx,(<=))
  ]).

:- endif.

% Surface syntax .pl
:- op(900,fy,(not)). 
:- op(1200,xfx,(then)).
:- op(1185,fx,(if)).
:- op(1190,xfx,(if)).
% the following is for "conditional expressions", (if C then T else E)
% this maybe slightly confusing versus (if Antecedent then Consequent) rules, 
% and other choices would be possible, e.g. Python's or C et.al... but this still feels
% easier on newcomers, hoping that context (and the obligation to use parenthesis) helps:
:- op(1100,xfy,else). 
:- op(1050,xfx,(terminates)).
:- op(1050,xfx,(initiates)).
:- op(1050,xfx,(updates)).
% Rejected:-( :- op(1050,fx,impossible). 
:- op(1050,fx,(observe)).
:- op(1050,fx,(false)).
:- op(1050,fx,initially).
:- op(1050,fx,fluents).
:- op(1050,fx,events).
:- op(1050,fx,prolog_events).
:- op(1050,fx,actions).
:- op(1050,fx,unserializable).
% notice ',' has priority 1000
:- op(999,fx,update).
:- op(999,fx,initiate).
:- op(999,fx,terminate).
:- op(997,xfx,in).
:- op(995,xfx,at).
:- op(995,xfx,during).
:- op(995,xfx,from). 
:- op(994,xfx,to). % from's priority higher
:- op(1050,xfy,::).

% lps.js syntax extras
:- op(1200,xfx,(<-)).
:- op(1050,fx,(<-)).
% -> is already defined as 1050, xfy, which will do given that lps.js does not support if-then-elses
:- op(700,xfx,(<=)).



