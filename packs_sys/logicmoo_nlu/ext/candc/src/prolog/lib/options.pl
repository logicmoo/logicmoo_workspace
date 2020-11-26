
:- module(options,[parseOptions/2,
                   candc_option/2,
                   setOption/3,
                   showOptions/1, 
                   setDefaultOptions/1]).

:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(library(lists),[member/2]).


/* =======================================================================
   Global dynamic predicates
========================================================================*/

:- dynamic candc_option/2.
 

/* =======================================================================
   Set Option
========================================================================*/

setOption(P,Option,Value):-
   assertOptions(P,[Option:Value]).


/* =======================================================================
   Check User Options
========================================================================*/

parseOptions(P,Arg):-
   opts(P,Options,Arg,[]), !,
   assertOptions(P,Options),
   dependentOptions(P).


/* =======================================================================
   Options Grammar
========================================================================*/

opts(_,[]) --> [].
opts(P,[O:do|L]) --> opt0(P,O), opts(P,L). 
opts(P,[O:V|L]) --> opt1(P,O), value(V), opts(P,L). 
opts(P,[O:true|L]) --> opt1(P,O), {candc_option(P,O,_,true,_)}, opts(P,L). 
opts(P,[O:V|L]) --> opt2(P,O), value(V), opts(P,L). 
opts(P,[O:V|L]) --> opt3(P,O), integer(V), opts(P,L). 
opts(P,[unknownoption:O|L]) --> opt4(P,O), opts(P,L). 
opts(P,[unknownoption:O|L]) --> opt4(P,O), value(_), opts(P,L). 
opts(P,[unknown:V|L]) --> value(V), opts(P,L). 

opt0(P,O) --> {candc_option(P,O,0,_,_)}, [O].
opt1(P,O) --> {candc_option(P,O,1,_,_)}, [O].
opt2(P,O) --> {candc_option(P,O,-1,_,_)}, [O].
opt3(P,O) --> {candc_option(P,O,-2,_,_)}, [O].
opt4(_,O) --> [O], {atom_chars(O,['-','-'|_])}.

value(V) --> [V], {atom_chars(V,[X,Y|_]),  \+ (X = '-', Y = '-')}.
value(V) --> [V], {atom_chars(V,[_])}.

integer(V) --> [Int], {atom_codes(Int,Codes), isInteger(Codes,0,V)}.


/* =======================================================================
   Check for Integer
========================================================================*/

isInteger([],Int,Int):- !.

isInteger([X|L],Old,Int):-
   X > 47, X < 58, !,
   New is (Old*10)+(X-48),
   isInteger(L,New,Int).


/* =======================================================================
   Dependent Options
========================================================================*/

dependentOptions(P):-
   findall(ifthen(A,B,C,D),dep(P,A,B,C,D),L),
   dependentOptions(L,P).

dependentOptions([],_):- !.

dependentOptions([ifthen(A,B,C,D)|L],P):-
   candc_option(A,B), !, 
   setOption(P,C,D),
   dependentOptions(L,P).

dependentOptions([_|L],P):-
   dependentOptions(L,P).



/* =======================================================================
   Assert Options
========================================================================*/

assertOptions(_,[]):-!.

assertOptions(P,[Option:do|L]):- 
   candc_option(P,Option,0,_,_), !,
   retract(candc_option(Option,_)),
   assert(candc_option(Option,do)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   candc_option(P,Option,-1,_,_), 
   atomic(Value), !,
   retract(candc_option(Option,_)),
   assert(candc_option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   candc_option(P,Option,-2,_,_), 
   number(Value), !,
   retract(candc_option(Option,_)),
   assert(candc_option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   atomic(Value), 
   candc_option(P,Option,1,Value,_), !,
   retract(candc_option(Option,_)),
   assert(candc_option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[unknownoption:Option|L]):- !, 
   error('candc_option ~p not supported',[Option]),
   assertOptions(P,L).

assertOptions(P,[unknown:Unknown|L]):- !,
   error('argument ~p not interpreted',[Unknown]),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   error('unknown value ~p for candc_option ~p',[Value,Option]), !,
   assertOptions(P,L).


/* =======================================================================
   Default Options
========================================================================*/

setDefaultOptions(P):- 
   retractall(candc_option(_,_)), 
   setof(Op,Ar^Val^Def^candc_option(P,Op,Ar,Val,Def),Options), 
   setDefaultOptions(Options,P).

setDefaultOptions([],_):- !.

setDefaultOptions([X|L],P):-  
   candc_option(P,X,_,_,D), !,
   assert(candc_option(X,D)),  
   setDefaultOptions(L,P).


/* =======================================================================
   Display Options
========================================================================*/

showOptions(P):-  
   ( setof(O,V^D^(candc_option(P,O,0,V,D),format(user_error,'  ~p~n',[O])),_), !; true ),
   ( setof(O,V^D^(candc_option(P,O,-1,V,D),format(user_error,'  ~p <file>~n',[O])),_), !; true ), 
   ( setof(O,V^D^(candc_option(P,O,-2,V,D),format(user_error,'  ~p <integer> (default: ~p)~n',[O,D])),_), !; true ),
   ( setof(o(O,D),V^candc_option(P,O,1,V,D),Options), !; true ),
   findall(_,( member(o(O,D),Options),
               findall(V,candc_option(P,O,1,V,_),L),
               format(user_error,'  ~p <arg> (possible values: ~p, default: ~p)~n',[O,L,D])),_), 
   nl(user_error).


/* =======================================================================
   Tokkie Options         % candc_option(Option,NumberArgs,Value,Default)
========================================================================*/

candc_option( tokkie, '--help',       0, _, dont       ).
candc_option( tokkie, '--version',    0, _, dont       ).
candc_option( tokkie, '--stdin',      0, _, dont       ).
candc_option( tokkie, '--warnings',   1, V, false      ):- member(V,[true,false]).
candc_option( tokkie, '--language',   1, V, en         ):- member(V,[en,it]).
candc_option( tokkie, '--quotes',     1, V, keep       ):- member(V,[keep,delete]).
candc_option( tokkie, '--mode',       1, V, poor       ):- member(V,[poor,iob,rich]).
candc_option( tokkie, '--format',     1, V, txt        ):- member(V,[prolog,txt]).
candc_option( tokkie, '--input',     -1, _, user_input ).
candc_option( tokkie, '--output',    -1, _, user_output).


/* =======================================================================
   Nutcracker Options         % candc_option(Option,NumberArgs,Value,Default)
========================================================================*/

candc_option( nutcracker, '--help',          0, _, dont      ).
candc_option( nutcracker, '--version',       0, _, dont      ).
candc_option( nutcracker, '--force',         1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--soap',          1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--modal',         1, V, false     ):- member(V,[true,false]).
%candc_option( nutcracker, '--vpe',           1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--plural',        1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--copula',        1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--resolve',       1, V, true      ):- member(V,[true,false]).
candc_option( nutcracker, '--nn',            1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--x',             1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--wordnet',       1, V, true      ):- member(V,[true,false]).
candc_option( nutcracker, '--warnings',      1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--info',          1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--graph',         1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--contradiction', 1, V, false     ):- member(V,[true,false]). % use theorem prover to check for contradictions
candc_option( nutcracker, '--wsd',           1, V, false     ):- member(V,[true,false]).
candc_option( nutcracker, '--roles',         1, V, proto     ):- member(V,[proto,verbnet,framenet]).  % ,e2c]).
candc_option( nutcracker, '--language',      1, V, en        ):- member(V,[en,it]).
candc_option( nutcracker, '--inference',     1, V, yes       ):- member(V,[yes,no,only]).
candc_option( nutcracker, '--tp',            1, V, bliksem   ):- member(V,[vampire,bliksem,otter]).
candc_option( nutcracker, '--mb',            1, V, mace      ):- member(V,[mace,paradox]).
candc_option( nutcracker, '--mbbis',         1, V, none      ):- member(V,[none,mace]).
candc_option( nutcracker, '--domsize',      -2, _, 50        ).
candc_option( nutcracker, '--timelim',      -2, _, 30        ).
candc_option( nutcracker, '--dir',          -1, _, 'working' ).
candc_option( nutcracker, '--axioms',       -1, _, 'none' ).


/* =======================================================================
   Boxer Options         % candc_option(Option,NumberArgs,Value,Default)
========================================================================*/

candc_option( boxer, '--help',       0, _, dont       ).
candc_option( boxer, '--version',    0, _, dont       ).
candc_option( boxer, '--stdin',      0, _, dont       ).
candc_option( boxer, '--resolve',    1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--integrate',  1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--warnings',   1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--instantiate',1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--ccg',        1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--elimeq',     1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--box',        1, V, false      ):- member(V,[true,false]).
%candc_option( boxer, '--vpe',        1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--nn',         1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--tense',      1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--modal',      1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--plural',     1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--x',          1, V, false      ):- member(V,[true,false]).
candc_option( boxer, '--copula',     1, V, true       ):- member(V,[true,false]).
candc_option( boxer, '--tokid',      1, V, local      ):- member(V,[local,global]).
candc_option( boxer, '--mwe',        1, V, no         ):- member(V,[no,yes,all]).
%candc_option( boxer, '--presup',     1, V, max        ):- member(V,[min,max]).
candc_option( boxer, '--theory',     1, V, drt        ):- member(V,[drt,sdrt]).
candc_option( boxer, '--roles',      1, V, proto      ):- member(V,[proto,verbnet,framenet]).  % ,e2c]).
candc_option( boxer, '--format',     1, V, prolog     ):- member(V,[prolog,xml,latex,dot,no]).
candc_option( boxer, '--semantics',  1, V, drs        ):- member(V,[drs,pdrs,fol,drg,amr,tacitus,der]).
candc_option( boxer, '--input',     -1, _, user_input ).
candc_option( boxer, '--output',    -1, _, user_output).


/* =======================================================================
   Dependent Options         % if O1:V1 then set O2:V2
========================================================================*/

dep(boxer, '--semantics',amr, '--elimeq',true). 
dep(boxer, '--semantics',amr, '--copula',false). 
dep(boxer, '--semantics',amr, '--modal',true). 
dep(boxer, '--semantics',amr, '--theory',sdrt). 
