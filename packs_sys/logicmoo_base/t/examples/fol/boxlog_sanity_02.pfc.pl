

:- include(test_header).

:- ensure_loaded(library(script_files)).

:- process_this_script.

% Initially Dmiles thought LEM was the culprit, it was not.
% this is a more generalized problem in Nomics

% ==============================================================
% Section 1:  Nomics Problem
% ==============================================================

% Turn off modal extensions (default was full)

%:- rtrace.
:- set_kif_option(qualify_modality,none).
:- must(kif_option_value(qualify_modality,none)).
%:- break.


% :- fully_expand_into_cache(change(assert,assert_u),~exists(X, cute_puppy(X))=>buys(joan, horse),_O).

% Rule 1: If no cute puppies exist, then Joan will buy a horse  (authored by Joan)
:- test_boxlog([+assert], 
  ~exists(X,cute_puppy(X)) => buys(joan,horse)).




% Rule 2: It is impossible for broke people to buy things  (authored by a Shop Keeper)
:- test_boxlog([+assert], 
  forall([P,A], 
    broke(P) => ~ buys(P,A))).

% Fact A: Joan is a broke person  (authored by Joan)
:- test_boxlog([+assert], 
  nesc(broke(joan))).



end_of_file.


% Expose the problem
:- mpred_test(cute_puppy(_)).
 /*

What = skIsCutepuppyExists_0FnSk ;

% Result: True
% Proof1:
%   broke(joan)
%   ~buys(joan,_)
%   exists(X,cute_puppy(X))

'' :-
        proven_not_nesc(buys(joan, horse)).
'' :-
        proven_not_nesc(cute_puppy(skIsCutepuppyExists_0FnSk)).
cute_puppy(_33699386) :-
        fwc,
        proven_not_nesc(cute_puppy(skIsCutepuppyExists_0FnSk)),
        { _33699386=skIsCutepuppyExists_0FnSk
        },
        proven_not_nesc(buys(joan, horse)),
        { is_unit(_33699386)
        }.
'' :-
        mfl(header_sane,
            '/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_02.pfc.pl',
            17).
*/


% yet, Joan could not afford a horse, regardless of the existence cute puppies
%
% I mean we need Joan to vote yes on propositon #2?
%
% How could it have been prevented?
%
% Firstly the very first rule could have qualified:
%  "If Joan is not broke,..."
%
% Why did we not?
%
%  Financial solvency was a property and we didn''t know about money at the time Joan authored the first rule
%
%  If the first rule was compiled int the system could we have tempered the second rule with:
%  "For everyone except joan, ..." ????
%  


% ==============================================================
% Section 2: Exasterbation
% ==============================================================

:- if(false).

% Rule 3:  If a cute puppy exists than Paul will buy it
exists(X,cute_puppy(X)) => buys(paul,X).

% A cute puppy was created in Section 1
:- mpred_test(buys(paul,_What)).

/*
'' :-
        cute_puppy(skIsCutepuppyExists_0FnSk).
buys(paul, _33453090) :-
        fwc,
        cute_puppy(_33453090),
        { is_unit(_33453090)
        }.
'' :-
        mfl(header_sane,
            '/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/boxlog_sanity_02.pfc.pl',
            73).
What = skIsCutepuppyExists_0FnSk.
*/


:-endif.


/*

 <dmiles> (to assume the existence of puppies in my example is even descidable based on here inability to buy a horse would definately be a logical error)
 <dmiles> but the system had no evidence at the time Rule #1 is stated that it should have rejected it
 <dmiles> Financial solvency was a property and we didn''t know about money at the time Joan authored the first rule
 <dmiles> some current systems SYS-01 would have rejected Fact A
 <dmiles> since the conflict only becomes appearent when Fact A is stated
 <dmiles> the way some System handles this, is SYS-02: creates three differnt possible worlds.. each containing only two of the statments
 <dmiles> ideally (it didnt happen in this case) the world with the most statements wins
 <dmiles> but that would happen later probably
 <dmiles> another way of handling this is SYS-03: comparing the worlds that contain the maximal number of represented authors
 <dmiles> (so the Joan only world would supply the least utility)
 <dmiles> I have an answer the the problem.. still i rather know the alternatives 
 <dmiles> (dispite the Joan only world would supply the least utility under SYS-03 managment of concensous priniples, it still is more likely to have the least contextual errors (as Joan is likely to have authored things from a single point of view))

 In summary:

 SYS-01: TMS would have rejected Fact A on the grounds that   ~buy(joan,horse) & buy(joan,horse) is true

 SYS-02: creates MTs for differnt possible worlds.. each containing a buildup of statements that dont disagree (block each Mt from seeing each other)
     but prioritize access based on largest MT

 SYS-03: Like SYS-02, but prioritize access based on "largest number of authors" (maximum consensus)

 SYS-04: Like SYS-02, but prioritize access based on "least number of authors" (based on principle authors will be consistent at least with themselves)

 SYS-05: "lazy skolem" dont forward chain the existence of cute puppy .. complain only if query happens (just dont let it be create and dail queries involving it)

 SYS-06: "Isolated skolem"  forward chain the existence of cute puppy in its own MT 
   (outside MT, Veto terms containing skolem if SYS-05 compliant occurs) 
     
 SYS-07:  "~buy(joan,horse) & buy(joan,horse)"  means disable buy/2 predicate is disabled Idni (Tau/Ohad suggests this)

 SYS-08:  ProbLog = weaken the acceptable truth value in buy/2 (NARS/Pei Wang)  aka "turd polishing"

 SYS-09:  Splitting buy into two predicates on "not_buy" "buy" (Mark Stickel)   not_buy means something different thant ~buy

 SYS-10:  Like SYS-09 + softly lemmatize "not_buy(X,Y) <=> ~buy(X,Y)"  (softly is have a mode that ignore this .. mark chians using this rule as possibly invalid when deductions confict)

  ... Sure there are many more!

*/




% ==============================================================
% Section L: LogicMOO TEST 1
% ==============================================================


% Effectively we wanted to ensure
test_1_take:- cwc,
  mpred_test(\+ cute_puppy(_)),
  mpred_test(\+ ~cute_puppy(_)),
  nop(mpred_test(proven_neg(buys(joan,horse)))),!.


% Additionally we wanted to ensure
test_2_take:- cwc,
 (mpred_test(
   \+ nesc( 
      ~exists(X,cute_puppy(X)) => buys(joan,horse)))).



% Turn modal extensions back on:
:- set_kif_option(qualify_modality,full).

% this secretly rewrites:
%
%     P 
%
% into 
%
%  <>P -> [_]P
%
% (Defeasablity "compiled in")

/*

Alternatively we can allow strong negastion to control:

[_]P -> <>P

since [_]~P <-> ~<>P

*/



% forget everything we thought we knew! (ensure empty KB and start over)
% Rule 1: If no cute puppies exist, then Joan will buy a horse 
% Rule 2: It is impossible for broke people to buy things,
% Fact A: Joan is a broke person

test_setup:- cwc,
 mpred_reset_kb,
 test_assert(~exists(X,cute_puppy(X)) => buys(joan,horse)),
 test_assert(forall([P,A], broke(P)=> ~buys(P,A))),
 test_assert(broke(joan)).


:- test_setup.

% so what logicmoo hears internally:
%
%   poss(~exists(X,cute_puppy(X)) => buys(joan,horse)) 
%     =>
%   nesc(~exists(X,cute_puppy(X)) => buys(joan,horse)).
%
%
% Fact A: Joan is a broke person + "if this fact is even possible"
% thus
%   Unit propagte: poss(broke(joan))=>nesc(broke(joan)).

% See if we passed Sanity

:- test_1_take.

:- test_2_take.


end_of_file.





