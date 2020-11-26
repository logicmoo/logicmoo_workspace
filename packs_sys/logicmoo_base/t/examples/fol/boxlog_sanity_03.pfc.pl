:- include(test_header).

% :- process_this_script.

% Initially Dmiles thought LEM was the culprit, it was not.
% this is a more generalized problem in Nomics

% ==============================================================
% Section 1:  Problem with Nomics
% ==============================================================

% Turn off modal extensions (default was full)
:- set_prolog_flag(logicmoo_modality,none).

% Rule 1: If no cute puppies exist, then Joan will buy a horse  (authored by Joan)
~exists(X,cute_puppy(X)) => buys(joan,horse).

% Rule 2: It is impossible for broke people to buy things  (authored by a Shop Keeper)
forall([P,A], broke(P)=> ~buys(P,A)).

% Fact A: Joan is a broke person  (authored by Joan)
broke(joan).
% I mean we need Joan to vote yes on propositon #2?

% Expose the problem
:- break.
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
:- mpred_test(buys(paul,What)).

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
test_1_take:-
  mpred_test(\+ cute_puppy(_)),
  mpred_test(\+ ~cute_puppy(_)),
  mpred_test(~buys(joan,horse)).

% Additionally we wanted to ensure
test_2_take:- 
 nop(mpred_test(
   \+ nesc( 
      ~exists(X,cute_puppy(X)) => buys(joan,horse)))).



% Turn modal extensions back on:
:- set_prolog_flag(logicmoo_modality,full).

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


test_setup:-
% forget everything we thought we knew!
 mpred_reset_kb,
% ensure empty KB and start over
 listing(header_sane:_),
% Rule 1: If no cute puppies exist, then Joan will buy a horse 
 test_assert(~exists(X,cute_puppy(X)) => buys(joan,horse)),
% Rule 2: It is impossible for broke people to buy things,
 test_assert(forall([P,A], broke(P)=> ~buys(P,A))),
 % Fact A: Joan is a broke person
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






%TODO Finish these below

% ==============================================================
% Section 4:  - Part MODALITY (optimization of the above)
% ==============================================================

% Turn modal extensions to partial
:- set_prolog_flag(logicmoo_modality,part).


% ==============================================================
%
%     P -> Q
%
% into 
%
%  ([_]P & <>Q) -> [_]Q
%
%
% ==============================================================
%
%     ~P 
%
% into 
%
%   ~<>P
%
% ==============================================================
%
%
%     P 
%
% into 
%
%  <>P -> [_]P
%


:- test_setup.

:- test_1_take.

:- test_2_take.






end_of_file.

* https://pdfs.semanticscholar.org/157c/8026af7e44ada20b98fc3c6321dc62db8ffb.pdf
* http://www.sciencedirect.com/science/article/pii/S0004370210000500
* http://www.ucl.ac.uk/infostudies/rob-miller/modular-e/cs05long.pdf
* Lookup Circumscription
* Lookup Implementation of Lukasiewicz's, Kleene's and McCarthy's 3-valued Logics 1975
* Lookup MODULAR-E and the role of elaboration tolerance in solving the qualification problem

CUTE STORY:
before i even knew whom McCarthy was, i was at in a coffee shop, enjoying a Telcommute.
And it happened to be at the Starbucks close to Stanford in which he enjoys:
My back to him, he somehow recognised the software i was working on my laptop which happened 
a system he created (before i was hired (to improve upon his system?!))
maybe as a joke or to keep me from screwing his work up. 
He decided to lead with: 
"Years ago I created a set of challenges the computers would need to do logically"
(rattling off what logicians needed to make computers do next for society so I thought they were Asimovs laws...
At least i didnt humilate myself by rolling my eyes!)
This led into a fun debate (I hope it was for him as well)
When John left, a few people came up to me and told me whom he was..
... the creator of the entire field (in which i was debating).


See common sense informatic situation Relevant history of logic Problems with logical AI Nonmonotonic reasoning
Domain dependent control of reasoning Concepts as objects Contexts as objects 
Partially defined objects Self-awareness Remarks and references LOGICAL AI Logical AI proposes 
computer systems that represent what they know about the world by sentences in a suitable mathematical logical language. 
It achieves goals by inferring that a certain strategy of action is appropriate to achieve the goal. 
It then carries out that strategy, using observations also expressed as logical sentences.


