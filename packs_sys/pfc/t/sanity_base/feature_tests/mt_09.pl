/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(mt_09,[]).

:- include(test_header).

:- pfc_test_feature(pfc_file_detect,must(\+ is_pfc_file)).

%:- add_import_module(mt_01,baseKB,end).

:- set_defaultAssertMt(myMt).

mtProlog(code1).
mtHybrid(kb2).

/*
;; All living people have an organ
(forall ((p PersonAlive)) (exists ((o Organ)) (have p o)))
;; An organ exists in which every living Persons ever will exists
(exists ((o Organ)) (forall ((p PersonAlive)) (co-temporal p o)))


Maybe people dont even exist?  Wait.. 'co-temporal' does this mean we have created People?
Have we created people with Eternal life as well?  At least until the Eternal Organ is no more?

To make my question fair 

(forall (x t) 
 (if 
  (and (ist t (instance x TemporalObject))
       (co-temporal x y))
    (ist t (exists (y) (instance y TemporalObject))))
  
(forall (x y) (iff (co-temporal x y)  (co-temporal y x)))
    
*/

code1: (a:- printAll('$current_source_module'(_M)),b).

kb2: (b).

genlMt(kb2,code1).


% before test, to make sure a was not accdently defined in kb2
:- sanity( clause(kb2:a,_)).

% before test, genlMt makes the rule available and should not corrupt the code1 module
:- sanity(\+ clause(code1:b,_)).

% make sure genlMt didnt unassert 
:- sanity(clause(kb2:b,_)).



% run the test
kb2: (?- a).


% to make sure a does not get accdently defined in kb2
:- mpred_must( clause(kb2:a,_)).

% genlMt makes the rule available and should not corrupt the code1 module
:- mpred_must(\+ clause(code1:b,_)).

% genlMt 

:- mpred_must(\+ clause(code1:b,_)).
