%:-prolog_flag(compiling,_,profiledcode).
%?-use_module(library(lists)).



reduceLoops(Actions, Ordering, FinalOrdering):-

 %locate any loops which exist
 member(  happens(edgeProgression(loop(LoopDestination), _),LoopCallTimepoint,LoopCallTimepoint)  ,  Actions ),
 %Find the destination they loop to
 !, matchHappens(LoopDestination,Timepoint,DestinationTimepoint, Actions),
 removeGoalAchievement(LoopCallTimepoint, Ordering, NewOrdering),
 addLoopAchievement(DestinationTimepoint,LoopCallTimepoint, NewOrdering, FinalOrdering ).
 
%If We ever fail the plan does not have a loop in it so return the same Ordering
reduceLoops(_, Ordering, Ordering).


reduceLoops(In,Out):- reduceLoops(In,In,Out).
 
 
removeGoalAchievement(LoopCallTimepoint, Ordering, NewOrdering):-

 delete(Ordering,b(LoopCallTimepoint,t), NewOrdering),!.

  
addLoopAchievement(DestinationTimepoint,LoopCallTimepoint, Ordering, NewOrdering):-
 append([b(LoopCallTimepoint,DestinationTimepoint)], Ordering, NewOrdering), !.
 

listMember([], CheckList) :- !, fail. 
 
listMember([Head|Tail], CheckList):-
    
    member(Head, CheckList), !.

listMember([Head|Tail], CheckList):-
    !, listMember(Tail, CheckList), !.
  
  

% --------------Tests ----------------------

%simple plan 
test_reduceLoops :- reduceLoops([happens(formEntry(assessForm,user,group), t6,t6),happens(edgeProgression(loop(formEntry(assessForm,user,group)), formElement( test2 ,match, off )),t9,t9)], [b(t9,t),b(t2,t)]).

%complex plan
test1_reduceLoops :- reduceLoops([happens(formEntry(admissionForm,jw99,receptionist),t12,t12),happens(createFormElement(textbox,patientName,'',[size=30]),t17,t17),happens(createFormElement(textbox,houseNumber,'',[size=100]),t18,t18),happens(createFormElement(textbox,road,'',[size=100]),t19,t19),happens(createFormElement(textbox,age,'',[size=100]),t20,t20),happens(createFormElement(textbox,ward,'',[size=100]),t21,t21),happens(entry(form,element,childddddddd),t29,t29),happens(entry(form,elementss,level1childrensward),t30,t30),happens(formSubmission(admissionForm),t16,t16),happens(edgeProgression(admissionForm,formElement(patientName,presence,none)),t33,t33),happens(formEntry(assessForm,jw99,doctor),t37,t37),happens(createFormElement(textarea,drugs,'',[rows=30,cols=30]),t42,t42),happens(formSubmission(assessForm),t41,t41),happens(edgeProgression(assessForm,formElement(drugs,presence,none)),t45,t45),happens(formEntry(checkdrugs,jw99,pharmacist),t49,t49),happens(createFormElement(checkbox,correctDrugs,true,[size=2]),t58,t58),happens(createFormElement(checkbox,doctorDrugs,database,[size=2]),t59,t59),happens(databaseFetch(drugsSpec),t55,t55),happens(formSubmission(checkdrugs),t57,t57),happens(edgeProgression(checkdrugs,formElement(correctDrugs,match,false)),t62,t62),happens(formEntry(failedDrugsApproval,jw99,doctor),t66,t66),happens(createFormElement(textbox,patientNameFailed,'',[multline]),t71,t71),happens(formSubmission(failedDrugsApproval),t70,t70),happens(edgeProgression(loop(formEntry(assessForm,_,_)),formElement(test2,match,off)),t77,t77)],[b(t66,t71),b(t71,t70),b(t49,t58),b(t59,t55),b(t58,t59),b(t55,t57),b(t37,t42),b(t42,t41),b(t12,t17),b(t20,t21),b(t19,t20),b(t18,t19),b(t17,t18),b(t70,t77),b(t77,t),b(t57,t62),b(t62,t66),b(t41,t45),b(t45,t49),b(t16,t33),b(t33,t37),b(t30,t16),b(t21,t29),b(t29,t30)]).
