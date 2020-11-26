%?-use_module(library(lists)).
%:-prolog_flag(compiling,_,profiledcode).

cleanTemporalOrderings([Plan,Ordering], ValidOrderings):-
	fetchTemporalReferences(Plan, TempRefs),!,
	append([t], TempRefs, FinalTempRefs),
	cleanTemp(FinalTempRefs, Ordering, ValidOrderings), 
	
	% Cut required when generating multiple solutions to avoid
	% backtrack possible cleaning options
	!.
	
cleanTemp( TempRefs, [], []).	
cleanTemp( TempRefs, [b(X,Y) | Tail] ,[ b(X,Y) | TailTemp]):-
	member(X, TempRefs),
	member(Y, TempRefs),
	cleanTemp( TempRefs, Tail,TailTemp ).	
	
cleanTemp( TempRefs, [b(X,Y) | Tail] ,[ b(X,Unknown ) | TailTemp ] ):-
	
	member(X,TempRefs),
	\+ member(Y, TempRefs),	
	
	member(b(Y, Unknown ), Tail),
	
	cleanTemp( TempRefs, Tail,TailTemp ).		

cleanTemp( TempRefs, [b(X,Y) | Tail] , [ b(Unknown,Y ) | TailTemp] ):-
	member(Y,TempRefs),
	\+ member(X, TempRefs),	
	
	member(b(Unknown,X), Tail),
		
	cleanTemp( TempRefs, Tail,TailTemp ).		

%delete rudundant temporal ordering
cleanTemp( TempRefs, [b(X,Y) | Tail] , TailTemp ):-
	cleanTemp( TempRefs, Tail,TailTemp ).		
	
fetchTemporalReferences([] ,[]).
fetchTemporalReferences([happens(A,F,T)|Tail], [ T | TempRefs]):-
	fetchTemporalReferences(Tail, TempRefs).

fetchTemporalReferences([b(X,Y)|Tail], [ T | TempRefs]):-
	fetchTemporalReferences(Tail, TempRefs).


% --------------Tests ----------------------

test1_cleanTemporalOrderings(Result):-
 cleanTemporalOrderings([[happens(createFormelement(restrictedDrugs,radiobtn,[size=2]),t149,t149),happens(entry(form,element,guard),t121,t121),happens(edgeProgression(form2,formElement(test2,value)),t132,t132),happens(formSubmission(form2),t129,t129),happens(entry(form,element,woobe),t120,t120),happens(formEntry(form2,jw99),t125,t125),happens(formEntry(form1,jw99),t8,t8),happens(createFormelement(textboxform1,textbox,[multline,numlines(4)]),t115,t115),happens(formSubmission(form1),t14,t14),happens(edgeProgression(form1,formElement(correctDrugs,off)),t108,t108)],[b(t149,t136),b(t138,t149),b(t115,t146),b(t148,t115),b(t142,t108),b(t146,t14),b(t8,t148),b(t14,t142),b(t134,t140),b(t136,t129),b(t125,t138),b(t129,t134),b(t134,t132),b(t132,t131),b(t131,t),b(t125,t131),b(t107,t125),b(t121,t119),b(t120,t118),b(t119,t14),b(t115,t118),b(t118,t119),b(t115,t112),b(t114,t115),b(t110,t108),b(t112,t14),b(t8,t114),b(t14,t110),b(t108,t107),b(t8,t107)]], Result), write(Result).
		
test2_clean(Z):-cleanTemp([t75,t45,t58,t29,t34],[b(t75,t92),b(t92,t34),b(t29,t75),b(t58,t60),b(t60,t29),b(t45,t58),b(t34,t)],Z).