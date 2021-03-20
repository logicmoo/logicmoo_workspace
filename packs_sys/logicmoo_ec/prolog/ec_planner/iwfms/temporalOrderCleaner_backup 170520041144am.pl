%?-use_module(library(lists)).



%go(Result):-
%cleanTemporalOrderings([[happens(createFormelement(restrictedDrugs,radiobtn,[size=2]),t149,t149),happens(entry(form,element,guard),t121,t121),happens(edgeProgression(form2,formElement(test2,value)),t132,t132),happens(formSubmission(form2),t129,t129),happens(entry(form,element,woobe),t120,t120),happens(formEntry(form2,jw99),t125,t125),happens(formEntry(form1,jw99),t8,t8),happens(createFormelement(textboxform1,textbox,[multline,numlines(4)]),t115,t115),happens(formSubmission(form1),t14,t14),happens(edgeProgression(form1,formElement(correctDrugs,off)),t108,t108)],[before(t149,t136),before(t138,t149),before(t115,t146),before(t148,t115),before(t142,t108),before(t146,t14),before(t8,t148),before(t14,t142),before(t134,t140),before(t136,t129),before(t125,t138),before(t129,t134),before(t134,t132),before(t132,t131),before(t131,t),before(t125,t131),before(t107,t125),before(t121,t119),before(t120,t118),before(t119,t14),before(t115,t118),before(t118,t119),before(t115,t112),before(t114,t115),before(t110,t108),before(t112,t14),before(t8,t114),before(t14,t110),before(t108,t107),before(t8,t107)]], Result), write(Result).



cleanTemporalOrderings([Plan,Ordering], ValidOrderings):-
	fetchTemporalReferences(Plan, TempRefs),!,
	append([t], TempRefs, FinalTempRefs),
	cleanTemp(FinalTempRefs, Ordering, ValidOrderings), 
	
	% Cut required when generating multiple solutions to avoid
	% backtrack possible cleaning options
	!.

	
	
	
	
cleanTemp( TempRefs, [], []).	
cleanTemp( TempRefs, [before(X,Y) | Tail] ,[ before(X,Y) | TailTemp]):-
	member(X, TempRefs),
	member(Y, TempRefs),
	cleanTemp( TempRefs, Tail,TailTemp ).	
	
cleanTemp( TempRefs, [before(X,Y) | Tail] ,[ before(X,Unknown ) | TailTemp ] ):-
	
	member(X,TempRefs),
	\+ member(Y, TempRefs),	
	
	member(before(Y, Unknown ), Tail),
	
	cleanTemp( TempRefs, Tail,TailTemp ).		

cleanTemp( TempRefs, [before(X,Y) | Tail] , [ before(Unknown,Y ) | TailTemp] ):-
	member(Y,TempRefs),
	\+ member(X, TempRefs),	
	
	member(before(Unknown,X), Tail),
		
	cleanTemp( TempRefs, Tail,TailTemp ).		

%delete rudundant temporal ordering
cleanTemp( TempRefs, [before(X,Y) | Tail] , TailTemp ):-
	cleanTemp( TempRefs, Tail,TailTemp ).		
		
	
	
fetchTemporalReferences([] ,[]).
fetchTemporalReferences([happens(A,F,T)|Tail], [ T | TempRefs]):-
	fetchTemporalReferences(Tail, TempRefs).

fetchTemporalReferences([before(X,Y)|Tail], [ T | TempRefs]):-
	fetchTemporalReferences(Tail, TempRefs).
	
		
clean(Z):-cleanTemp([t75,t45,t58,t29,t34],[before(t75,t92),before(t92,t34),before(t29,t75),before(t58,t60),before(t60,t29),before(t45,t58),before(t34,t)],Z).