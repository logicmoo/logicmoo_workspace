:- prolog_use_module(library(julian)).

replaceFunctionalArgs(Item,Style,Result) :-
	viewIf([item,Item,style,Style]),
	Item =.. [Pred|Args],
	viewIf([args,Args]),
	findall(ArgPosition,(functionalInArgs(Pred,TmpArgPosition),ArgPosition is TmpArgPosition - 1),ArgPositions),
	viewIf([argPositions,ArgPositions]),
	length(Args,ArgsLength),
	viewIf([argsLength,ArgsLength]),
	length(ArgPositions,ArgPositionsLength),
	viewIf([argPositionsLength,ArgPositionsLength]),
	(   (	ArgPositionsLength > 0) ->
	    (	
		(   Style = variable ->
		    (	
			viewIf([argPositionsLength1,ArgPositionsLength]),
			length(ArgReplacements,ArgPositionsLength)
		    ) ;   
		    (	Style = blank ->
			(   
			    viewIf([argPositionsLength2,ArgPositionsLength]),
			    repl(blank,ArgPositionsLength,ArgReplacements)
			) ;   
			view([styleWTF,Style]))),
		viewIf([argReplacements,ArgReplacements]),
		replaceArgPositions(Args,ArgPositions,ArgReplacements,NewArgs),
		Result =.. [Pred|NewArgs]
	    ) ;
	    (	Result = Item)),
	viewIf([result,Result]).


atTimeQueryNow(Query) :-
	getCurrentDateTime(DateTime),
	atTimeQuery(DateTime,Query).

atTimeQuery(DateTime,Query) :-
	view([dateTime,DateTime]),
	view([query,Query]),
	view([findall(atTime(DateTime2,Query2),(atTime(DateTime2,Query2),Query = Query2,view([query,Query,query2,Query2,dateTime,DateTime,dateTime2,DateTime2]),compareTime(>,DateTime,DateTime2)),Results)]),
	findall(atTime(DateTime2,Query2),(atTime(DateTime2,Query2),Query = Query2,compareTime(>,DateTime,DateTime2)),Results),
	view([results,Results]),
	findall(Query4,(member(atTime(_,Query3),Results),replaceFunctionalArgs(Query3,blank,Query4)),TmpDistinctQueries),
	view([tmpDistinctQueries,TmpDistinctQueries]),
	setof(DistinctQuery,member(DistinctQuery,TmpDistinctQueries),DistinctQueries),
	view([distinctQueries,DistinctQueries]),
	member(TmpDistinctQuery,DistinctQueries),
	replaceFunctionalArgs(TmpDistinctQuery,variable,DistinctQuery),
	findall(atTime(A,DistinctQuery),member(atTime(A,DistinctQuery),Results),AllResults),
	predsort(compareAtTime,AllResults,SortedResults),
	view([sortedResults,SortedResults]),
	last(SortedResults,Result),
	view([result1,Result]),
	term_variables(Query,Vars),
	atTime(_,Query) = Result.


%% atTimeQuery(DateTime,Query) :-
%% 	atTimeQuery(DateTime,Query,true).

%% atTimeQuery(DateTime,Query,TruthValue) :-
%% 	view([dateTime,DateTime]),
%% 	view([query,Query]),
%% 	findall(atTime(DateTime2,Query2,TruthValue2),(atTime(DateTime2,Query2,TruthValue2),Query = Query2,Truthvalue = TruthValue2,compareTime(>,DateTime,DateTime2)),Results),
%% 	view([results,Results]),
%% 	findall(Query4,(member(atTime(_,Query3,TruthValue),Results),replaceFunctionalArgs(Query3,blank,Query4)),TmpDistinctQueries),
%% 	view([tmpDistinctQueries,TmpDistinctQueries]),
%% 	setof(DistinctQuery,member(DistinctQuery,TmpDistinctQueries),DistinctQueries),
%% 	view([distinctQueries,DistinctQueries]),
%% 	member(TmpDistinctQuery,DistinctQueries),
%% 	replaceFunctionalArgs(TmpDistinctQuery,variable,DistinctQuery),
%% 	findall(atTime(A,DistinctQuery,TruthValue),member(atTime(A,DistinctQuery,TruthValue),Results),AllResults),
%% 	predsort(compareAtTime,AllResults,SortedResults),
%% 	view([sortedResults,SortedResults]),
%% 	last(SortedResults,Result),
%% 	view([result1,Result]),
%% 	term_variables(Query,Vars),
%% 	atTime(_,Query) = Result.

compareAtTime(Order,atTime(T1,A),atTime(T2,B)) :-
	viewIf([a,A,b,B,order,Order]),
	compareTime(Order,T1,T2).

compareTime(>,TI1,TI2) :-
	fixTime(TI1,T1),
	fixTime(TI2,T2),
	julian:compare_time(>,T1,T2).
compareTime(<,TI1,TI2) :-
	fixTime(TI1,T1),
	fixTime(TI2,T2),
	julian:compare_time(<,T1,T2).
compareTime(=,TI1,TI2) :-
	fixTime(TI1,T1),
	fixTime(TI2,T2),
	julian:compare_time(=,T1,T2).

fixTime(TI1,T1) :-
	viewIf([t1,T1,t2,T2]),
	TI1 = [Y-M-D,H:Mi:S],
	viewIf([s,S]),
	(   not(integer(Y)) ->
	    (	
		atom_number(Y,Y1),
		atom_number(M,M1),
		atom_number(D,D1),
		atom_number(H,H1),
		atom_number(Mi,Mi1),
		%% atom_number(S,S1)
		viewIf([sb,S]),
		atom_number(S,TmpS),
		S1 is float(TmpS),
		viewIf([s1,S1])
	    ) ;
	    (	
		Y = Y1,
		M = M1,
		D = D1,
		H = H1,
		Mi = Mi1,
		(   number(S) -> (S1 is float(S)) ; 
		(
		 viewIf([sa,S]),
		 atom_number(S,TmpS),
		 S1 is float(TmpS),
		 viewIf([s1,S1])
		))
	    )),
	viewIf(wtf),
	T1 = [Y1-M1-D1,H1:Mi1:S1],
	viewIf(wtf),
	viewIf([time,T1]),
	true.
