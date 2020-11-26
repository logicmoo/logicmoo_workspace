% --------------------- op_post_cond_check.pl ---------------------
% operator post-condition consistency checker.
% activated by the command 'tool3'.
% uses operators and modified inconsistent_constraints.

% checks whether the operators post-conditions validate any inconsistent constraints.
% -----------------------------------------------------------------
:- dynamic check1_error/0.

start_post_check :- 
      nl,write('Checking consistency of post-conditions..'),
      incon_list(InconList),
      tool3(InconList), 
      check_complete.

tool3(InconList) :-
        operator(Op_Name,Prev,Necc,Cond),
	get_post_cond(Prev,Necc,Cond,List),
        consistency_check(Op_Name,List,InconList),
        fail.
tool3(InconList):-
        nl,write('All other operators post-conditions appear to be OK'),
        nl,told.

% puts all the postconditions(Prev_cond +Necc_rhs +Cond_rhs) into a list.
%******************************************************************

get_post_cond(Prev,Necc,Cond,List):-
	get_prev_cond(Prev, Prev_cond),
	get_rhs(Necc,Necc_rhs),
	get_rhs(Cond,Cond_rhs),
	append(Prev_cond,Necc_rhs,List1),
	append(List1,Cond_rhs,List),!.
	
%*****************************************************************

% checks if any subsets and permutations of the list of postconditions 
% validates the inconsistent_constraints.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

consistency_check(Op_Name,List,InList):-
        check_incon_OK(List,InList),!.
consistency_check(Op_Name,List,InList):-
	nl,write('OPERATOR '),
	write(Op_Name),write(' is not inconsistent'),nl.

%****************************************************************
% get the prevail conditions
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
get_prev_cond([], []):-!.
get_prev_cond(Prev, Prev_cond):-
        get_prev_cond1(Prev, [],Prev_cond),!.

get_prev_cond1([], Prev_cond, Prev_cond):-!.
get_prev_cond1([se(_,_,Prev)|TPrev],List,Prev_cond):-
        append(List, Prev, List1),
        get_prev_cond1(TPrev,List1,Prev_cond),!.

%****************************************************************
% get the post conditions
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
get_rhs([], []):-!.
get_rhs(List, Post_cond):-
        get_rhs1(List, [],Post_cond),!.

get_rhs1([], Post_cond, Post_cond):-!.
get_rhs1([sc(_,_,Lhs=>Rhs)|TList],Start,Post_cond):-
        append(Start, Rhs, Start1),
        get_rhs1(TList,Start1,Post_cond),!.


% check_incon_OK task list if it is inconsistent

check_incon_OK(Task,InList):-
	not(check_in(InList,Task)).

check_in([HInLs|Rest],Tasklist) :-
	check_in1(HInLs,Tasklist,[]).
check_in([HInLs|Rest],Tasklist) :-
	check_in(Rest,Tasklist).

check_in1(Inls,Tasklist,Delist) :-
  	search_for_ne(Inls,NOne, NE),
	check_in2(NOne, NE,Tasklist,Delist).

check_in2([HInLs|TLs],NE,Tasklist,Delist) :-
  	member(HInLs,Tasklist),
        binding(NE),
        subtract(Tasklist, [HInLs],Tasklist1),
        append(Delist,[HInLs],Delist1),
	check_in2(TLs,NE,Tasklist1,Delist1).
check_in2([],NE,Tasklist,Delist):-
        nl, write('CONDITION '),write(Delist),
        write(' generated from').

incon_list(List):-
  setof(A,A^inconsistent_constraint(A),List).

% search the list to find out the ne()s, 
% search_for_ne(List, NoNels, NEls)
search_for_ne([],[], []):-!.
search_for_ne([ne(A,B)|TLs],NOne, [ne(A,B)|NE1]):-
        search_for_ne(TLs,NOne,NE1),!.
search_for_ne([HInLs|TLs],[HInLs|NOne1], NE):-
        search_for_ne(TLs,NOne1, NE),!.

binding([]).
binding([ne(A,B)|Rest]):-
        A\==B,
        binding(Rest).
