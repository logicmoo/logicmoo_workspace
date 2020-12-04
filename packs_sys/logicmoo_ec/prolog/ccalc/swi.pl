:-  use_module(library(logicmoo/xlisting)).

atom_hash(T,H) :- hash_term(T,H).

format_to_atom(C,S,L) :-
   sformat(C2,S,L),
   string_to_atom(C2,C).

format_to_charlist(Chars,Format,Args) :-
	format_to_atom(String,Format,Args),
	string_to_list(String,Chars).
system(X,Y) :- shell(X,Y).
system(X) :- shell(X).

compile(X) :- consult(X).

common_statistics(X) :-
	( value(last_stat_time,T) ->
	     statistics(cputime,T2),
	     X is 1000 * (T2 - T),
	     iset(last_stat_time,T2)
	; statistics(cputime,T),
	  X is 1000 * T,
	  iset(last_stat_time,T) ).

% Apparently, SWI Prolog changed the behavior of "exists_file" so it only
% works for files, not directories.  So we use access_file/2, which works
% for both.
% -- Selim T. Erdogan, 9 May 2012
%common_file_exists(File) :- exists_file(File).
common_file_exists(File) :- access_file(File, exist).

common_file_delete(File) :-
	( exists_file(File) ->
	     delete_file(File)
	; true ).

common_tmpname(TmpName) :- tmp_file(temp, TmpName).

common_number_chars(X,Y) :-
   % SWI-Prolog throws an exception instead of just failing when Y isn't
   % a string of numbers, so we have to trap the exception and fail normally
   catch(number_chars(X,Y),_,fail).


% The argument order SWI Prolog used to have for last/2 was the 
% reverse of many other systems (such as SICStus), so we had to have a 
% wrapper.  Now the order has been changed so we fixed the wrapper.
% -- Selim T. Erdogan, 26 Jan 2008
%
%common_last([M|Ns],N) :- last(N,[M|Ns]).
common_last([M|Ns],N) :- last([M|Ns],N).

suffix(S,S).
suffix(S,[_|Rest]) :- suffix(S,Rest).

/* This version of set_dirs was commented out because it didn't work right.
 * Now we use the one below the comments, taken directly from sicstus.3.7.1.pl.
set_dirs :-
   % set current working directory to current Unix working directory
   % (instead of the directory ccalc was loaded from) 
%   environ('PWD',PWD),  
%   working_directory(_,PWD),
   
   % set the value of ccalc_dir to the location from which ccalc.pl was
   % loaded
   seeing(Input),
   absolute_file_name(Input,FullName),
   determine_path(FullName,CCDir),
   iset(ccalc_dir,CCDir).
*/
set_dirs :-
   prolog_load_context(directory,D),
   name(D,D2),
   last(D2,C),
   ( C == 47   % check if path ends in slash
     -> CCDir = D2
   ; format_to_atom(CCDir,"~s/",[D2]) ),
   iset(ccalc_dir,CCDir).


environ(Var,Value) :- getenv(Var,Value).
/*
working_directory(OldDir,NewDir) :-
   prolog_load_context(stream,File),
   determine_path(File,OldDirStr),
   format_to_atom(OldDir0,"~w",[OldDirStr]),
   absolute_file_name(OldDir0,OldDir),
   chdir(NewDir).
*/

expand_filename(Filename,Expanded) :-
   expand_file_name(Filename,[Temp|_]),
   absolute_file_name(Temp,Expanded).

seeing_filename(Filename) :-
   seeing(S),
   stream_property(S,file_name(Filename)).


format_to_chars(Pat,Subs,Chars) :-
   sformat(Str,Pat,Subs),
   string_to_list(Str,Chars).

list([_|_]).
          

sum_list([],0).
sum_list([A|Rest],S) :-
	sum_list(Rest,R),
	S = R + A.


term_hash(Term,_X,Y,Hash) :-
        sumhash(Term,HT),
        ( var(HT) -> true ; Hash is HT mod Y ).
        
sumhash(Term,Hash) :-
        ( atom(Term)
            -> atom_hash(Term,Hash)
        ; integer(Term)
            -> Hash = Term
        ; compound(Term)
            -> functor(Term,F,N),
               atom_hash(F,HF),
               sumhash_args(Term,0,N,HN),
               ( var(HN)
                   -> true
               ; Hash is HF + HN )
        ; var(Term)
            -> true
        ; Hash = 0 ).

sumhash_args(_Term,N,N,0) :-
        !.
sumhash_args(Term,M,N,HT) :-
        M1 is M+1,
        arg(M1,Term,A),
        sumhash(A,HA),
        ( var(HA)
            -> true
        ; sumhash_args(Term,M1,N,HN),
          ( var(HN)
              -> true
          ; HT is HA + HN ) ).


% The old versions of SWI Prolog used to have findall/3, but no 
% findall/4, while SICStus and other systems had the latter too.
% So we used to define it here ourselves.
% findall/4 was added to SWI in September 2007 so this is no longer
% necessary (and, in fact, re-defining a built-in predicate is 
% forbidden.) 
% -- Selim T. Erdogan, 26 Jan 2008
%
%findall(P,G,Ps,T) :-
%        findall(P,G,Xs), append(Xs,T,Ps).

% false :- fail.

ord_member(M,[N|Ns]) :-  
        ( M > N
            -> ord_member(M,Ns)
        ; M = N
            -> true 
        ; fail ).
        
ord_subset([M|Ms],[N|Ns]) :-
        ( M > N
            -> ord_subset([M|Ms],Ns)
        ; M = N
            -> ord_subset(Ms,Ns)
        ; fail ).
ord_subset([],_).
               

read_line(end_of_file) :-
	at_end_of_stream,
	!.
read_line(Cs) :- read_line_loop(Cs).
          
read_line_loop([]) :-
        peek_char(C),
        ( C = end_of_file
            -> !
        ; C = '\n'
            -> !, get_char('\n')
        ; fail ).
read_line_loop([C|Cs]) :-
        get0(C), read_line_loop(Cs).
          

skip_line :- read_line(_).

common_ground(Term) :- var(Term), !, fail.
common_ground(Term) :- functor(Term,_F,N), common_ground(Term,0,N).

common_ground(_A,N,N) :-
        !.
common_ground(A,I,N) :-
        I1 is I+1, arg(I1,A,Ai),
        ( var(Ai) -> fail
        ; functor(Ai,_F,M), common_ground(Ai,0,M) ),
        common_ground(A,I1,N).
        
db_init_external.
        
db_open_external(_).
        
db_store_rule(_I,Term) :- assertz(Term).
        
db_fetch_rule(Term) :- call(Term).

db_init_query_external.

db_open_query_external(_).
        
db_store_query_rule(_I,Term) :- assertz(Term).
        
db_fetch_query_rule(Term) :- call(Term).
              
common_select(V,X,Y) :-
        ( var(V) -> fail ; select(V,X,Y) ).


% remove_duplicates (List, Result)
%
% Removes duplicate entries in a list and sort it
%

remove_duplicates(Xs,Ys) :-
   list_to_set(Xs,Ys).

/*
remove_duplicates(Xs,Ys) :-
   remove_duplicates_aux(Xs,Zs), 
   sort(Zs,Ys). 


remove_duplicates_aux([], []).

remove_duplicates_aux([X|Rest], Result) :-
        member(X, Rest), !,
        remove_duplicates_aux(Rest, Result).
        
remove_duplicates_aux([X|Rest], [X|Result]) :-
        % X is not a member of Rest as
        % the above clause has a cut in it.
        remove_duplicates_aux(Rest, Result).
*/

nth(Index, List, Elem) :- nth1(Index,List,Elem).


% Apparently, SWI Prolog changed the behavior of "pipe" which we used to
% below, in determining the operating system.  So I made a new version
% of determine_os/1, using the built-in process_create/3.
%
% -- Selim T. Erdogan, 23 May 2012
%
% unifies OS with a string representing the operating system in use, as
% returned by the system call "uname"
%
determine_os(OS) :-
% Since the UTCS machines still have SWI Prolog 5.8.0, which doesn't
% seem to support process_create/3, we make sure that the version is
% higher than that before using process_create/3.
	current_prolog_flag(version,Ver),
	( Ver > 50800 
          -> (process_create(path(uname),[],[stdout(pipe(Stream))]),
              see(Stream))
          ;  see(pipe(uname))),
	read_line(OS),
	seen.

:- dynamic((atomicFormula/1, (<=)/2 )).

% hook CCalc's help routines into Prolog's "help" predicate
prolog:help_hook(help) :- cchelp.
prolog:help_hook(help(Item)) :- cchelp(Item).

