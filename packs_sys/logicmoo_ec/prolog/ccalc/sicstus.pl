:- multifile user:portray_message/2.
:- dynamic   user:portray_message/2.   
user:portray_message(warning, not_loaded(_,_)).

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library(charsio)).

vernum(N) :-
   ( predicate_property(current_prolog_flag(_,_),built_in)
     -> current_prolog_flag(version,Ver)
   ; predicate_property(prolog_flag(_,_),built_in)
     -> prolog_flag(version,Ver) ),
   name(Ver,VerName),
   append("SICStus ",N,VerName).

:- vernum(VerNum),
   ( append("3.7",_,VerNum)
     -> use_module(library(db))
   ; use_module(library(bdb)) ).

common_statistics(X) :- statistics(runtime,[_,X]).

% select(?Element, ?List, ?List2) 
% The result of removing an occurrence of Element in List is List2. 
common_select(V,X,Y) :- select(V,X,Y).

common_file_exists(File) :- file_exists(File).

common_file_delete(File) :-
	( file_exists(File) ->
	     delete_file(File)
	; true ).

common_tmpname(TmpName) :- tmpnam(TmpName).

common_number_chars(X,Y) :- number_chars(X,Y).


set_dirs :-
   prolog_load_context(directory,D),
   name(D,D2),
   last(D2,C),
   ( C == 47   % check if path ends in slash
     -> CCDir = D2
   ; format_to_atom(CCDir,"~s/",[D2]) ),
   iset(ccalc_dir,CCDir).


expand_filename(Filename,Expanded) :-
   absolute_file_name(Filename,Expanded).

seeing_filename(Filename) :-
   seeing(Filename).

format_to_atom(Atom,FormatString,Args) :-
        format_to_chars(FormatString,Args,Chars), name(Atom,Chars).

format_to_charlist(Chars,Format,Args) :- format_to_chars(Format,Args,Chars).

list([]).
list([_|_]).


%^jo- if the db is open, then close 
%^jo- do we still have rule_db?
%^jo- external db is created only if the number of grounded rules is large
%^jo- currently works with only sicstus.


db_init_external :-
   ( vernum(VerNum),
     ( append("3.7",_,VerNum)
       -> current_db(rule_db,_,_,DBref)
     ; db_current(rule_db,_,_,none,DBref) )
     -> db_close(DBref),
        system('rm rule_db/0 rule_db/if rule_db/spec; rmdir rule_db')
%  ; db_open(rule_db,update,_,DBref)
%    -> db_close(DBref), 
%       system('rm rule_db/0 rule_db/if rule_db/spec; rmdir rule_db')
   % the statements above caused problems when ccalc is loaded from a
   % directory other than the current working directory.  I think the
   % statements below have the same intended effect...
   ; file_exists('rule_db')
      -> system('rm -r rule_db')
   ; true ).


db_open_external(I) :-
        value(max_no_of_clauses_in_internal_db,MaxNoOfClauses),
        ( I = MaxNoOfClauses
            -> format("[Ext DB] ",[]), flush_output,
               db_open(rule_db,update,DBref),
               set_default_db(DBref), db_buffering(_,on)
        ; true ).

db_store_rule(I,Term) :-
        value(max_no_of_clauses_in_internal_db,MaxNoOfClauses),
        I < MaxNoOfClauses -> assertz(Term) ; db_store(Term,_).
        
db_fetch_rule(Term) :-
        value(max_no_of_clauses_in_internal_db,MaxNoOfClauses),
        value(rule_count,I),
        ( I < MaxNoOfClauses
            -> call(Term)
        ; ( call(Term)
          ; db_findall(Term,TermList), member(Term,TermList) ) ).


/*
db_init_external.
        
db_open_external(_).
        
db_store_rule(_I,Term) :- assertz(Term).
        
db_fetch_rule(Term) :- call(Term).
*/


db_init_query_external.

db_open_query_external(_).
        
db_store_query_rule(_I,Term) :- assertz(Term).
        
db_fetch_query_rule(Term) :- call(Term).


read_line(end_of_file) :-
        at_end_of_stream,
        !.
read_line(Cs) :- read_line_loop(Cs).  

read_line_loop([]) :-
        at_end_of_line, !, skip_line.
read_line_loop([C|Cs]) :-
        get0(C), read_line_loop(Cs).



common_ground(Term) :- var(Term), !, fail.
common_ground(Term) :- functor(Term,_F,N), common_ground(Term,0,N).

common_ground(_A,N,N) :-
        !.
common_ground(A,I,N) :-
        I1 is I+1, arg(I1,A,Ai),
        ( var(Ai) -> fail
        ; functor(Ai,_F,M), common_ground(Ai,0,M) ),
        common_ground(A,I1,N).

common_last([M|Ns],N) :- last([M|Ns],N).
        

determine_os(OS) :-
% unifies OS with a string representing the operating system in use, as
% returned by the system call "uname"
   popen(uname,read,Stream),
   current_input(CurInput),
   set_input(Stream),
   read_line(OS),
   close(Stream),
   set_input(CurInput).


user_help :- cchelp.
help(Item) :- cchelp(Item).


% leave the following lines at the end of the file -- it causes problems
% otherwise

:- op( 700, xfx, [ \= ] ).
X \= Y :- \+ X = Y.
