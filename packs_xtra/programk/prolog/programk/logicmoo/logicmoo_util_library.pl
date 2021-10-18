% ===================================================================
% File 'logicmoo_util_library.pl'
% Purpose: To load the logicmoo libraries as needed
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_library.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-module(logicmoo_util_library,[dynamic_transparent/1,
         upcase_atom_safe/2,
         concat_atom_safe/3,
         string_to_atom_safe/2,
         atom_concat_safe/3,
         exists_file_safe/1,
         exists_directory_safe/1,
         time_file_safe/2,
         throw_safe/1,
         maplist_safe/2,
         maplist_safe/3,
         list_to_set_safe/2,
   multi_transparent/1]).

dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A).
dynamic_transparent(F/A):-!,multi_transparent(user:F/A).
dynamic_transparent(X):-functor(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(user:F/A).
multi_transparent(X):-functor(X,F,A),multi_transparent(F/A),!.

:- module_transparent(library_directory/1).

throw_safe(Exc):-trace,throw(Exc).
string_to_atom_safe(ISO,LISTO):-LISTO==[],!,string_to_atom(ISO,'').
string_to_atom_safe(ISO,LISTO):-string_to_atom(ISO,LISTO).
atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.
exists_file_safe(File):-prolog_must(atomic(File)),exists_file(File).
exists_directory_safe(File):-prolog_must(atomic(File)),exists_directory(File).
concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,concat_atom(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,concat_atom(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- concat_atom(List,Sep,Atom),!.
upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B),!.
time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).
list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),debugOnFailure(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
%% though this should been fine %%  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), debugOnFailure(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),debugOnFailure(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
%% though this should been fine %% maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).


:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).
:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).


hasLibrarySupport :- absolute_file_name(library('logicmoo/logicmoo_util_library.pl'),File),exists_file(File).

throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(library_directory), throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
:-not(hasLibrarySupport) -> addLibraryDir ; true .

:-hasLibrarySupport->true;throwNoLib.

% TODO remove this next line
% :-ensure_loaded(library('logicmoo/logicmoo_util_bugger.pl')).
% and replace with...


