

:-dynamic(asserted/4).
:-dynamic(assertion/13).

cyc_system_intern_magic.

current_file(FileBase,Dir):-current_stream(File,read,_Stream),atom(File),is_absolute_file_name(File),
   file_directory_name(File,Dir),file_base_name(File,FilePart),once(FileBase=FilePart;file_name_extension(FileBase,_Ext,FilePart)).

asserta_if_new(A):-retract(A),fail.
asserta_if_new(A):-asserta(A),!.

:-source_file(cyc_system_intern_magic,F),current_file(cyc_system_intern,Dir),writeq(current_file(F,Dir)),nl,!,
   asserta_if_new(user:file_search_path(cyc_api, Dir)),asserta_if_new(library_directory(Dir)).

:-use_module(library(cyc)). 


ensure_qlf_loaded(Name):-!,ensure_loaded(Name).
ensure_qlf_loaded(Name):- file_name_extension(FileStem,_,Name),file_name_extension(FileStem,'qlf',QLF),  
   catch((load_files(FileStem,[qcompile(auto)]),(ensure_loaded(Name))),_,(qcompile(FileStem),ensure_loaded(QLF))),!.


load_lljs([]):-!.
load_lljs([X|Xs]):-load_llj(X),!,load_lljs(Xs).
load_llj(X):-[X].

:-dynamic(nd_cyc_meta_predicate/3).
:-multifile(nd_cyc_meta_predicate/3).
cyc_meta_predicate(X):-var(X),!,nd_cyc_meta_predicate(F,A,_LA),X=F/A.
cyc_meta_predicate([X|L]):-cyc_meta_predicate(X),cyc_meta_predicate(L),!.
cyc_meta_predicate(X):-functor(X,F,A),F \= '/', !, cyc_meta_predicate(F/A).
cyc_meta_predicate(X):-X = F/A, nd_cyc_meta_predicate(F,A,A),!.
cyc_meta_predicate(X):-X = F/A, asserta(nd_cyc_meta_predicate(F,A,A)),!,dynamic(X),multifile(X),style_check(-singleton).

:-dynamic(nd_cyc_predicate/3).
:-multifile(nd_cyc_predicate/3).
cyc_predicate(X):-var(X),!,nd_cyc_predicate(X).
cyc_predicate([X|L]):-cyc_predicate(X),cyc_predicate(L),!.
cyc_predicate(X):-functor(X,F,A),F \= '/', !, cyc_predicate(F/A).
cyc_predicate(X):-X = F/A, nd_cyc_predicate(F,A,A),!.
cyc_predicate(X):-X = F/A, asserta(nd_cyc_predicate(F,A,A)),dynamic(X),multifile(X),style_check(-singleton).


:-use_module('cyc.pl').

some_data_file(File,Filename):-
   absolute_file_name(library(File),[extensions(['.pl','.qlf',''])],Filename),exists_file(Filename),!.
some_data_file(File,Filename):-
   absolute_file_name(cyc_api(File),[extensions(['.pl','.qlf',''])],Filename),exists_file(Filename),!.
some_data_file(Filename,Filename).

cycdataFile(File):-some_data_file(File,Filename),exists_file(Filename),debugFmt(ensure_qlf_loaded(Filename)),ensure_qlf_loaded(Filename).

cycdataFile(File,File):-cycdataFile(File).

init_cycdataFile(F):-initialization(cycdataFile(F)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

loadSplits:- expand_file_name('splits/*.llj',Xs),load_lljs(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


:- ensure_loaded('splits/allSplits').

%%:- loadSplits.

:- ignore((savedStubs(_, F, A), dynamic(F/A) , multifile(F/A),fail)).
:- ignore((nd_cyc_meta_predicate(F, A,_), dynamic(F/A) , multifile(F/A),fail)).
:- ignore((nd_cyc_predicate(F, A,_), dynamic(F/A) , multifile(F/A),fail)).

:- qcompile('splits/allSplits').

:-dynamic(holds/5).
holdsMt(Reln,A1,A2,A3,A4,Mt):- ':TRUE-MON'(Reln,A1,A2,A3,A4,Mt,_Id).
holdsMt(Reln,A1,A2,A3,A4,Mt):- ':TRUE-DEF'(Reln,A1,A2,A3,A4,Mt,_Id).

holds(Reln,A1,A2,A3,A4):-holdsMt(Reln,A1,A2,A3,A4,_Mt).

:-dynamic(holds/4).
holdsMt(Reln,A1,A2,A3,Mt):- ':TRUE-MON'(Reln,A1,A2,A3,Mt,_Id).
holdsMt(Reln,A1,A2,A3,Mt):- ':TRUE-DEF'(Reln,A1,A2,A3,Mt,_Id).

holds(Reln,A1,A2,A3):-holdsMt(Reln,A1,A2,A3,_Mt).

:-dynamic(holds/3).
holdsMt(Reln,A1,A2,Mt):- ':TRUE-MON'(Reln,A1,A2,Mt,_Id).
holdsMt(Reln,A1,A2,Mt):- ':TRUE-DEF'(Reln,A1,A2,Mt,_Id).

holds(Reln,A1,A2):-holdsMt(Reln,A1,A2,_Mt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


/*
cycdataFile(('nocomment')),cycdataFile(('asserted')),cycdataFile(('withvars')),
:-init_cycdataFile(('real_big_kb')).
:-init_cycdataFile(('littlebetter')).
:-init_cycdataFile(('nonewlines')).
:-init_cycdataFile(('allp')).
:-init_cycdataFile(('hl_holds')).
:-init_cycdataFile(('el_holds')).
:-init_cycdataFile(('plassertions')).
:-init_cycdataFile(('plnarts')).
:-init_cycdataFile(('plconstants')).


*/

file_name_no_extension(File,Base):-file_name_extension(Base,_,File).
load_cyc_data_file(ZZZ):-debugFmt(ZZZ),maplist(cycdataFile,ZZZ,_).

loadRestOfStages:-expand_file_name('./stage*/*.pl',X),expand_file_name('./stage*/*.qlf',Y),append(X,Y,Z),maplist(file_name_no_extension,Z,ZZ),sort(ZZ,ZZZ),load_cyc_data_file(ZZZ).


%:-ensureCycCallsProlog.

