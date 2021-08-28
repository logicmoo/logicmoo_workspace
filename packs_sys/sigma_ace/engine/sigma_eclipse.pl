

%:-use_module(library(quintus)).
%:- use_module(library(fd)). 
 %use_module(library(cprolog)),consult('sigma_eclipse.pl').
%:-use_module(library(cprolog)).
:-use_module(library(iso)).



%multifile(_).

%set_prolog_flag(_,_).


%:-op(600,xfy,':').
 

arithmetic_function(_).

flag(Name,Out,In):-
	flag_db(Name,Out),
	set_the_flag(Name,In).

:-dynamic(in_flag_db/2).

index(_).	

	
	

flag_db(Name,Out):-in_flag_db(Name,Out),!.
flag_db(Name,0).

set_the_flag(Name,In):-
	retractAllProlog(in_flag_db(Name,_)),
	set_the_flag_1(Name,In).
	
set_the_flag_1(Name,V):-var(V).

set_the_flag_1(Name,X + Y):-Z is X + Y,!,
	     set_the_flag_1(Name,Z).
set_the_flag_1(Name,X - Y):-Z is X - Y,!,
	     set_the_flag_1(Name,Z).
set_the_flag_1(Name,V):-asserta(in_flag_db(Name,V)).

style_check(_).


ignore(X):-(call(X);true).

absolute_file_name(X,X).

include_header:-!.

:-['sigma_header.pl'].


writeModeSet(Mode):-
	(unsetSigmaOption(client=_)),
	(setSigmaOption(client=Mode)),!.


:-absolute_file_name(.,Local),assert('LOGIC_ENGINE_RT'(Local)),assert(library_directory(Local)).
:-absolute_file_name('../..',X),assert('ROOT_RT'(X)).

:-dynamic(ado_cnnstr/1).

:- ( not(ado_cnnstr(_)) -> ('ROOT_RT'(X),atom_concat(X,'/tmp/sigma_prolog.mdb',O),assert(ado_cnnstr(O))) ; true ).


:- dynamic sigmaCache/1.
:- dynamic sigmaCache/6.
	
:-['sigma_temporal.pl'].
:-['sigma_reader.pl'].
:-['sigma_notes.pl'].
:-['sigma_soap.pl'].
:-['sigma_transitivity.pl'].
:-['sigma_query_buffer.pl'].
:-['sigma_inference.pl'].
:-['sigma_enable.pl'].
:-['sigma_canonicalize.pl'].
:-['sigma_api_test.pl'].
:-['sigma_utility.pl'].
:-['sigma_builtin.pl'].
:-['sigma_suo.pl'].
:-['sigma_ace.pl'].
:-['sigma_kif_check.pl'].
:-['sigma_file_functions.pl'].
:-['sigma_wfs_safety.pl'].
:-['sigma_theorems.pl'].
:-['sigma_hashing.pl'].
:-['sigma_assert.pl'].
:-['sigma_retract.pl'].
:-['sigma_arg_domains.pl'].
:-['sigma_surf_to_can.pl'].
:-['sigma_surface_inference.pl'].
:-['sigma_reduce.pl'].
:-['sigma_functions.pl'].
%:-['sigma_can_to_wfs.pl'].
:-['sigma_operation.pl'].
:-['sigma_modules.pl'].
:-['sigma_krlog.pl'].
:-['sigma_database.pl'].
:-['agentConsultation.pl'].
:-['sigma_useragent.pl'].
:-['sigma_useragent.pl'].
:-['sigma_response.pl'].
:-['sigma_query.pl'].
:-['sigma_truth_maintains.pl'].
:-['sigma_tester.pl'].
:-['sigma_translit.pl'].
:-['sigma_term_ml.pl'].
:-['sigma_httpd.pl'].
:-['sigma_image.pl'].
:-['sigma_equal.pl'].
:-['sigma_query.pl'].


get_time(6667).
convert_time(T,'Now is the time').
convert_time(T,1,2,3,4,5,6,7).

sigma_numbervars(X,_,Y,Z):-sigma_numbervars(X,Y,Z).


:-setSigmaOptionDefaults.
%:-set_prolog_flag(unknown,fail).

 
string([_|_]).

writeFmt(Format,Args):-!,current_output(Stream),writeFmt(Stream,Format,Args).

 


writeFmt(Loc,[],Args):-!.
writeFmt(user_error,F,Args):-!,
	writeFmt(user_output,F,Args).
writeFmt(Loc,Atom,Args):-atom(Atom),atom_codes(Atom,String),!,
	writeFmt(Loc,String,Args).
writeFmt(Loc,[126,110|Rest],Args):-!,nl(Loc),
	writeFmt(Loc,Rest,Args),!.
writeFmt(Loc,[126,N|Rest],[A|Rgs]):-!,
	wformat(Loc,N,A),!,
	writeFmt(Loc,Rest,Rgs),!.
writeFmt(Loc,[N|Rest],Rgs):-!,
	put_code(Loc,N),
	writeFmt(Loc,Rest,Rgs),!.
writeFmt(Loc,Rest,Rgs):-!,throw('fomat_error'(writeFmt(Loc,Rest,Rgs))).	


fmtString(Format,Args):-!,current_output(Stream),fmtString(Stream,Format,Args).

fmtString([],[],Args):-!.
fmtString(Loc,Atom,Args):-atom(Atom),atom_codes(Atom,String),!,
	fmtString(Loc,String,Args).
fmtString([10|Loc],[126,110|Rest],Args):-!,nl(Loc),
	fmtString(Loc,Rest,Args),!.
fmtString(OLOc,[126,N|Rest],[A|Rgs]):-!,
	swformat(LocN,N,A),!,
	fmtString(Loc,Rest,Rgs),!,
	append(LocN,Loc,OLOc).
fmtString([N|Loc],[N|Rest],Rgs):-!,
	fmtString(Loc,Rest,Rgs),!.
fmtString(Loc,Rest,Rgs):-!,throw('fomat_error'(fmtString(Loc,Rest,Rgs))).	

wformat(Loc,_,C):-string(C),!,atom_codes(A,C),write(Loc,A).
wformat(Loc,119,A):-!,write(Loc,A). % ~w
wformat(Loc,113,A):-!,writeq(Loc,A). % ~q
wformat(Loc,103,A):-!,write(Loc,A). % ~g
wformat(Loc,N,A):-!,put_code(Loc,N),put_code(Loc,A),!.

swformat(A,_,A):-string(A),!.
swformat(C,_,A):-atom(A),!,atom_codes(A,C),!.
swformat(String,119,A):-!,term2string(A,String). % ~w
swformat(String,113,A):-!,term2string(A,String). % ~q
swformat(String,103,A):-!,term2string(A,String). % ~g
swformat([N,A],N,A):-!.




%:-ensure_loaded(sigma_pttp_original).
%:-ensure_loaded(sigma_fol).
%:-ensure_loaded(sigma_normal).


/*
recompile_all :- !,make,
         ['sigma_dynamics.pl'],
         ['sigma_language.pl'].
*/
recompile_all :- make.


%         get_storage_file("sigma_dynamics.kif",Name),
%         load_kif_as_prolog(Name),
 %        get_storage_file("sigma_language.kif",Name1),
 %        load_kif_as_prolog(Name1).

:-recompile_all.

%:-ensure_loaded(sigma_console).

getCleanCharsWhitespaceProper([X],[]):-is_whitespace_code(X).
getCleanCharsWhitespaceProper([F|X],W):-is_whitespace_code(F),getCleanCharsWhitespaceProper(X,W).
getCleanCharsWhitespaceProper(X,X):-!.

list_to_set(List1,List2):-sort(List1,List2).

string_clean(X,X).



%['sigma_bprolog.pl'],[tests_for_doug],load_ontologies.

ttta:-term_to_atom('T'(A),X),nl,writeq(X),nl.

term_to_atom(T,A):-break,
	open(term_to_atom,write,W,[type(binary)]),
	writeq(W,T),
	close(W,[force(true)]),!,
	open(term_to_atom,read,R),
	get_all_codes(R,Codes),
	atom_codes(A,Codes),
	close(R,[force(true)]).!.
	
get_all_codes(R,[]):-at_end_of_stream(R),!.
get_all_codes(R,[C|Odes]):-
	get_code(R,C),
	get_all_codes(R,Odes),!.
	
	
%atom_to_term(Atom,Term,Vars):-parse_atom(Atom,Term,Vars).

is_whitespace_code(32).
is_whitespace_code(X):-X<32.




unnumbervars(X,Y):-
	getPrologVars(X,List,_,_),
	recopy_each_var(X,List,Y).

recopy_each_var(X,[],X).
recopy_each_var(X,[V|List],Y):-
	ok_subst(X,V,NewVar,O),
	recopy_each_var(O,List,Y),!.
	
	


