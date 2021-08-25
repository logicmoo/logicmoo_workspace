% ===========================================================
% NATIVE HTTPD SERVER FOR SWI-PROLOG (Douglas Miles)
% ===========================================================
:-include('sigma_header.pl').


service_httpd_request(In,Out):-
      logOnFailure(read_http(In,OptionsIn)),
      logOnFailure(fixOptionsFromWeb(OptionsIn,OptionsIn0)),
      logOnFailure((fixOptionsFromWeb(OptionsIn0,OptionsIn1))),
      logOnFailure(fixOptionsFromForeign(OptionsIn1,Options)),
	writeFmt('REQUESTED: "~q" \n',[Options]),
	writeFmt(Out,'HTTP/1.1 200 OK\nServer: Sigma-HTTPD\nContent-Type: text/html\n\n',[]),
	logOnFailure(give_start_page(In,Out,Options)),
	catch(flush_output(Out),_,true).

give_start_page(In,Out,Options):-
	setSigmaOption(Options),
	(parse_sigma_ua(Options)-> true; sigma_ua(Options)).


get_session(opt_kb,UKB):-getDefaultKB(UKB).
get_session(opt_ctx_assert,CTX):-get_default_assertion_context(CTX).


write_kb_combobox(Format):-
		getSigmaOption(opt_kb='Merge',UKB),
		%setof(KB,sigmaCache(PredR,_,_,_,_,KB,_),KBs),
		toMarkUp(html,select(opt_kb,[UKB|KBs]),Vars,Out),writeFmt(Format,[Out]).
		
write_ctx_combobox(Format):-
		getSigmaOption(opt_ctx_assert='ToplevelContext',UCTX),
		%setof(CTX,sigmaCache(PredR,_,_,_,_,CTX,_),CTXs),
		toMarkUp(html,select(opt_ctx_assert,[UCTX|CTXs]),Vars,Out),writeFmt(Format,[Out]).
	
	
call_prolog_http(In,Out,CMD,Vars):-
	writeFmt(Out,'<html><head><title>call_prolog_http(~w)</title></head>',[CMD]),
	writeFmt(Out,'<html><pre>',[]),
	invoke_cmd(Out,CMD,Vars,_),
	writeFmt(Out,'</pre></html>',[]).


read_http(In,Request):-	 
	read_line_with_nl(In, Codes, []),
	append("GET /",Stuff,Codes), %trace,
	append(RequestCodes,[72,84,84,80|_],Stuff),
	atom_codes(RequestEncoded,RequestCodes),
	build_request(RequestEncoded,Request).	

build_request(RequestEncoded,[file=Request]):- concat_atom([X],'?',RequestEncoded),www_form_encode(Request,X),!.
build_request(RequestEncoded,[file=Request|ENCARGS]):- concat_atom([X,ARGS],'?',RequestEncoded),www_form_encode(Request,X),build_request_args(ARGS,ENCARGS).
build_request_args(ARGS,ENCARGS):- concat_atom(ArgList,'&',ARGS),decode_request_args(ArgList,ENCARGS).

decode_request_args([],[]):-!.
decode_request_args([ctx=Value|List],[ctx=CValue,kb=KValue|ARGS]):-  
	  concat_atom([KValue,CValue],':',Value),!,
	  decode_request_args(List,ARGS).
	  
decode_request_args([Arg|List],[DName=DValue|ARGS]):-  
	  split_nv(Arg,Name,Value),
	  www_form_encode(AName,Name),      	
	  www_form_encode(AValue,Value),!,
	  safe_atom_to_term(AName,DName),
	  safe_atom_to_term(AValue,DValue),
	  decode_request_args(List,ARGS).
	  
%ctx=Merge%3ASTRUCTURAL-ONTOLOGY&

split_nv(Arg,Name,Value):-concat_atom([Name,Value],'=',Arg),!.
split_nv(Arg,Arg,Arg).
	       
safe_atom_to_term(A,A):-var(A),!.
safe_atom_to_term(tn,tn):-!.
safe_atom_to_term(N,N):-number(N),!.
safe_atom_to_term(A=B,AA=BB):-
		safe_atom_to_term(A,AA),
		safe_atom_to_term(B,BB),!.
safe_atom_to_term(A,T):-catch(atom_to_term(A,T,_),_,fail),number(T),!.
safe_atom_to_term(A,T):-catch(atom_to_term(A,T,_),_,fail),not(var(T)),not(compound(T)),!.
safe_atom_to_term(A,T):-atom(A),catch(atom_codes(A,[95|_]),_,fail),catch(atom_to_term(A,T,_),_,fail),!.
safe_atom_to_term(A,A):-!.


read_line_with_nl(Fd, Codes, Tail) :-
	get_code(Fd, C0),
	read_line_with_nl(C0, Fd, Codes, Tail).
read_line_with_nl(end_of_file, _, Tail, Tail) :- !.
read_line_with_nl(-1, _, Tail, Tail) :- !.
read_line_with_nl(10, _, [10|Tail], Tail) :- !.
read_line_with_nl(C, Fd, [C|T], Tail) :-
	get_code(Fd, C2),
	read_line_with_nl(C2, Fd, T, Tail).

