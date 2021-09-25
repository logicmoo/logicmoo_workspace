% ==============================	
% Simple MT Prolog Server 
% ==============================	


%:- assert(user:prolog_file_type('P', prolog)). 	

:-use_module(library(logicmoo_utils)).
:-dynamic(serve_connection/0).

sigma_notrace(G):- once(G).

:- style_check(-singleton).
:- style_check(-discontiguous).

was_style_check(_):- set_prolog_flag(double_quotes,codes).
was_indexed(_).

:- was_style_check(-atom).
:- was_style_check(-string).


%:-set_prolog_flag(unknown,fail).
%:-set_prolog_flag(unknown,warn). 


:- ensure_loaded(library(readutil)).
:- ensure_loaded(library(socket)).
%:- include('sigma_header.pl').

:-dynamic(xmlCurrentOpenTags/2).
:-dynamic(isKeepAlive/1).
:-dynamic(isConsoleOverwritten/0).
:-dynamic(sigmaThreadCreate_data/5).
:-volatile(sigmaThreadCreate_data/5).


createPrologServer(Port) :-
	sigmaThreadCreate(nokill,'Sigma XML/SOAP Server Socket',xmlPrologServer(Port),_,[]).

win32:-
	setSigmaOption(client=html),
	xmlPrologServer(4051).

:- dynamic(sigma_tmp:sigma_server_socket/1).
:- volatile(sigma_tmp:sigma_server_socket/1).
tcp_close_socket_sigma:- ignore((sigma_tmp:sigma_server_socket(Socket), catch(tcp_close_socket(Socket),_,true))).

xmlPrologServer(Port):-
	tcp_socket(ServerSocket),
        catch(ignore(tcp_setopt(ServerSocket, reuseaddr)),_,true),
  assert(sigma_tmp:sigma_server_socket(ServerSocket)),
	at_halt(tcp_close_socket_sigma),
	please_tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 655),
	repeat,

		tcp_open_socket(ServerSocket, AcceptFd, _),
		cleanOldThreads,
		tcp_accept(AcceptFd, ClientSocket, ip(A4,A3,A2,A1)),
		getPrettyDateTime(DateTime),
		sformat(Name,'Dispatcher for ~w.~w.~w.~w  started ~w ',[A4,A3,A2,A1,DateTime]),
		sigmaThreadCreate(kill,Name,serviceAcceptedClientSocket(ClientSocket),_,[detatch(true)]),
	fail.

serviceAcceptedClientSocket(ClientSocket):-
	tcp_open_socket(ClientSocket, In, Out),
	catch(serviceIO(In,Out),E,writeSTDERR(serviceIO(In,Out)=E)),
	ignore((catch(flush_output(Out),_,true),catch(tcp_close_socket(ClientSocket),_,true))),
	thread_exit(complete),!.
			
mutex_call(Goal,Id):-
			mutex_create(Id),
			mutex_lock(Id),!,
			with_mutex(Id,Goal),!,
			mutex_unlock_all.
	



please_tcp_bind(ServerSocket, Port):-
	catch((tcp_bind(ServerSocket, Port),
	flush_output,
	%writeSTDERR('%~ cs.\nSigma server started on port ~w. \n\nYes\n?- ',[Port]),flush_output),
  writeSTDERR('%~ Sigma server started on port ~w.',[Port]),flush_output),
	error(E,_),
	(nop(writeSTDERR('\nWaiting for OS to release port ~w. \n(sleeping 4 secs becasue "~w")\n',[Port,E])),
	sleep(4),
	please_tcp_bind(ServerSocket, Port))),!.
	

saveUserInput:-retractall(isConsoleOverwritten),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten,!.
writeOverwritten:-assert(isConsoleOverwritten).

cleanOldThreads:-sigma_notrace(cleanOldThreadsTracable).

cleanOldThreadsTracable:-
	saveUserInput,
	current_thread(Id,Status),
	handleThreadStatus(Id,Status),fail.
cleanOldThreadsTracable:-writeSavedPrompt,!.
cleanOldThreadsTracable:-!.

handleThreadStatus(Id,running):-!. %Normal
handleThreadStatus(Id,exited(complete)):-!,thread_join(Id,_),!.
handleThreadStatus(Id,true):-!, writeSTDERR('% Thread ~w complete.\n',[Id]),!,thread_join(Id,_),writeOverwritten,!.
handleThreadStatus(Id,exception(Error)):-!, writeSTDERR('% Thread ~w exited with exceptions: ~q \n',[Id,Error]),!,thread_join(Id,_),!,writeOverwritten.
handleThreadStatus(Id,O):-!, writeSTDERR('% Thread ~w exited "~q". \n',[Id,O]),!,thread_join(Id,_),!,writeOverwritten.
	
:-dynamic(socket_in/2).
socket_in(main, user_input).
:-dynamic(socket_out/2).
socket_out(main, user_error).

serviceIO(In,Out):-
	retractall(socket_in(Session,_)),asserta(socket_in(Session,In)),
	retractall(socket_out(Session,_)),asserta(socket_out(Session,Out)),
	writeFmtServer(Out,'<?xml version="1.0" encoding="ISO-8859-1"?>\n',[]),
	peek_char(In,Char),
	serviceIOBasedOnChar(Char,In,Out),!.
	
	
serviceIOBasedOnChar('<',In,Out):-!,writeSTDERR('XML Request'),logOnFailure(service_soapd_request(In,Out)).
serviceIOBasedOnChar('G',In,Out):-!,writeSTDERR('HTTPD Request'),logOnFailure(service_httpd_request(In,Out)).
serviceIOBasedOnChar(_,In,Out):-       trace,
	thread_self(Session),
	retractall(isKeepAlive(Session)),
	xmlClearTags,
	repeat,
		catch(
			read_term(In,PrologGoal,[variable_names(ToplevelVars),character_escapes(true),syntax_errors(error)]),
			E,
			writeErrMsg(Out,E)),
		%writeSTDERR(PrologGoal:ToplevelVars),
		invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns),
		notKeepAlive(Out,Session),!.
				
notKeepAlive(Out,Session):-isKeepAlive(Session),
	write(Out,
		'complete.\n'  
		%'<prolog:keepalive/>\n'
				),catch(flush_output(Out),_,true),!,fail. 
notKeepAlive(Out,Session):-catch(flush_output(Out),_,true). 



xmlOpenTag(Name):-thread_self(Self),asserta(xmlCurrentOpenTags(Self,A)),writeFmtServer('<~w>',[Name]),!.
xmlOpenTagW(Out,Name,Text):-thread_self(Self),asserta(xmlCurrentOpenTags(Self,A)),writeFmtServer(Out,'~w',[Text]),!.

xmlCloseTag(Name):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer('</~w>',[Name]),!.
xmlCloseTagW(Name,Text):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer('~w',[Text]),!.
xmlCloseTagW(Out,Name,Text):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer(Out,'~w',[Text]),!.

xmlClearTags:-thread_self(Self),retractall(xmlCurrentOpenTags(Self,A)).

xmlExitTags:-thread_self(Self),retract(xmlCurrentOpenTags(Self,A)),writeFmtServer('</~w>',[Name]),fail.
xmlExitTags.
       
writeSTDERR(F):-writeSTDERR('~q',[F]).
writeSTDERR(F,A):-sigma_notrace((
	format(user_error,F,A),
	nl(user_error),
	flush_output(user_error))).
	
keep_alive:-thread_self(Me),retractall(isKeepAlive(Me)),assert(isKeepAlive(Me)),writeFmtFlushed('<keepalive/>\n',[]).
goodbye:-thread_self(Me),retractall(isKeepAlive(Me)),writeFmtServer('<bye/>/n',[]).

createThreadedGoal(Goal):-sigmaThreadCreate((thread_at_exit((thread_self(Id),thread_exit(i_am_done(Id)))),Goal),Id,[]).

invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns):-var(PrologGoal),!.
	
invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns):-
	term_to_atom(Session,Atom),concat_atom(['$answers_for_session',Atom],AnswersFlag),
	writeFmtServer(Out,'<prolog:solutions goal="~q">\n',[PrologGoal]),
	flag(AnswersFlag,_,0),
	set_output(Out),set_input(In),!,
	getCputime(Start),
	callNondeterministicPrologCommand(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars),
	xmlExitTags,
	getCputime(End),
	flag(AnswersFlag,Returns,Returns),
%	(Returns > 0 -> 
%		writeFmtServer(Out,'<prolog:yes/>\n',[]) ; 
%		writeFmtServer(Out,'<prolog:no/>\n',[])),!,
	Elapsed is End -Start,
	writeFmtServer(Out,'</prolog:solutions answers="~w" cputime="~g">\n',[Returns,Elapsed]),!.

callNondeterministicPrologCommand(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-
	ground(PrologGoal),!,
	catch(
		(PrologGoal,
		 flag(AnswersFlag,Answers,Answers+1),
		 writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars)
		 ),
	   Err,writeErrMsg(Out,Err,PrologGoal)),!.
	

writeErrMsg(Out,E):-message_to_string(E,S),writeFmtFlushed(Out,'<prolog:error>~s</prolog:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):-message_to_string(E,S),writeFmtFlushed(Out,'<prolog:error>goal "~q" ~s</prolog:error>\n',[Goal,S]),!.


callNondeterministicPrologCommand(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-
	catch(
		(PrologGoal,
		 flag(AnswersFlag,Answers,Answers+1),
		 writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars),
		 fail),
	   Err,writeErrMsg(Out,Err,PrologGoal)),!.
callNondeterministicPrologCommand(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-!.
	
	
writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars):-
	 flag(AnswersFlag,Answers,Answers),
	writeFmtServer(Out,'<prolog:result solution="~w">\n',[Answers]),
	writePrologToplevelVarsXML2(Out,ToplevelVars),
	writeFmtServer(Out,'</prolog:result>\n',[]),!.
			 
writePrologToplevelVarsXML2(Out,[]):-!.
writePrologToplevelVarsXML2(Out,[Term|REST]):-!,Term=..[_,N,V],
         writeFmtFlushed(Out,'       <prolog:p>~w = ~q</prolog:p>\n',[N,V]),
         writePrologToplevelVarsXML2(Out,REST),!.

writeFmtServer(A,B,C):-!.
writeFmtServer(A,B):-!.

writeFmtServer(A,B,C):-
	writeFmtFlushed(A,B,C).
writeFmtServer(A,B):-
	writeFmtFlushed(A,B).
	
writeFileToStream(Dest,Filename):-
	catch((
	open(Filename,'r',Input),
	repeat,
		get_code(Input,Char),
		put(Dest,Char),
	at_end_of_stream(Input),
	close(Input)),E,
	writeFmtFlushed('<prolog:error goal="~q">~w</prolog:error>\n',[writeFileToStream(Dest,Filename),E])).
     

sigmaThreadCreate(Perms,Name,Goal,Id,Options):-
	thread_create((thread_at_exit(sigmaThreadSelfClean),Goal),Id,Options),
	asserta(sigmaThreadCreate_data(Perms,Name,Goal,Id,Options)).

sigmaThreadCreate(Name,Goal,Id,Options):-
	thread_create((thread_at_exit(sigmaThreadSelfClean),Goal),Id,Options),
	asserta(sigmaThreadCreate_data(kill,Name,Goal,Id,Options)).

sigmaThreadCreate(Goal,Id,Options):-
	thread_create((thread_at_exit(sigmaThreadSelfClean),Goal),Id,Options),
	asserta(sigmaThreadCreate_data(kill,thread(Id),Goal,Id,Options)).

sigmaThreadCreate(Goal):-
	sigmaThreadCreate(Goal,Id,[detach(true)]).

isSigmaThread(ID,Goal):-
	sigmaThreadCreate_data(_,_,Goal,ID,_).
      
debugThread(T):-thread_signal(T, (attach_console, trace)).
  

sigmaThreadSelfClean:-ignore((thread_self(Id),retractall(sigmaThreadCreate_data(Perms,Name,Goal,Id,Options)))).

showSigmaThreads:-
	current_thread(Id,Status),
	sigmaThreadCreate_data(Perms,Name,Goal,Id,Options),
	writeSigmaThreadsHTML(Perms,Name,Goal,Id,Options,Status),
	fail.
showSigmaThreads.


writeSigmaThreadsHTML(nokill,Name,Goal,Id,Options,Status):-
	writeFmt('<tr><td>~w</td><td><nobr>~w</td><td>~w</td><td>&nbsp;</a></td><td>~w</td><td>~w</td><tr>\n ',[Id,Name,Status,Options,Goal]),!.

writeSigmaThreadsHTML(Perms,Name,Goal,Id,Options,Status):-
	writeFmt('<tr><td>~w</td><td><nobr>~w</td><td>~w</td><td><A href="controlpanel.jsp?kill=~w">Kill</a></td><td>~w</td><td>~w</td><tr>\n ',[Id,Name,Status,Id,Options,Goal]),!.

	
throwSigma(Module,Type,Details):-
	current_prolog_flag(debug_on_error, DebugOnError),
	set_prolog_flag(debug_on_error, false),!,
	throw(sigmaException(Module,Type,Details,DebugOnError)),
	ifInteractive(writeDebug('Post throwSigma')),!.

cs:-  setSigmaOption(client=html),
	createPrologServer(4051),
	cleanOldThreads.

:- ensure_loaded('sigma_swiprolog.pl').

