
end_of_file.

% ==============================	
% Simple MT Prolog Server 
% ==============================	

:-dynamic(socket_in/1).
:-dynamic(socket_out/1).
:-dynamic(serve_connection/0).

:-assert(prolog_file_type('P', prolog)). 	

:- style_check(-singleton).
:- style_check(-discontiguous).

was_style_check(_):- set_prolog_flag(double_quotes,codes).
was_indexed(_).

:- was_style_check(-atom).
:- was_style_check(-string).

:- use_module(library(shell)).      
:- use_module(library(shlib)).        
:- use_module(library(url)).        
:- use_module(library(quintus)).        

%:-set_prolog_flag(unknown,fail).
%:-set_prolog_flag(unknown,warn). 



swi_server(Port):- 
	current_prolog_flag(arch,'i386-win32'),!,
	tcp_socket(ServerSocket),
	please_tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 655),
	repeat,
	tcp_open_socket(ServerSocket, AcceptFd, _Useless),
	tcp_accept(AcceptFd, ClientSocket, _PeerIP), 
	work_on_client_socket('main',ClientSocket),
	fail.
	
swi_server(Port):- 
	thread_create(swi_server_thread(Port),_,[]).


please_tcp_bind(ServerSocket, Port):-
	catch((tcp_bind(ServerSocket, Port),writeFmt(user_error,'\nOS gave me port ~w. \n',[Port])),error(E,_),(writeFmt('\nWaiting for OS to release port ~w. \n(sleeping 4 secs becasue "~w")\n',[Port,E]),sleep(4),please_tcp_bind(ServerSocket, Port))).

swi_server_thread(Port):-
	tcp_socket(ServerSocket),
	(tcp_setopt(ServerSocket, reuseaddr) -> true; writeFmt(user_error,'You need to install the thread-safe version of socket.so (The prolog server may have bugs)\n',[])),
	please_tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 655),
	repeat,
	once(work_on_server_socket(ServerSocket)),
	fail.
	

win32_dispatch(AcceptFd) :-   % Non Multit Version 
         sleep(0.1),
         tcp_accept(AcceptFd, Socket, _PeerIP),
         writeq(_PeerIP),nl,
	 handle_the_socket(Socket,In, Out),
	 !,
	 win32_dispatch(AcceptFd).


	
work_on_server_socket(ServerSocket):-
	tcp_open_socket(ServerSocket, AcceptFd, _Useless),
	tcp_accept(AcceptFd, ClientSocket, _PeerIP), 
	work_on_client_socket_mt(ClientSocket).

work_on_client_socket_mt(ClientSocket):-
        threads,
	thread_create((
		thread_self(Self),
		mutex_create(Id),
		mutex_lock(Id),!,
		with_mutex(Id,work_on_client_socket(Self,ClientSocket)),
		mutex_unlock_all,
		thread_exit(complete)
	),_,[detatch(true)]).


work_on_client_socket_single(ClientSocket):-
	work_on_client_socket(single,ClientSocket).

work_on_client_socket(Self,ClientSocket):-
	tcp_open_socket(ClientSocket, In, Out),!,
	writeFmt(user_error,'% thread="~w" input="~w" output="~w" \n',[Self,In,Out]),
	flush_output(user_error),
	handle_the_socket(ClientSocket,In, Out).

handle_the_socket(ClientSocket,In, Out):-
		catch(thread_self(ID),_,ID=err),
		asserta(socket_in(ID,In)),
		asserta(socket_out(ID,Out)),
		catch(service_telnet_request(ID,In,Out),E,writeq(service_one_client(E))),
		retractAllProlog(socket_in(ID,_)),
		retractAllProlog(socket_out(ID,_)),
		ignore((
			catch(flush_output(Out),_,true),
			catch(tcp_close_socket(ClientSocket),_,true)
		)).

service_telnet_request(Self,In,Out):-
        writeFmt(Out,'<?xml version="1.0" encoding="ISO-8859-1"?>\n<answer thread="~w">\n',[Self]),
	flush_output(Out),
        once(( catch(read_term(In,CMD,[variable_names(Vars)]),_,
			(writeFmt(Out,'<badinput/>\n',[]),flush_output(Out)))
		)),
	getCputime(Start),
	invoke_cmd(Self,Out,CMD,Vars),
	statistics(cputime,End),
	Elapsed is End - Start,
	writeFmt(Out,'<statistics getCputime="~g"/>\n</answer>\n',[Elapsed]),	
      catch(flush_output(Out),_,true).

invoke_cmd(Self,Out,CMD,Vars):-
      tell(Out),
      retractAllProlog(answer_yes(Self)),
      catch(CMD,Err,(writeFmt('<error description="~w"/>',[Err]),!,fail)),
      assert(answer_yes(Self)),
      write_swi_vars_proc(Out,Vars),
      fail.

invoke_cmd(Self,Out,_,_):-   
         (retract(answer_yes(Self)) -> 
                writeFmt(Out,'<yes/>\n',[]) 
                 ;
                writeFmt(Out,'<no/>\n',[])),!. 


write_swi_vars_proc(Out,[]):-!.
write_swi_vars_proc(Out,Vars):-
         write(Out,'<result>'),
         write_swi_vars(Out,Vars),
         writeFmt(Out,'</result>\n',[]).
                              
write_swi_vars(Out,[]):-!.
write_swi_vars(Out,[Term|REST]):-  !,Term=..[_,N,V],
         writeFmt(Out,'<var name="~w">~q</var>',[N,V]),
         write_swi_vars(Out,REST).

call_as(UserToken,initialize):-term_to_atom(UserToken,ModuleName),assert(ModuleName:usertoken(UserToken)),assert(user:username(UserToken)),!.
call_as(UserToken,load_file(Filename)):-term_to_atom(UserToken,ModuleName),ModuleName:ensure_loaded(Filename),!.
call_as(UserToken,terminate):-term_to_atom(UserToken,ModuleName),terminate_the(ModuleName),!,retract(user:username(ModuleName)).
call_as(UserToken,Goal):-term_to_atom(UserToken,ModuleName),ModuleName:Goal.

call_all(Goal):-
	user:username(UserToken),
	once(call_as(UserToken,Goal)),fail.
call_all(Goal):-!.

terminate_modules(UserContraint):-
	user:username(UserContraint),
	once(call_as(UserToken,terminate)),fail.
terminate_modules(UserContraint):-!.

			
terminate_the(ModuleName):-
	current_predicate(_,ModuleName:Q),
	not(predicate_property(Q,built_in)),
	functor(Q,F,A),
	ModuleName:abolish(F/A),
	fail.
	
terminate_the(_ModuleName):-!.
	
	


serve_me:-repeat,once(serve_connection),retract(is_leaving).
goodbye:-saveSigmaCache,assert(is_leaving).

serve_connection(Id):-
	 (socket_in(Id,In)),			      
	 (socket_out(Id,Out)),
        service_telnet_request(Id,In,Out).

serve_connection:-
	 (socket_in(In)),			      
	 (socket_out(Out)),
        service_telnet_request(In,Out).
	


use_additional_context(P/N):-use_additional_context(P/N,'user').

use_additional_context(P/N,Add):-
        not(ground(P/N)),!,
	writeFmt(user_error,'Instanciation Error in use_additional_context/1-2.\n',[]),fail.
use_additional_context(P/N,Add):-
        context_module(Module),
        dynamic(Module:P/N),
         length(Args,N),
         CallerPrototype=..[P|Args],
         Prototype=..[P|Args],
         assert((Add:CallerPrototype:- Module:Prototype)),!.
	 										      
subcontext(user,_).
				      
:-dynamic(context_skolem/2).

:-dynamic(user:context_dag_db/2).


link_module_parent(NewParent):-context_module(Me),link_module_parent(Me,NewParent).

link_module_parent(Child,Parent):-not(ground((Child,Parent))),!,writeFmt(user_error,'Error: Arguments are not sufficiently Instanciated ~q \n',[link_module_parent(Child,Parent)]),!,fail.

link_module_parent(Child,Parent):-retractAllProlog(user:context_dag_db(Child,Parent)),!,assert(user:context_dag_db(Child,Parent)).

remove_module_parent(OldParent):-context_module(Me),remove_module_parent(Me,OldParent).

remove_module_parent(Child,Parent):-retractAllProlog(user:context_dag_db(Child,Parent)),!.


context_dag(User,Parent):-
	user:context_dag_db(User,Parent).
context_dag(User,Grandparent):-
	user:context_dag_db(User,Parent),
	context_dag(Parent,Grandparent).
	

load_file_into_module(File,PublicName):-expand_file_name(File,[Name]),!,
	open(Name,'read',Stream),!,
	repeat,
	once(handle_stream(Stream)),
	at_end_of_stream(Stream),!,close(Stream).
	
handle_stream(Stream):-	
		catch(read_term(Stream,X,[syntax_errors(true),module(PublicName),term_positon('$stream_position'(CharIndex,LineNum,Line,Pos))]),E,true),
		warn_call(catch(process_read(PublicName,X),E,writeFmt(user_error,'~q\n',[E]))),flush_output(user_error),!.
	
warn_call(X):-X,!.
warm_call(X):-writeFmt(user_error,'WARNING: Prdicate failed ~q \n',[X]),!.


process_read(PublicName,':-'(include(File))):-!. %,load_file_into_module(File,PublicName).
process_read(PublicName,':-'(X)):-!,PublicName:X.
process_read(PublicName,(X)):-!,assert(PublicName:X).


call_as(UserToken,initialize):-term_to_atom(UserToken,ModuleName),assert(ModuleName:usertoken(UserToken)),assert(user:username(UserToken)),!.
call_as(UserToken,load_file(Filename)):-term_to_atom(UserToken,ModuleName),load_file_into_module(Filename,ModuleName).
call_as(UserToken,terminate):-term_to_atom(UserToken,ModuleName),terminate_the(ModuleName),!,retract(user:username(ModuleName)).
call_as(UserToken,Goal):-term_to_atom(UserToken,ModuleName),run_under(Goal,ModuleName).


run_under(true,User):-!.
run_under(Goal,User):-run_under_only(Goal,User).
run_under(Goal,User):-
	context_dag(User,Parent),
	clause(Parent:Goal,Precons),
	run_under(Precons,User).

run_under_only(true,User):-!.
run_under_only(current:B,User) :- !,User:B.
run_under_only(_:Goal,User):- predicate_property(Goal,built_in),!,run_special(Goal,User).
run_under_only(U:B,User) :- User \==U ,!,run_under(B,U).
run_under_only(U:B,User) :- !,User==U,run_under_only(B,User).


run_under_only(Goal,User):- predicate_property(Goal,built_in),!,run_special(Goal,User).

run_under_only(Goal,User):-
	clause(User:Goal,Precons),
	run_under_only(Precons,User).

run_under_only(user:Goal,User):-
	clause(user:Goal,Precons),
	run_under_only(Precons,user).

% this could be circumvented if the user expicitly loaded his/her own library predicates.
run_under_only(Goal,User):-functor(Goal,F,A),index(F,A,_,File),once(use_module(library(File))),!,User:Goal.
	

	
run_special(B,User) :- cutin(B,B1,B2),!,
     run_under(B1,User),!,
     run_under(B2,User).

run_special(A -> B,User) :- !,
     run_under(A,User) -> run_under(B,User).

/*
run_special(A*-> B,User) :- !,
     run_under(A,User) *-> run_under(B,User).
*/

run_special((Goal1,Goal2),User):-!,
	run_under(Goal1,User),
	run_under(Goal2,User).
	
run_special((Goal1;Goal2),User):-!,
	run_under(Goal1,User);
	run_under(Goal2,User).

run_special(findall(Vars,Goal,List),User):-!,
	findall(User,run_under(Goal,User),List).
run_special(bagof(Vars,Goal,List),User):-!,
	bagof(User,run_under(Goal,User),List).
run_special(setof(Vars,Goal,List),User):-!,
	setof(User,run_under(Goal,User),List).
run_special(call(Goal),User):-!,
	setof(User,run_under(Goal,User),List).
run_special(once(Goal),User):-!,
	run_under(Goal,User),!.
run_special(ignore(Goal),User):-
	ignore(run_under(Goal,User)).
run_special(not(Goal),User):-
	not(run_under(Goal,User)).
run_special((Goal),User):-
	User:Goal.
		
cutin(!,!,true) :- !.
cutin((!,B),!,B) :- !.
cutin((A,!),(A,!),true) :- !.
cutin((A,B),(A,As),Bs) :- cutin(B,As,Bs).
	
	
:-dynamic(user:username/1).

user:username(default).

% This predicate Calls goal and returns Length answers begining with Start.
% Also this can keep infinate chains from taking place.

call_number_of(Goal,Start,Length):-
	End is Start + Length -1,
	context_module(Ctx),
	flag(Ctx,_,2),!,
	Goal,
	flag(Ctx,F,F+1),
	Start<F, ((F>End,!);true).

% given this program

q(X):-member(X,[a,b,c,d,e,f,g,h,i,j,k,l]).
		


% ==============================	
% Sending a File back to server
% ==============================	
insert_file_to_stream(O,Filename):-
	safe_file_open(Filename,'r',Input),
	putchar_stdout(O,Input).

putchar_stdout(O,Input):-at_end_of_stream(Input),close(Input),!.
putchar_stdout(O,Input):-get_code(Input,Char),put(O,Char),!,putchar_stdout(O,Input).


% Not part of program below

basic_facts:-
	call_as(joe,assert(has(dog))),
	call_as(joesSister,assert(has(rat))),
	call_as(joesMom,assert(has(cat))),
	call_as(joesDad,assert(has(tiger))),
	call_as(joesGrandpa,assert(keeps(bees))),
	call_as(user,assert(( has(A):-keeps(A) )) ). 
/*
by putting in 'user' This means the it becomes module_transparent meaning you do not need to use module_transparent/1 or dynamic/1 now.

This means all contexts (modules) inherit user automatically.
They also of course inherit if you    link_module_parent(child,Parent)

*/

 
build_family_tree:-
	link_module_parent(joe,joesMom),
	link_module_parent(joe,joesDad),
	link_module_parent(joesDad,joesGrandpa),
	link_module_parent(joesSister,joesMom),
	link_module_parent(joesSister,joesDad).

removed_dad:- remove_module_parent(joe,joesDad).

