% =====================================
% supplies hook specific architecture
% =====================================
:- redefine_system_predicate(with_mutex(_,_)).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

%:-use_module(library(threadutil)).        
thread_create(X,win32,_):-X.
thread_exit(_,_):-!.
thread_at_exit(_,_):-!.
thread_at_exit(_):-!.
thread_self(win32):-!.
thread_exit(_):-!.
threads:-!. %statistics.

mutex_unlock_all.

mutex_create(_).
:- redefine_system_predicate(mutex_lock/2).
mutex_lock(_,_).
mutex_lock(_).
with_mutex(_,G):-G.
mutex_unlock(_,_).
mutex_unlockall.


current_thread(main,running).

%:-assert((thread_util:open_xterm(T,In,Out):-sigma_server_break(T,In,Out))).


/*
:- redefine_system_predicate(message_hook(_,_,_)).

:- asserta(((user:message_hook(A,B,C) :- sigma_server_message_hook(A,B,C)))).

sigma_server_message_hook(trace_mode(on),B,Lines):-
	catch(thread_self(Id),_,fail),
	'$get_pid'(Pid),
	fmtString(Title, 'SWI-Prolog Thread ~w (pid ~d) interactor', [Id, Pid]),
	tty_in(Stream),
	thread_signal(main,(close(Stream))),
	set_input(Stream),
	set_output(user_output),
	thread_at_exit(thread_signal(main,set_input(Stream))).
*/
:- volatile(tty_in/1).
:- volatile(tty_out/1).
save_tty_streams:-current_input(Stream),assert(tty_in(Stream)),current_output(StreamO),assert(tty_out(StreamO)).
:- initialization(save_tty_streams,now).
:- initialization(save_tty_streams,restore_state).

:-dynamic(cpuend/1).

call_with_time_limit(Goal,Time,New):-
	retractAllProlog(cpuend(_)),
	getCputime(Now),
	Then is Now + Time,
	asserta(cpuend(Then)),!,Goal,
	getCputime(NewNow),New is Now-NewNow.
%call_with_time_limit(Goal,Time,timeout):-!.


%:-on_signal(2, _throw,call_break).
call_break(_):-call_break(_,_).
call_break(_,_):-retractAllProlog(cpuend(_)),asserta(cpuend(0)),on_signal(2, _throw,call_break).




clock_time_out:-getCputime(Now),cpuend(Then),Now>Then.
	
alarm(_).

bt(ID):-!.          
    

% ===================================================================
% getCleanCharsWhitespaceProper/2.. Cleans String Up before parser uses it
% ===================================================================

getCleanCharsWhitespaceProper([],[]):-!.
getCleanCharsWhitespaceProper(X,Z) :- !,logOnFailure(ascii_clean(X,Y)),!,logOnFailure(getCleanCharsWhitespaceProper3(Y,Z)),!.

% Converts not ANSI Chars to whitespace 
ascii_clean([],[]):-!.
ascii_clean([X|String],[Y|Out]) :- transpose_char(X,Y),!,ascii_clean(String,Out).


string_clean(X,X).

transpose_char(10,32).
%transpose_char(32,32).
%transpose_char(X,32):-not(integer(X)),!.
%transpose_char(X,32):-X<33,!.
transpose_char( X , X).
   
% Blocks of Spaces are removed from a Charlist 
getCleanCharsWhitespaceProper3([],[]).
getCleanCharsWhitespaceProper3([32],[]).
getCleanCharsWhitespaceProper3([10],[]).
getCleanCharsWhitespaceProper3([13],[]).
getCleanCharsWhitespaceProper3([32,32],[]).
getCleanCharsWhitespaceProper3([32,32,32],[]).
getCleanCharsWhitespaceProper3([X],[X]):-!.
getCleanCharsWhitespaceProper3([32,32,32,32,32,32,32|String],[32|Out]) :-!, getCleanCharsWhitespaceProper3(String,Out),!.
getCleanCharsWhitespaceProper3([32,32,32,32,32|String],[32|Out]) :- !,getCleanCharsWhitespaceProper3(String,Out),!.
getCleanCharsWhitespaceProper3([32,32,32|String],[32|Out]) :-!, getCleanCharsWhitespaceProper3(String,Out),!.
getCleanCharsWhitespaceProper3([32,32|String],[32|Out]) :- !,getCleanCharsWhitespaceProper3(String,Out),!.
getCleanCharsWhitespaceProper3([X|String],[X|Out]) :- !,getCleanCharsWhitespaceProper3(String,Out),!.
getCleanCharsWhitespaceProper3(X,X):-!.


/*


% ==============================================
%	Create the ADO connection
% ==============================================

adoConnect :-!.
adoConnect :- adoobj(connection,IP),!.

adoConnect :-
      ado_cnnstr(Connection),
 %     Connection="Driver={SQL Server};Server=MBUILD;UID=sa;PWD=;Database=SigmaWFS"',
      %sendNote(debug,contentManager,'Making ODBC Connecton',Connection),
		actx_errors_as_exceptions(true),
      catch((
            actx_create_object('ADODB.Connection',IPConn),
      		assert(adoobj(connection,IPConn)),
            actx_invoke_object(IPConn,'Open',[Connection],_),
      		actx_create_object('ADODB.Recordset',IPRset),
      		actx_create_object('ADODB.Command',IPCmd),
      		assert(adoobj(recordset,IPRset)),
      		assert(adoobj(command,IPCmd)),
      		adoobj(connection,IPConn),
      		actx_invoke_object(IPRset,'Open',['PrologMemory',IPConn],_),
            assert(ado_connected)
      ),E, sendNote(debug,contentManager,'ODBC Connecton Failed',E) ),
            actx_errors_as_exceptions(false).

			

			

adoUpdate_odbc(NO_ODBC_TODO,String,KB,Ctx,TN,Author):-!.
adoUpdate_odbc(auth,String,KB,Ctx,TN,Author):-!,
      execSQLf('Insert into PrologMemory (AssertionID,SourceForm,SourceText,KnowledgeBase,Context,Creator) VALUES ("~q","auth","~s","~q","~q","~q")',[TN,String,KB,Ctx,Author]).
adoUpdate_odbc(Form,CL,KB,Ctx,TN,Author):- !, 
      execSQLf('Insert into PrologMemory (AssertionID,SourceForm,SourceText,KnowledgeBase,Context,Creator) VALUES ("~q","~q","~q","~q","~q","~q")',[TN,Form,CL,KB,Ctx,Author]).

%adoUpdate_memory(Form,CL,KB,Ctx,TN,Author):-not('in-active-memory'(KB,Ctx)),!.
adoUpdate_memory(prolog,CL,KB,Ctx,TN,Author):-
            unnumbervars(CL,UNCL),logOnFailure(assert(UNCL)).
            %sendNote(extreme_debug,contentManager,'asserted to working memory',CL).

adoUpdate_memory(Form,CL,KB,Ctx,TN,Author):-!.

% ==============================================
%	Retrieve from ADO
% ==============================================
purge_entire_kb(KB):-retractAllProlog(sigmaCache(PredR,Form,Source,Prolog,KB,Ctx,TN,Author,_)),execSQLf('DELETE FROM PrologMemory WHERE KnowledgeBase LIKE "~q"',[KB]) .



get_store(cache_only,Form,Source,KB,Ctx,TN,Author):-sigmaCache(PredR,Format,PrologFormS,CL,KB,Ctx,TN,Author,_).

get_store(odbc,Form,Source,KB,Ctx,TN,Author):-
         once(select_rs(Form,Source,KB,Ctx,TN,Author,IPRset)),
         once(save_rs(IPRset,sigmaCache)),
         actx_release_object(IPRset),
         fail.
get_store(odbc,Form,Source,KB,Ctx,TN,Author):-sigmaCache(PredR,Form,Source,KB,Ctx,TN,Author).

:-dynamic(wc/2).

%sync_ado_cache:-!.
sync_ado_cache:-
        adoConnect,
         retractAllProlog(sigmaCache(PredR,_,_,_,_,_,_)),
         sendNote(power_user,contentManage,'Refreshing ADO/ODBC Cache',' '),
         once(select_rs(Form,Source,KB,Ctx,TN,Author,IPRset)),
         once(save_rs(IPRset,sigmaCache)),
         actx_release_object(IPRset).
sync_ado_cache:-!.
         

select_rs(Form,Source,KB,Ctx,TN,Author,IPRset):-         
         retractAllProlog(wc(_,_)),
         (var(Form) -> true ; assert(wc('SourceForm',Form))),
         (var(KB) -> true ; assert(wc('KnowledgeBase',KB))),
         (var(Ctx) -> true ; assert(wc('Context',Ctx))),
         (var(TN) -> true ; assert(wc('AssertionID',TN))),
         (var(Author) -> true ; assert(wc('Creator',Author))),
         get_where_clause(WC),
         execSQLf('SELECT SourceForm,SourceText,KnowledgeBase,Context,AssertionID,Creator FROM PrologMemory WHERE ~w Retracted=False ',[WC],IPRset).

get_where_clause(WC):-retract(wc(N,V)),get_where_clause(Next),fmtString(WC,'~w ~w LIKE "~q" AND ',[Next,N,V]).
get_where_clause(' ').   

save_rs(IPRset,sigmaCache):-
		      actx_invoke_object(IPRset,'moveFirst',[],_),
            save_rs_until_end(IPRset).
            
save_rs_until_end(IPRset):-save_rs_until_end_0(IPRset),!,save_rs_until_end(IPRset).
save_rs_until_end(IPRset).

save_rs_until_end_0(IPRset):-
            actx_invoke_object(IPRset,['EOF',propget],[],false),
            actx_invoke_object(IPRset,'GetString',[2,1,',','  ','_'],Value),!,
            catch(fmtString(S,'sigmaCache(PredR, ~s )',[Value]),_,true),
            %catch((atom_to_term(S,Term,V),assert(Term)),_,save_rs_until_end_1(IPRset)).
            catch((atom_to_term(S,Term,V),assert(Term)),_,true).

save_rs_until_end_1(IPRset):-
            actx_invoke_object(IPRset,['Fields',propget],[],IPFields),
            get_name_value(IPFields,'AssertionID',AssertionID),
            get_name_value(IPFields,'SourceForm',SourceForm),
            get_name_value(IPFields,'SourceText',SourceText),
            get_name_value(IPFields,'KnowledgeBase',KnowledgeBase),
            get_name_value(IPFields,'Context',Context),
            get_name_value(IPFields,'Creator',Creator),
            assert(sigmaCache(PredR,SourceForm,SourceText,KnowledgeBase,Context,AssertionID,Creator)),
		      actx_invoke_object(IPRset,'MoveNext',[],_).


get_name_value(IPFields,Name,Value):-
            actx_invoke_object(IPFields,['Item','propget'],[Name],IPField1),
            actx_invoke_object(IPField1,['Value','propget'],[],Value),
            actx_release_object(IPField1). %,char_code(ValueA,Value).
            



% ==============================================
%	execSQL % 259-6225
% ==============================================
      
execSQL(Cmd):-
		execSQL(Cmd,IPRset),!,actx_release_object(IPRset),!.

execSQL(Cmd,IPRset):-
		adoobj(connection,IPConn),!,
		actx_invoke_object(IPConn,['execute',func],[Cmd],IPRset),!.

execSQLf(Format,Args):-
      fmtString(S,Format,Args),
      string_to_atom(S,A),
      execSQL(A),!.

execSQLf(Format,Args,IPRset):-
      fmtString(S,Format,Args),
      string_to_atom(S,A),
      execSQL(A,IPRset),!.
		
actx_run(RUN):-
      actx_errors_as_exceptions(true),
		catch(RUN,
				activex_error(StrFunc,StrDesc,TermInError),
				(sendNote(debug,contentManger,['ActiveX Error: ',StrFunc],[StrDesc,TermInError]),actx_errors_as_exceptions(true))),actx_errors_as_exceptions(true).

                      


*/
