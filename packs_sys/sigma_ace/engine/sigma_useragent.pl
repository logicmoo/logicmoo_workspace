% ===================================================================
% File 'useragent.pl'
% Author: Douglas Miles
% Contact: dmiles@teknowledge.com ; apease@teknowledge.com
% Version: 'sigma_useragent.pl' 1.0.0
% Revision:		$Revision: 1.44 $
% Revised At:	$Date: 2002/03/04 22:48:23 $
% History:
% Created - 2000/11/10 dmiles@teknowledge.com

% ===================================================================
% PURPOSE  
% ===================================================================
% This file meets the needs of an external agent working for the needs of eigther an automated or human user 
% Interfacea with Java with XML to display proof trees and variable bindings
% It defines the default settings most users will use and gives a starting expected state.
% Ask a KB, tell to a KB, retract from a KB, consult the user (this function is currently treated by ua_command/2, but it is planned to separate it), 
% Report status to Sigma, initialization of the LE, and file-handling routines 
% ===================================================================

% ===================================================================
% EXPORTS   (Called  only by XSB Java server beans)
% ===================================================================
:-include('sigma_header.pl').

/*:-module(sigma_useragent,
            [
            initializeSigmaServerData/0,   % Loads appropriate KBs

            ua_ask/3,        % handles user invoked query
            ua_tell/4,         % handles user invoked assertions
            ua_retract/4,    % handles user invoked retractions
            ua_command/2, %allows user to define new terms, and handles consultation with user
            
            define_kb/1,  %selects user's choice of a kb to load
            rename_kb/1,  %allows server to rename a kb
            reconstitute_kb/1,  %allows server to redefine the location of a kb
            delete_kb/1,  %allows server to delete a kb from the IE
            
            establish_status/1,
            establish_status/2,  %loads and unloads kbs, reports status to server,
            
            verify_status/1,
            verify_status/2,  %only reports status of kbs,
            
            server_startup_status/1,  %presets multiple kbs to be loaded at startup of IE,
            kb_startup_status/1,  %presets a single kb to be loaded at startup of IE,
            
            ua_read/2 % -- (to be dropped) loads a single kb
            ]).
  */


% ===================================================================
% Other major predicates: 
% agentConsultation/3 (called by ua_out in sigma_response.pl, also by writeUAEvent, and by command_proc/3, by any predicate marked for consultation)
% ===================================================================

% ===================================================================
% DEBUG PREDICATES
% ===================================================================

fme2:-ua_ask("(?P2 ?A1 ?2)", 'ToplevelContext', [opt_kb='MergeAddition',disp_debug=true,disp_note_user=true,disp_notes_nonuser=true]).
fme3:-ua_ask("(?P3 ?A1 ?A2 ?A3)", 'ToplevelContext', [opt_kb='MergeAddition',disp_debug=true,disp_note_user=true,disp_notes_nonuser=true]).
gme2:-ua_ask("(genls ?X ?Y)").

% ===================================================================
% IMPORTS
% ===================================================================


% Stored persistantly as  kb_make_status_start(kb(Name,CanFileLocal)=StartupStatus)  in sigma_persist.wfs


% ===================================================================
%  BOOTUP SIGMA SERVER
% ===================================================================

% This Function when called from outside does the Embeding the Logic Engine into the XSB Server
initializeSigmaServerData :-!,
      setPrologFlag(architecture,Architecture),
      setPrologFlag(version,Version),
      setPrologFlag(host_cpu,Host_cpu),
      setPrologFlag(install_dir,Install_dir),
      setPrologFlag(release_date,Release_date),
      setPrologFlag(scheduling_strategy,Scheduling_strategy),
     % findall(L,library_directory(L),Library_directory),
      sendNote(user,logicEngine,'Initializing and Embedding Sigma Logic Engine',['architecture=',Architecture,Host_cpu,Version,Release_date,Scheduling_strategy]),
      establish_startup_state,
      sendNote(debug,logicEngine,'KBs are now loaded by contentManager',' '),
      ensureSigmaKB('SigmaKernel','ToplevelContext'),!.

%:-include('sigma_header.pl').

% ===========================================================
% CONVERSE WITH JAVA
% ===========================================================

sigma_ua(OptionsIn):-
		ignore(reset_wd),
                logOnFailure(fixOptionsFromWeb(OptionsIn,OptionsIn0)),
                logOnFailure(fixOptionsFromWeb(OptionsIn0,OptionsIn1)),
		logOnFailure(fixOptionsFromForeign(OptionsIn1,Options)),
		logOnFailure(setSigmaOption(Options)),
                nl,writeq(user_error,setSigmaOption(Options)),nl,
		ensureSigmaOption(client,html,ResponseType),
		writeModeSet(ResponseType),!,
		logOnFailure(parse_sigma_ua(Options)),!.

reset_wd:-'LOGIC_ENGINE_RT'(Local),cd(Local).
reset_wd.

fixOptionsFromWeb([],[]):-!.
fixOptionsFromWeb(N,''):- atom(N), name(N,Codes),name(end_of_file,Codes),!.
fixOptionsFromWeb([file=end_of_file|List],[file=''|ARGS]):- !, fixOptionsFromWeb(List,ARGS).
fixOptionsFromWeb([N=V|List],[NN=VV|ARGS]):- fixOptionsFromWeb(N,NN),fixOptionsFromWeb(V,VV), !, fixOptionsFromWeb(List,ARGS).
fixOptionsFromWeb(N,NN):- atom(N),name(N,Codes),fixCodes(Codes,CodesN),name(NN,CodesN),!.
fixOptionsFromWeb(List,List):-!.

fixCodes(Codes,CodesO):- append(CodesL,[C],Codes),char_type(C,white),!,fixCodes(CodesL,CodesO).
fixCodes(Codes,CodesO):- append([32],CodesL,Codes),char_type(C,white),!,fixCodes(CodesL,CodesO).
fixCodes(Codes,Codes).

fixOptionsFromForeign([],[]):-!.

fixOptionsFromForeign([context=Value|List],[opt_ctx_assert=Value,opt_ctx_query=Value|ARGS]):-     %TODO Sepatate KB/Ctx
	  fixOptionsFromForeign(List,ARGS).
fixOptionsFromForeign([ctx=Value|List],[opt_ctx_assert=Value,opt_ctx_query=Value|ARGS]):-     %TODO Sepatate KB/Ctx
	  fixOptionsFromForeign(List,ARGS).
fixOptionsFromForeign([sf=AValue|List],[sf=AValue|ARGS]):-atom(AValue),!,
	  logOnFailure(fixOptionsFromForeign(List,ARGS)).
fixOptionsFromForeign([sf=String|List],[sf=AValueClean|ARGS]):-is_list(String),
	  logOnFailure(string_to_atom(String,AValueClean)),!,
	  logOnFailure(fixOptionsFromForeign(List,ARGS)).
fixOptionsFromForeign([From=AValue|List],[To=DValue|ARGS]):-
          transform_option(From,To),!,
	  logOnFailure(safe_atom_to_term(AValue,DValue)),
	  fixOptionsFromForeign(List,ARGS).
fixOptionsFromForeign([AName=AValue|List],[DName=DValue|ARGS]):-  
	  logOnFailure(safe_atom_to_term(AName,DName)),
	  logOnFailure(safe_atom_to_term(AValue,DValue)),
	  logOnFailure(fixOptionsFromForeign(List,ARGS)).
fixOptionsFromForeign([AName|List],[DName|ARGS]):-  
	  safe_atom_to_term(AName,DName),
	  logOnFailure(fixOptionsFromForeign(List,ARGS)).

% transform_option(From,To)
transform_option(author,user).
transform_option(kb,opt_kb).
transform_option(language,interp).
transform_option(timeLimit,opt_timeout).
transform_option(bindingLimit,opt_answers_max).
transform_option(depthLimit,opt_backchains_max).


parse_sigma_ua(Options):-memberchk(cmd='Halt LE',Options),halt.
parse_sigma_ua(Options):-memberchk(cmd='Rebuild',Options),write('Rebuilding Sigma').


%parse_sigma_ua(Options):-memberchk(command=html,Options),!,
	
parse_sigma_ua(Options):-memberchk(client=soap,Options),!,
	parse_sigma_soap(Options).

parse_sigma_ua(Options):-memberchk(client=sigma_xml,Options),!,
	parse_sigma_soap(Options).

% ===========================================================
% Surface Returns External TN
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=tn_find,Options),!,
	ensureSigmaOption(opt_ctx_assert,'ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb,'Merge',KB),
	ensureSigmaOption(client,'java',CLIENT),
	ensureSigmaOption(sf,surf,Assertion),
	atom_codes(Assertion,Assertion_Chars),
	ensureSigmaOption(user,'Web',User),
         logOnFailure(getCleanCharsWhitespaceProper(Assertion_Chars,Show)),!,
         logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
         logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!,
	 write_out_kif_tn(Assertion_Chars,NEWFORM,Vars,Ctx,KB,Author),!.
	 
write_out_kif_tn(Assertion_Chars,computed(comment(_)),Vars,Ctx,KB,Author):-
	 writeFmt('Syntax Error: Unmatched parentheses in "~s"\n',[Assertion_Chars]).
write_out_kif_tn(Assertion_Chars,comment(_),Vars,Ctx,KB,Author):-
	 writeFmt('Syntax Error: Unmatched parentheses in  "~s"\n',[Assertion_Chars]).
write_out_kif_tn(Assertion_Chars,NEWFORM,Vars,Ctx,KB,Author):-
	copy_term((NEWFORM),(CNF)),
	numbervars(CNF,'$VAR',0,_),
	sigmaCache(PredR,surface,CNF, _, KB,Ctx, TN, Author, O),
	sigmaCache(PredR,surface,OF, _, KB,Ctx, TN, Author, O),
	numbervars(OF,'$VAR',0,_),
	OF == CNF,!,
	writeFmt('~w\n',[TN]),!.
write_out_kif_tn(Assertion_Chars,NEWFORM,Vars,Ctx,KB,Author):-
	 writeFmt('Not found in ~w of ~w "~s"\n',[Ctx,KB,Assertion_Chars]).

% ===========================================================
% Detroys an External TN
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=tn_delete,Options),!,
	ensureSigmaOption(opt_ctx_assert,'ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb,'Merge',KB),
	ensureSigmaOption(client,'java',CLIENT),
	ensureSigmaOption(tn,0,TN),
	ensureSigmaOption(sf,surf,Assertion),
	atom_codes(Assertion,Assertion_Chars),
	ensureSigmaOption(user,'Web',User),
	destroyTN(KB,TN,Ctx),
	saveSigmaCache.

destroyTN(KB,TN,Ctx):-
	retractall(sigmaCache(Literal,_,KB,Ctx,TN)),  %Facts
	retractall(sigmaCache(Literal,AnteLiteral,_,KB,Ctx,TN)),	 %Rules 
	retractall(sigmaCache(Surface,CLF,Flags,Vars,KB,Ctx,TN,Author,TMResult)).
	
destroyKB(KB):-
	retractall(sigmaCache(KB,_)),
	retractall(sigmaCache(KB,_,_)),
	retractall(sigmaCache(KB,_,_,_)),
	retractall(sigmaCache(Literal,_,KB,Ctx,TN)),  %Facts
	retractall(sigmaCache(Literal,AnteLiteral,_,KB,Ctx,TN)),	 %Rules 
	retractall(sigmaCache(Surface,CLF,Flags,Vars,KB,Ctx,TN,Author,TMResult)),
	saveSigmaCache.
	
% ===========================================================
% Shows whats known about an External TN
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=tn_show,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(client='java',CLIENT),
	ensureSigmaOption(tn=0,TN),
	ensureSigmaOption(sf=surf,Assertion),
	writeKnownFormsTN(_,KB:TN).

writeKnownFormsTN(Ctx,KB:Word):-(atom(Word)),!,
	writeKnownFormsAboutTerm(Ctx,KB:Word).

writeKnownFormsAboutTerm(Ctx,KB:Word):-
	sigmaCache(PredR,Fact,Pre,Type,true,KB,Agent,P),
	contains_const((Fact,Pre),Word),
	flag(proof_linenumber,LN,1),
	once(writeObject(P,_)),
	write('<hr>'),
	fail.

writeKnownFormsAboutTerm(Ctx,KB:Word):-
	sigmaCache(PredR,Fact,Type,true,KB,Agent,P),
	contains_const(Fact,Word),
	flag(proof_linenumber,LN,1),
	once(writeObject(P,_)),
	write('<hr>'),
	fail.

writeKnownFormsAboutTerm(Ctx,KB:Word).
		
contains_const(Fact,Word):-
	getConstants(atomic,Fact,List,_,_),!,
	memberchk(Word,List).

writeKnownFormsTN(Ctx,KB:TN):-    
	writeFmt('\n</pre><H3><br>Compiled Forms</H3><pre>\n',[]),fail.
	
writeKnownFormsTN(Ctx,KB:TN):-     
	flag(indent,_,0),
	sigmaCache(Surface,CLF,Flags,Vars,KB,Ctx,TN,Author,TMResult),
	once(tam(Surface,Vars)),fail.
/*
writeKnownFormsTN(Ctx,KB:TN):-     
	flag(indent,_,0),
	sigmaCache(Surface,CLF,Flags,Vars,KB,Ctx,TN,Author,TMResult),
	once(writeObject(writeq(sigmaCache(Flags,Vars,KB,Ctx,TN,Author,TMResult)),Vars)),fail.

writeKnownFormsTN(Ctx,KB:TN):-     
	flag(indent,_,0),
	sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,TMResult),
	flag(clause_id,_,0),
	tam(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result),fail.
*/	
writeKnownFormsTN(Ctx,KB:TN):-    
	writeFmt('\n</pre><H3><br>Active Forms</H3><pre>\n',[]),fail.

writeKnownFormsTN(Ctx,KB:TN):-     
	flag(clause_id,CLID,CLID),
	format('<hr>Clauses: ~w',[CLID]),fail.

writeKnownFormsTN(Ctx,KB:TN):-     
	sigmaCache(Fact,Pre,Cost,KB,Agent,TN),
	list_to_and(Pre,Ands),
	flag(indent,_,0),
	once(writeObject('<hr>',Vars)),
	once(writeObject(writeq(Fact:Pre),Vars)),
	once(writeObject(nl,Vars)),
	once(writeObject('KB |= '(Ands,Fact),Vars)),fail.

writeKnownFormsTN(Ctx,KB:TN):-     
	sigmaCache(Fact,Cost,KB,Agent,TN),
	flag(indent,_,0),
	once(writeObject('<hr>',Vars)),
	once(writeObject(Fact,Vars)),fail.

/*
writeKnownFormsTN(Ctx,KB:TN):- 
	isSigmaOption(opt_debug=on),
	writeKnownFormsTN_used(Ctx,KB:TN).
*/		
writeKnownFormsTN(Ctx,KB:TN):-writeFmt('</pre>').
	
writeKnownFormsTN_used(Ctx,KB:TN):-
	writeFmt('\n</pre><H3><br>Inference Forms</H3><pre>\n',[]),
	retractall(query_kb(X)),
	asserta(query_kb(KB)),  
	sigmaCache(PredR,Fact,Ctx,KB:TN:_^Vars),
	t_ado_cache(PredR,_,Fact, Vars, KB,Ctx, TN, Surf, on),
	writeFmt('\n<hr>'),fail.

writeKnownFormsTN_used(Ctx,KB:TN):-	 
	sigmaCache(PredR,Fact, Ctx, Pre, KB:TN:_^Vars),
	toMarkUp(html,formula(entails(Pre,Fact)),Vars,O),
	writeFmt('\n~w<hr>',[O]),fail.

	
% ===========================================================
% Ask
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=ask,Options),!, make,
	ensureSigmaOption(opt_ctx_query='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(sf=surf,Askion),
	atom_codes(Askion,Askion_Chars),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(interp='kif',Interp),
	logOnFailure(getCleanCharsWhitespaceProper(Askion_Chars,Show)),!,
	logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
	logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!,
	logOnFailure(invokeOperation(quiet,query(NEWFORM),Ctx,TrackingAtom,KB,User,Vars)).


% ===========================================================
% Add
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=tn_sync,Options),!,
       %  writeFmt(user_error,'ua: ~w\n',[Options]),flush_output(user_error),
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(tn=_,EXTID),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(sf=surf,Assertion),
	logOnFailure(atom_codes(Assertion,Codes)),
	getSurfaceFromChars(Codes,STERM,Vars),
	getSigmaTermFromSurface(STERM,Surface),
	destroyTN(KB,EXTID,_),
	once(invokeTell([trusted,canonicalize],surface,Surface,Ctx,EXTID,KB,Vars,User)),!.
	
parse_sigma_ua(Options):-memberchk(submit=tn_form_update,Options),!,
       %  writeFmt(user_error,'ua: ~w\n',[Options]),flush_output(user_error),
	ensureSigmaOption(tn=_,EXTID),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(opt_kb='Merge',KB),	
	ensureSigmaOption(sf=surf,Assertion),
	logOnFailure(atom_codes(Assertion,Codes)),
	getSurfaceFromChars(Codes,STERM,Vars),
	getSigmaTermFromSurface(STERM,Surface),
	logOnFailure(retract(sigmaCache(PredR,surface,OldSurf,_,KB,Ctx,EXTID,_,_))),
	destroyTN(KB,EXTID,_),
	once(invokeTell([trusted,canonicalize],surface,Surface,Ctx,EXTID,KB,Vars,User)),!.

parse_sigma_ua(Options):-memberchk(submit=canonicalize,Options),!,
	ensureSigmaOption(opt_kb='Merge',KB),
	once(invokeKBCompilerThread(KB)),write_ln('canonicalizing.\n').	

parse_sigma_ua(Options):-memberchk(submit=blank_kb,Options),!,
	ensureSigmaOption(opt_kb='Merge',KB),	
	destroyKB(KB),
	saveSigmaCache.


parse_sigma_ua(Options):-memberchk(submit=removeKbNameSpace,Options),!,
	ensureSigmaOption(opt_kb='Merge',KB),	
	destroyKB(KB),
	retractall(sigmaCache(instance,surface,'instance'(KB,'KnowledgeBase'),'$VAR'(0),'SigmaKernel','ToplevelContext',TN1,'WebUser',gaf)),
	retractall(sigmaCache(instance,surface,'instance'(Ctx,'Context'),'$VAR'(0),KB,'ToplevelContext',TN2,'WebUser',gaf)),
	retractall(sigmaCache(instance,surface,'instance'('ToplevelContext','Context'),'$VAR'(0),KB,'ToplevelContext',TN2,'WebUser',gaf)),
	retractall(sigmaCache(instance,surface,'sourcefile-of'(KB,Filename),'$VAR'(0),'SigmaKernel','ToplevelContext',TN3,'WebUser',gaf)),
	!.

% ===========================================================
% Verify Surface Returns SUO-KIF
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=verify_assert,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(client='java',CLIENT),
	ensureSigmaOption(sf=surf,Assertion),
	atom_codes(Assertion,Assertion_Chars),
	ensureSigmaOption(user='Web',User),
         logOnFailure(getCleanCharsWhitespaceProper(Assertion_Chars,Show)),!,
         logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
         logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!, 
	 write_out_kif(Assertion_Chars,NEWFORM,Vars,Ctx,KB,Author).
	 
write_out_kif(Assertion_Chars,computed(comment(_)),Vars,Ctx,KB,Author):-
	 writeFmt('Syntax Error: Unmatched parentheses in "~s"',[Assertion_Chars]).
write_out_kif(Assertion_Chars,comment(_),Vars,Ctx,KB,Author):-
	 writeFmt('Syntax Error: Unmatched parentheses in "~s"',[Assertion_Chars]).
write_out_kif(Assertion_Chars,NEWFORM,Vars,Ctx,KB,Author):-
	logOnFailure(getTruthCheckResults(tell,[untrusted],surface,NEWFORM,Ctx,STN,KB,Vars,Author,Result)),
	(Result=accept(_) -> 
			(toMarkUp(kif,NEWFORM,Vars,Out),writeFmt('~w\n',[Out]),!)
			;
			(
			Result=notice(FormatStr,Args),
			writeFmt('error:\n',[]),
			writeFmt(FormatStr,Args)
			)
	),!.

% ===========================================================
% Draw Context DAG in HTML
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=show_dag,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(user='Web',User),
	writeFmt('<pre>\n',[]),
	retractall(drawn(_)),
	draw_context_dag(Ctx,KB,Ctx,0),!,
	writeFmt('</pre>\n',[]).

tcdag(C):-retractall(drawn(_)),draw_context_dag('Merge',C,0).

:-dynamic(drawn/1).

draw_context_dag(KB,Ctx,N):-
	assert(drawn(Ctx)),
	make_space(N,O),
	writeFmt('~w<strong>~w</strong>\n',[O,Ctx]),
	NN is N + 1,!,
	show_subs(KB,Ctx,NN).

show_subs(KB,Ctx,8):-!.
%show_subs(Top,KB,Top,_):-!.
	
show_subs(KB,Ctx,NN):-
	context_dag(KB,Ctx,Sub),
	not(drawn(Sub)),
	draw_context_dag(KB,Sub,NN),
	fail.
show_subs(KB,Ctx,NN):-!.	
	
	
make_space(1,' ->'):-!.
make_space(0,''):-!.
make_space(N,O):-
	NN is N -1,
	make_space(NN,M),
	atom_concat('   ',M,O),!.



% ===========================================================
% Tell
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=assert,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(sf=surf,Assertion),
	atom_codes(Assertion,Assertion_Chars),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(interp='kif',Interp),
	logOnFailure(ensureSigmaOption(tn=_,TN)),
        logOnFailure(getCleanCharsWhitespaceProper(Assertion_Chars,Show)),!,
        logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
        logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!,
	logOnFailure(invokeOperation(verbose,assert(NEWFORM),Ctx,TN,KB,User,Vars)).
	

% ===========================================================
% Start Canonicalizer
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=canonicalize,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	logOnFailure(invokeKBCompilerThread(KB)).
	

% ===========================================================
% Create Knowledge Base (New)
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=register_kb,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(client='java',CLIENT),
	ensureSigmaOption(user='Web',User),
	saveSigmaCache,
	getDefaultImageFilepath(IF),
	open(IF,append,Handle,[close_on_abort(false),buffer(full)]),
	assert(save_can_to_file(KB,Handle)),
	!.

:-dynamic(save_can_to_file/2).

% ===========================================================
% Retract
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit=retract,Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(sf=surf,Retraction),
	atom_codes(Retraction,Retraction_Chars),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(interp='kif',Interp),
	give_kif_window,
         getCleanCharsWhitespaceProper(Retraction_Chars,Show),!,
         logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
         logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!,
              once(( NEWFORM=comment(_) -> 
                     (do_chars(Show),!,FORM=_) ;(!,
		     logOnFailure(invokeOperation(verbose,retract(NEWFORM),Ctx,TN,KB,User,Vars))		     
		     ))).


% ===========================================================
% Delete Assertion
% ===========================================================
parse_sigma_ua(Options):-memberchk(submit='Delete Assertion',Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),
	ensureSigmaOption(asid=_,AID),
	ensureSigmaOption(user='Web',User),
	ensureSigmaOption(interp='kif',Interp),
	writeFmt('<H3><Font Color=Red>Deleted....</Font></H3>',[]),
	delete_assertion(AID).
	
delete_assertion(AID):-
	retract(sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,_)),
	delete_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author),
	fail.
	
delete_assertion(AID):-writeFmt('<H3><Font Color=Red>Done Deleting.</Font></H3>',[]).

delete_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author):-
	toMarkUp(html,SURF,Vars,SAtom),
	writeFmt('<IMG src="pixmaps/bullet.gif" asrtid=~w><nobr>',[AID]),
	writeFmt('<b>~w</b> ID<font color=red>~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font>',[Form,AID,KB,Ctx,Author]),
	%format_o('&nbsp;&nbsp;~w&nbsp;Enabled&nbsp;&nbsp;<br>',checkbox(AID,OnOff)),
	writeFmt('~w<br>',[SAtom]),!.		

parse_sigma_ua(Options):-
	memberchk(cmd='Show Cache',Options),!,
	writeFmt('<H3><Font Color=Red>Listing Cache Assertions...</Font></H3><PRE>',[]),	
	listing(sigmaCache),
	writeFmt('</PRE><BR><B>Done.</B>',[]),!.


listingt(NH):-catch((string_to_atom(NH,AA),listing(AA)),_,true).
%listingt(NH):-listing(NH),!.

% ===========================================================
% Invoke Prolog Command
% ===========================================================
parse_sigma_ua(Options):- %memberchk(interp='prolog',Options),!,
	memberchk(submit=command,Options),!, make,
	ensureSigmaOption(sf=surf,Prolog),
	ignore(parse_prolog_cmd(Prolog)).
	
parse_prolog_cmd(Prolog):-
	give_kif_window,
	catch(atom_to_term(Prolog,CMD,Vars),E,
	(message_to_string(E,S),writeFmt('\nCall "~w" could not be read.  \nError: ~s\n',[Prolog,S]))),!,
	callFromWeb(Prolog,CMD,Vars),!.
parse_prolog_cmd(Prolog):-writeFmt('\nCall "~w" failed',[Prolog]),!.
callFromWeb(Prolog,CMD,Vars):-var(CMD),!.
callFromWeb(Prolog,CMD,Vars):-
	thread_self(Id),
	socket_out(Id,Out),
	socket_in(Id,In),
	invokePrologCommand(Id,In,Out,CMD,Vars,Results),
	writeFmt('\n       Results: ~w\n',[Results]),!.

	


% ===========================================================
% Logical Form Display
% ===========================================================
parse_sigma_ua(Options):-memberchk(logicforms=logicforms,Options),!,
       parse_sigma_lf(Options).
 

% ===========================================================
% Un-Canonicalize on KB/Ctx
% ===========================================================

parse_sigma_ua(Options):-
	memberchk(cmd='Un-Canonicalize',Options),!,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),!,
	(unsetSigmaOption(opt_surface_check=_)),
	(setSigmaOption(opt_surface_check=untrusted)),
	(unsetSigmaOption(opt_tracking_number=_)),
	(setSigmaOption(opt_tracking_number=supplied)),!,
	(unsetSigmaOption(opt_canonicalizer=_)),
	(setSigmaOption(opt_canonicalizer=byrd)),!,
	writeFmt('<H2>Un-Canonicalizing  KB:~w Ctx:~w.  This process may take several minutes.. Do not navigate away.</H2>',[KB,Ctx]),
	mark_all_surface_to_uncanonicalized(KB,Ctx),!.
	
% ===========================================================
% Invoke Load SKB on KB/Ctx (POST)
% ===========================================================

parse_sigma_ua(Options):-
		memberchk(KB='Load SKB',Options),!,
		(unsetSigmaOption(opt_kb=_)),
		(setSigmaOption(opt_kb=KB)),
		ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
		ensureSigmaOption(opt_kb='Merge',KB),!,
		fmtString(FileChars,'C:/sigmal/SUO/~w.can',[KB]),!,string_to_atom(FileChars,Filename),!,
		(unsetSigmaOption(opt_surface_check=_)),
		(setSigmaOption(opt_surface_check=trusted)),
		idGen(TN1),
		idGen(TN2),
		idGen(TN3),
		idGen(TN4),
		retractall(sigmaCache(PredR,_,_,_,KB,Ctx,_,_,_)),
		assertaClean(sigmaCache(instance,surface,'instance'(KB,'KnowledgeBase'),'$VAR'(0),'SigmaKernel','ToplevelContext',TN1,'WebUser',gaf)),
		assertaClean(sigmaCache(instance,surface,'instance'(Ctx,'Context'),'$VAR'(0),KB,'ToplevelContext',TN4,'WebUser',gaf)),
		assertaClean(sigmaCache(instance,surface,'instance'('ToplevelContext','Context'),'$VAR'(0),KB,'ToplevelContext',TN2,'WebUser',gaf)),
		assertaClean(sigmaCache('sourcefile-of',surface,'sourcefile-of'(KB,Filename),'$VAR'(0),'SigmaKernel','ToplevelContext',TN3,'WebUser',gaf)),
		load_kif_to_kb_ctx(KB,Filename,'ToplevelContext','SigmaWeb').

% ===========================================================
% load_from_can_file (Knowledge Base File)
% ===========================================================
parse_sigma_ua(Options):-memberchk(cmd='Load SKB',Options),!,
		ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
		ensureSigmaOption(opt_kb='Merge',KB),!,
		fmtString(FileChars,'C:/sigmal/SUO/~w.can',[KB]),!,string_to_atom(FileChars,Filename),!,
		(unsetSigmaOption(opt_surface_check=_)),
		(setSigmaOption(opt_surface_check=trusted)),
		idGen(TN1),
		idGen(TN2),
		idGen(TN3),
		idGen(TN4),
		retractall(sigmaCache(PredR,_,_,KB,Ctx,_,_,_)),
		assertaClean(sigmaCache(instance,surface,'instance'(KB,'KnowledgeBase'),'$VAR'(0),'SigmaKernel','ToplevelContext',TN1,'WebUser',gaf)),
		assertaClean(sigmaCache(instance,surface,'instance'(Ctx,'Context'),'$VAR'(0),KB,'ToplevelContext',TN4,'WebUser',gaf)),
		assertaClean(sigmaCache(instance,surface,'instance'('ToplevelContext','Context'),'$VAR'(0),KB,'ToplevelContext',TN2,'WebUser',gaf)),
		assertaClean(sigmaCache('sourcefile-of',surface,'sourcefile-of'(KB,Filename),'$VAR'(0),'SigmaKernel','ToplevelContext',TN3,'WebUser',gaf)),
		load_kif_to_kb_ctx(KB,Filename,'ToplevelContext','SigmaWeb').
		

% ===========================================================
% Invoke Canonicalizer on KB/Ctx (GET)
% ===========================================================
parse_sigma_ua(Options):-
	memberchk(cmd='Canonicalize',Options),!,%trace,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),!,
	(unsetSigmaOption(opt_surface_check=_)),
	(setSigmaOption(opt_surface_check=untrusted)),
	(unsetSigmaOption(opt_tracking_number=_)),
	(setSigmaOption(opt_tracking_number=supplied)),!,
	(unsetSigmaOption(opt_canonicalizer=_)),
	(setSigmaOption(opt_canonicalizer=byrd)),!,
	writeFmt('<H2>Canonicalizing  KB:~w Ctx:~w.  This process may take several minutes.. <br>This process must be done once and should not be interupted<br>Wait until this page is completely loaded before clicking <br>any links and do not navigate away.</H2>',[KB,Ctx]),
	logOnFailure(canonicalizeSigmaKBHTML(KB,Ctx)),!.

% ===========================================================
% Invoke Canonicalizer on KB/Ctx (POST)
% ===========================================================
parse_sigma_ua(Options):-
	member(KB='Canonicalize',Options),!,
	(unsetSigmaOption(opt_kb=_)),
	(setSigmaOption(opt_kb=KB)),
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	(unsetSigmaOption(opt_surface_check=_)),
	(setSigmaOption(opt_surface_check=untrusted)),
	(unsetSigmaOption(opt_tracking_number=_)),
	(setSigmaOption(opt_tracking_number=supplied)),!,
	(unsetSigmaOption(opt_canonicalizer=_)),
	(setSigmaOption(opt_canonicalizer=byrd)),!,
	writeFmt('<H2>Canonicalizing  KB:~w Ctx:~w.  This process may take several minutes.. <br>This process must be done once and should not be interupted<br>Wait until this page is completely loaded before clicking <br>any links and do not navigate away.</H2>',[KB,Ctx]),
	logOnFailure(canonicalizeSigmaKBHTML(KB,Ctx)),!.


% ===========================================================
% Invoke Surface Checker on KB/Ctx
% ===========================================================
parse_sigma_ua(Options):-
	memberchk(cmd='Surface Check',Options),!,%trace,
	ensureSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	ensureSigmaOption(opt_kb='Merge',KB),!,
	logOnFailure(surface_check_entire_kb_ctx(KB,Ctx)),!,
	writeFmt('<hr>Done Surface Check\n',[]).

% ===========================================================
% Invoke Prolog Command
% ===========================================================
parse_sigma_ua(Options):- %memberchk(interp='prolog',Options),!,
	memberchk(submit='Clean Slate',Options),!,halt.
	    
% ===========================================================
% Invoke Prolog Command
% ===========================================================
parse_sigma_ua(Options):- %memberchk(interp='prolog',Options),!,
	memberchk(cmd='Compile Demo',Options),!,autoload,
	(unsetSigmaOption(client=html)),qsave_program('sigma_server',[goal=(initializeSigmaServerData,cs)]),(setSigmaOption(client=html)).

build_exe_and_halt:- !,autoload,
	(unsetSigmaOption(client=html)),qsave_program('sigma_server',[goal=(initializeSigmaServerData,cs)]).
	
parse_sigma_ua(Options):-parse_sigma_enable(Options),!.



% ===========================================================
% Backup Sigma
% ===========================================================
parse_sigma_ua(Options):-memberchk(cmd='Save Image',Options),!,saveSigmaCache.
		                              
% ===========================================================
% Define New context
% ===========================================================
parse_sigma_ua(Options):-memberchk(client='command',Options),memberchk(submitbuttonUp='Create',Options),!, 
			ensureSigmaOption(newCtxName='ToplevelContext',Ctx),
			ensureSigmaOption(kb='Merge',KB),
			ensureSigmaOption(user='Web',User),
			idGen(TN2),
			assertaClean(sigmaCache(instance,surface,'instance'(Ctx,'Context'),'$VAR'(0),KB,'ToplevelContext',TN2,User,on)).
		
		    													 
% ===========================================================
% KB/Ctx ComboBoxs
% ===========================================================
show_available_contexts:-
	findall(Atom,
		(sigmaCache(PredR,surface,('instance'(Ctx,'Context'):_), KB,_, _, _, _),concat_atom([KB,':',Ctx],Atom)),
		List),
	writeq(List),nl.


getCtxListForKB(KB,['ToplevelContext'|Sorted]):-
	findall(Ctx,sigmaCache(_,_,_, _, KB,Ctx, _, _, _),UList),!,sort(UList,Sorted).
getCtxListForKB(KB,['ToplevelContext']):-!.
	
show_available_contexts_in_combobox(KB,null):- 
	getCtxListForKB(KB,List),
	toMarkUp(html,select(ctx,List),_,Out),write(Out).  
	
show_available_contexts_in_combobox(KB,Selected):- 
	getCtxListForKB(KB,List),
	toMarkUp(html,select(ctx,[Selected|List]),_,Out),write(Out).  
	    

getListOfKB(KBList):-
	findall(KB,sigmaCache(_,_,_, _, KB,Ctx, _, _, _),UList),!,sort(UList,KBList).

show_available_kbs_in_combobox(Out):-
		findall(KB,sigmaCache(PredR,surface,('instance'(KB,'KnowledgeBase'):_), 'SigmaKernel',_, _, _, _),List),
		toMarkUp(html,select(kb,List),_,Out).		

% =================================================
% Useragent Control Panel
% =================================================
parse_sigma_ua(Options):-memberchk(client='controlpanel',Options), 
	memberchk(kill=_,Options),!,
	member(kill=ID,Options),
	catch(thread_signal(ID,thread_exit(user_killed(ID))),_,true),
	parse_sigma_ua([client='controlpanel']).


parse_sigma_ua(Options):-memberchk(client='controlpanel',Options), !,
	statistics,
	writeFmt('<hr><table border=1 width=80%><th>Id</th><th>Name</th><th>Status</th><th>Actions</th><th>Options</th><th>Goals</th>',[]),
	showSigmaThreads,
	writeFmt('</table>',[]).

% ===========================================================
% TQ HOOK
% ===========================================================
parse_sigma_ua(Options):-memberchk(client='tqsystem',Options),!,ignore(parse_sigma_ua_tq(Options)).

% ===========================================================
% FALLBACKS
% ===========================================================

parse_sigma_ua(Options):-memberchk(client=command,Options),!.

parse_sigma_ua(Options):-%writeq(Options),write('<P>'),
	give_kif_window.

give_kif_window:-!. %Given

% ===========================================================
% EDITOR WINDOW
% ===========================================================

give_editor_window:-!,
	ensureSigmaOption(sf='(isa ?X ?Y)',Formula),
	writeFmt('<textarea rows=6 cols=90 name="sf">~w</textarea><br>',[Formula]),
	writeFmt('<br><INPUT type=submit name=submit value="Update Source"></INPUT><hr>',[]),
	writeFmt('<INPUT type=radio name="interp" value="kif" checked>KIF</INPUT>',[]),
	writeFmt('<INPUT type=radio name="interp" value="ace">ACE</INPUT>',[]).


%=================================================================
%  LEGACY BELOW
%=================================================================



%=================================================================
%  QUERY PROCESSING
%=================================================================

% Arity 1 (unused by Java)
ua_ask(QueryStringI):-!,ua_ask(QueryStringI,'ToplevelContext',[]).

% Arity 2 (unused by Java)
ua_ask(QueryStringI,Options):-!,ua_ask(QueryStringI,'ToplevelContext',Options).

% Arity 3 (used by Java)
ua_ask(QueryStringI,_Ctx,OptionList) :-
      setSigmaOption(OptionList),
      write_response_begin,!, %%trace,
     % elevate_status(kb(KB,_)=current),
      isSigmaOption(opt_kb=KB),
      once(ask(QueryStringI,_Ctx,KB)),
      write_response_end.

%=================================================================
%  ASSERT/RETRACT  PROCESSING
%=================================================================
/*
% Make assertions
% ua_tell(formulaObject(i),contextAtom(i),trackingAtom(i),OptionList(i))
% ua_tell("(mother Bill Jane)",sample_context,sigma-44-545,[disp_modification] ).
% ua_tell("(assertion(= > (and (mother ?CH ?M) (siblings ?CH ?CH2)) (mother ?CH2 ?M ))", sample_context,sigma-44-546,[disp_modification]).

% Make retractions

% ua_retract(assertionObject(i),OptionList(i))               TODO
% ua_retract("(assertion sample_context sigma-44-545 (mother Bill Jane))",[disp_modification] ).
% ua_retract("(assertion sample_context sigma-44-546 (=> (and (mother ?CH ?M) (siblings ?CH ?CH2)) (mother ?M ?CH2)))",[disp_modification] ). 
*/

%=================================================================
%  ASSERT  PROCESSING
%=================================================================

% Arity 1 (unused by Java)

ua_tell(Assertion_Chars):-!,ua_tell(Assertion_Chars,'ToplevelContext',_,[]).

% ua_tell(formulaObject(i),contextAtom(i),trackingAtom(i),OptionList(i))
ua_tell(Assertion_Chars,_Ctx,TrackingAtom,OptionList):-
      (((idGen(TN),TrackingAtom=(TN)) ; true)),
      setSigmaOption(OptionList),
      ignore(member(opt_kb=KB,OptionList)),
      write_response_begin,!, %%trace,
      ignore(once(tell(Assertion_Chars,_Ctx,TrackingAtom,KB,CMP))),
      write_response_end.

%=================================================================
% RETRACT  PROCESSING
%=================================================================

% Arity 1 (unused by Java)
ua_retract(Retraction_Chars):-!,ua_retract(Retraction_Chars,'ToplevelContext',_,[]).

% ua_retract(formulaObject(i),contextAtom(i),trackingAtom(i),OptionList(i))
ua_retract(Retraction_Chars,_Ctx,TrackingAtom,OptionList):-
      (((idGen(TN),TrackingAtom=(TN)) ; true)),
      setSigmaOption(OptionList),
      ignore(member(opt_kb=KB,OptionList)),
      write_response_begin,!, %%trace,
      ignore(once(retract(Retraction_Chars,_Ctx,TrackingAtom,KB,CMP))),
      write_response_end.

%=================================================================
%  COMMAND PROCESSING
%=================================================================

% Arity 1 (unused by Java)
ua_command(CommandStringI):-ua_command(CommandStringI,[]).

% ua_command(CommandString(i),OptionList(i))
ua_command(CommandStringI,OptionList):-
      write_response_begin,
         setSigmaOption(OptionList),
         ask_parse_chars(CommandStringI,CMD,Vars),
      write_response_end.
         
ua_command_invoke(nil,_):-!.
ua_command_invoke(CMD,Vars):-
   sendNote(debug,kernel,'Making Prolog Call',writeq(CMD)),
            ua_out(query_start,ask_prolog(PrologKR),Vars),!,
            getCputime(Start),
            sigma_inference(CMD,Vars),
            getCputime(Now),
            Elapsed is Now - Start,
            ua_out(query_end,('true',CMD,Elapsed,0,0),Vars).


% ===================================================================================================
%  End of sigma_useragent.pl
% ===================================================================================================

getKbStatus('SigmaKernel'):-writeFmt('Browse Only\n',[]),!.
getKbStatus(KB):-
	isKBCompilerThread(KB,Progress),
	writeFmt('<A href=controlpanel.jsp?kb=~w><font color=green>Canonicalizing</font></a>\n',[KB]),!.

getKbStatus(KB):-
	isUncanonicalized(KB),
	invokeKBCompilerThread(KB),
	isKBCompilerThread(KB,Progress),
	writeFmt('<A href=controlpanel.jsp?kb=~w><font color=green>Canonicalizing</font></a>\n',[KB]),!.
	
getKbStatus(KB):-
	isUncanonicalized(KB),
	isKBCurrentlyInUse(OtherKB,Status),!,
	writeFmt('<font color=orange>Waiting for ~w</font>\n',[OtherKB]).
	
getKbStatus(KB):-
	isKBCurrentlyInUse(KB,Status),
	writeFmt('<font color=orange>Busy ~w</font>\n',[Status]).

getKbStatus(KB):- 
	isKbUntransfered(KB),
	writeFmt('<A href=skb_to_prolog.jsp?kb=~w><font color=red>Update</font></a>\n',[KB]),!.

getKbStatus(KB):- 
	isSourceNewerThenImage(KB),
	writeFmt('<A href=skb_to_prolog.jsp?kb=~w><font color=red>Needs Update</font></a>\n',[KB]),!.
	
getKbStatus(KB):-isKnowledgeBaseLoaded(KB,_),writeFmt('<A href="ask_tell.jsp?kb=~w">Ask/Tell</A>\n',[KB]),!.

getKbStatus(KB):-writeFmt('Unknown\n',[]),!.

