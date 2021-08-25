
:-include('sigma_header.pl').

% ===========================================================
% Context From File 
% ===========================================================
ctxFromFile(BadCtxName,Filename,KB):-
		catch(atom_codes(BadCtxName,Codes),_,Codes=[]),
		length(Codes,L),L<3,
		file_base_name(Filename,BaseCtxName),
		file_name_extension(CtxName,Extension,BaseCtxName),
		writeFmt('<B color=red>No name was given, so a Context called <font color=green>~w</font> is being created.<p>',[CtxName]),
		load_kif_to_kb_ctx(CtxName,Filename,'ToplevelContext','SigmaWeb').

ctxFromFile(CtxName,Filename,KB):-!,
		idGen(TN1),
		idGen(TN3),
		assertaClean(sigmaCache(PredR,surface,'instance'(CtxName,'Context'),'$VAR'(0),KB,'ToplevelContext',TN1,'WebUser',on)),
		assertaClean(sigmaCache(PredR,surface,'sourcefile'(CtxName,Filename),'$VAR'(0),KB,'ToplevelContext',TN3,'WebUser',on)),
		load_kif_to_kb_ctx(KB,Filename,CtxName,'SigmaWeb').
		




load_kif_to_kb_ctx(KB,FileName,Ctx,User):-!,
	ignore(User='SigmaWeb'),
	assert(telling_file),	 
%%         atom_concat(FileName,'.compiled.pl',DBG),
%%         tell(DBG),
	 get_default_assertion_context(DCtx), !,ignore((Ctx=DCtx)),!,  
         getDefaultKB(DKB), !,  ignore((KB=DKB)),!,
         
	 writeFmt('Reading In ~w to ~w with a default context of ~w <p>',[FileName,KB,Ctx]),nl,
	 flag('Axioms Compiled',_,0),
         safe_file_open(FileName,'r',INPUT),
	 repeat,   %trace,
		load_kif_to_kb_ctx_display(KB,Ctx,User,INPUT),
		close(INPUT),
		flag('Axioms Compiled',AX,AX),
         writeFmt('\n% Compiled ~w axioms.\n',[AX]),
         ignore(retract(findings(CPU,RESULT))) ,
         ignore(findall(T,retract(title(T)),Title)),saveSigmaCache.

	 

ado_to_prolog(FileName):-
	tell(FileName),
	sigmaCache(Pred, Head, Type, Logic,KB, Ctx, Proof),
	w_ado_cache(Pred, Head, Type, Logic,KB, Ctx, Proof),
	fail.

ado_to_prolog(FileName):-
	sigmaCache(Pred, Head, Cond,Type, Logic,KB, Ctx, Proof),
	w_ado_cache(Pred, Head, Cond,Type, Logic,KB, Ctx, Proof),
	fail.
ado_to_prolog(FileName):-told.

atom_to_prolog(FileName):-
	tell(FileName),
	atom_to_prolog(FileName),
	told.
	

atom_to_prolog:-
	sigmaCache(Pred, Head, Type, Logic,KB, Ctx, Proof),
	a_ado_cache(Pred, Head, Type, Logic,KB, Ctx, Proof),
	fail.

atom_to_prolog:-
	sigmaCache(Pred, Head, Cond,Type, Logic,KB, Ctx, Proof),
	a_ado_cache(Pred, entails(Head, Cond),Type, Logic,KB, Ctx, Proof),
	fail.

atom_to_prolog:-
	sigmaCache(atom(A)),
	format(' ~q ; ',[A]),fail.

atom_to_prolog.

a_ado_cache(argOf, Head, Type, true,KB, Ctx, Proof):-!.
a_ado_cache(documentation, Head, Type, true,KB, Ctx, Proof):-!.
a_ado_cache(Pred, FOO,Type, Logic,KB, Ctx, Proof):-
	getConstants(atomic,FOO,List,_,_),
	assert_list(List).

assert_list([]):-!.
assert_list([A|L]):-!,
	assert_list_n(A),
	assert_list(L).
	
	
assert_list_n(A):-sigmaCache(atom(A)),!.
assert_list_n([]):-!.
assert_list_n('.'):-!.
assert_list_n(N):-number(N),!.
assert_list_n(A):-
	asserta(sigmaCache(atom(A))).
	



	
w_ado_cache(argOf, Head, Type, true,KB, Ctx, Proof):-!.
w_ado_cache(documentation, Head, Type, true,KB, Ctx, Proof):-!.

w_ado_cache(Pred, Head, Type, true,KB, Ctx, Proof):-
	format('~q.~n',[Head]),!.
w_ado_cache(Pred, Head, Type, false,KB, Ctx, Proof):-
	format('not_~q.~n',[Head]),!.
w_ado_cache(Pred, Head, Pre, Type, true,KB, Ctx, Proof):-
	pre_to_b(Pre,B),
	format('~q:-~q.~n',[Head,B]),!.
w_ado_cache(Pred, Head, Pre, Type, false,KB, Ctx, Proof):-
	pre_to_b(Pre,B),
	format('not_~q:-~q.~n',[Head,B]),!.
	
pre_to_b(B,var(B)):-isSlot(B),!.
pre_to_b(and(A,B),(AA,BB)):-!,pre_to_b(A,AA),pre_to_b(B,BB).
pre_to_b(not(B),BBO):-pre_to_b(B,BB),BB=..[F|A],atom_concat('not_',F,NF),BBO=..[NF|A].
pre_to_b(B,B).


load_kif_to_kb_ctx_display(KB,Ctx,User,Stream):-at_end_of_stream(Stream),!.
	 
load_kif_to_kb_ctx_display(KB,Ctx,User,Stream):- 
		      once((
				source_from_stream(Stream,_,SOURCEFORM,Vars),
				catch(
						(       
									flag('Axioms Compiled',X,X+1),
									invokeTell([trusted,nocanonicalize],surface,SOURCEFORM,Ctx,TN,KB,Vars,User)
						),

					E,
					(line_count(Stream,Line),writeFmt('\nLine ~w  Uninterpretable "~q" (~q)\n',[Line,KIFSTERM,E]))
				)
		      )),
		      fail.


% ===================================================================
%  Read Knowledge Representation File
% ===================================================================

% Example 
lmerge:-tell_from_kif(forall,'c:/sigmaL/SUO/Merge.can','Merge','ToplevelContext',Author).

kif_file(File):-tell_from_kif(forall,File,File,'ToplevelContext',Author).

tell_from_kif(SourceFile):-!,tell_from_kif(forall,SourceFile,KB_Name,Ctx,SourceFile).

tell_from_kif(SourceFile,KB_Name):-!,tell_from_kif(forall,SourceFile,KB_Name,Ctx,SourceFile).

tell_from_kif(Driver,SourceFile,KB_Name,Ctx,Author):-
            sendNote(debug,kifParser,['Loading Sigma KIF/CAN file into ',Driver,' as',KB_Name,Ctx,from,SourceFile],' '),
            ignore(safe_kb_info_db(KB_Name,SourceFile,WFSFile,_)),
            ignore(Author=SourceFile),
            safe_file_open(SourceFile,'r',INPUT),
            repeat,     
               once((
                     once(readKIF(INPUT,CHARS)),
                     once(invokeTell(Driver,chars,CHARS,Ctx,TN,KB_Name,Vars,Author))
                     )),
            at_end_of_stream(INPUT),!,
            file_close(INPUT),
            garbage_collect,
            garbage_collect_atoms.

% ===================================================================
%  Read KIF File as Prolog
% ===================================================================


load_kif_as_prolog(SourceFile):-  
         add_file_extension(".pl",SourceFile,PLocation),
         file_newer(PLocation,SourceFile),!,
         sendNote(debug,kifParser,'Loading kif as if prolog.',[PLocation,from,SourceFile]),
         consult_as_dynamic(PLocation).
  

t6:-ua_tell("(pnx_nf (domain AssignmentFn 1 Function) ToplevelContext T-536)").


load_kif_as_prolog(SourceFile):- !, 
         add_file_extension(".pl",SourceFile,PLocation),!,
         safe_file_open(SourceFile,'r',INPUT),
         safe_file_open(PLocation,'w',OUTPUT),       
         sendNote(debug,kifParser,'Loading kif as if prolog.',[converting,SourceFile,to,PLocation]),
           repeat,                  
               once((once(readKIF(INPUT,Chars)),
                    %catch(writeFmt(OUTPUT,'\n% ~s  ',[Chars]),_,true),
                    tell_retract_parse_chars(Chars,PROLOG,_),
                     once((
                           PROLOG=comment(_)
                            ;
                          writeFmt(OUTPUT,'\n~q.\n',[PROLOG])
                            )))),
                     at_end_of_stream(INPUT), !,
            sendNote(debug,kifParser,'File Loaded',PLocation),
            file_close(OUTPUT),
            file_close(INPUT),
            consult_as_dynamic(PLocation).

% ===================================================================
%  Export Surface Forms as KIF (From a KB and Context)
% ===================================================================


export_kif_from_source(KB,DestFile):-  
         export_kif_from_source(KB,Ctx,DestFile,full_cmt).

export_kif_from_source(KB,Ctx,DestFile):-  
         export_kif_from_source(KB,Ctx,DestFile,full_cmt).

export_kif_from_source(KB,Ctx,DestFile,Fmt):-  
         add_file_extension(".kif",DestFile,PLocation),
         safe_file_open(PLocation,'w',OUTPUT),       
         sendNote(debug,kifParser,'Saving kif from Prolog.',[saving,to,PLocation]),
         export_kif_from_source_0(Fmt,KB,Ctx,OUTPUT).

export_kif_from_source_0(Format,KB,Ctx,OUTPUT):-
         get_store(forall,surface,(Surface:Vars),KB,Ctx,TN,Author),
         unnumbervars((Surface:Vars),(USurface:UVars)),
         toMarkUp(kif,USurface,UVars,Chars),
         ( Format=full_cmt -> writeFmt(OUTPUT,'\n; KB:~q  Ctx:~q  TN:~q  Auth:~q\n~s\n',[KB,Ctx,TN,Author,Chars]);
         ( Format=terse -> writeFmt(OUTPUT,'\n~s\n',[Chars]);
         ( Format=pnx_nf -> writeFmt(OUTPUT,'\n( pnx_nf ~s ~w ''~w'' )\n',[Chars,Ctx,TN])))),
         fail.

export_kif_from_source_0(Format,KB,Ctx,OUTPUT):-!.

% ===================================================================


agent_load_kif_quiet(Filename,ToplevelContext,User):-
	agent_load_kif_surface(Filename,KB,Ctx,User,AX,quiet).

agent_load_kif(Filename,Ctx,User):-
	agent_load_kif_surface(Filename,KB,Ctx,User,AX,loud).

	
agent_load_kif_surface(Filename,KB,Ctx,User,AX,Verbose):-
	ignore(User='Automation'),
	(unsetSigmaOption(opt_surface_check=_)),
	(setSigmaOption(opt_surface_check=trusted)),
	 get_default_assertion_context(DCtx), !,ignore((Ctx=DCtx)),!,  
         getDefaultKB(DKB), !,  ignore((KB=DKB)),!,
	idGen(TN1),
	idGen(TN2),
	idGen(TN3),
	assertaClean(sigmaCache(PredR,surface,'instance'(KB,'KnowledgeBase'),'$VAR'(0),'SigmaKernel','ToplevelContext',TN1,User,gaf)),
	assertaClean(sigmaCache(PredR,surface,'instance'('ToplevelContext','Context'),'$VAR'(0),KB,'ToplevelContext',TN2,User,gaf)),
	assertaClean(sigmaCache(PredR,surface,'sourcefile-of'(KB,Filename),'$VAR'(0),'SigmaKernel','ToplevelContext',TN3,User,gaf)),
	writeFmt(user_error,'% Reading In ~w to ~w with a default context of ~w \n',[Filename,KB,Ctx]),
	flag('Axioms Compiled',_,0),
        safe_file_open(Filename,'r',INPUT),!,
           repeat,                  
               once((once(readKIF(INPUT,Chars)),
		  % say(Chars),
                    %catch(writeFmt(OUTPUT,'\n% ~s  ',[Chars]),_,true),
                    tell_retract_parse_chars(Chars,PROLOG,Vars),
                     once((
                           PROLOG=comment(_)
                            ;
                          
			  remember_ado(PROLOG,Vars,KB,Ctx,User,Verbose)
                            )))),
                     at_end_of_stream(INPUT), !,
	close(INPUT),
	flag('Axioms Compiled',AX,AX),
	writeFmt(user_error,'% Loaded ~w axioms.\n',[AX]),
	ignore(retract(findings(CPU,RESULT))),
	ignore(findall(T,retract(title(T)),Title)),!.

remember_ado(Surface,Vars,KB,Ctx,User,quiet):-!,
	flag('Axioms Compiled',AX,AX+1),
	once(invokeTell([untrusted,canonicalize],surface,Surface,Ctx,TN,KB,Vars,User)).
	
remember_ado(Surface,Vars,KB,Ctx,User,_):-!,
	flag('Axioms Compiled',AX,AX+1),
	once(invokeTell([untrusted,canonicalize],surface,Surface,Ctx,TN,KB,Vars,User)),
	ignore((writeObject(formula(Surface),Vars))),nl.
		
agent_u_save(FileName,Ctx):-
	tell(FileName),
%	writeFmt((':-include(\'sigma_header.pl\').\n'),[]),
	writeFmt(':-multifile(sigmaCache/6).\n',[]),
	writeFmt(':-multifile(sigmaCache/9).\n',[]),
	writeFmt(':-multifile(sigmaCache/5).\n',[]),
	writeFmt(':-retractAllProlog(sigmaCache(PredR,_,_,_,_,~q,_,_,_)).\n',[Ctx]),	
	writeFmt(':-was_indexed(sigmaCache(1 ,0,1,0,0,1,0,0,1)).\n',[]),
	save_each_assertion_of(Ctx),
	told.

	
save_each_assertion_of(Ctx):-sigmaCache(PredR,Form,Surface,Vars,KB,Ctx,EXTID,User,Status),
		writeFmt('~q.\n',[sigmaCache(PredR,Form,Surface,Vars,KB,Ctx,EXTID,User,Status)]),fail.
save_each_assertion_of(Ctx):-!.		

agent_u_load(Filename):-ensure_loaded(Filename).

