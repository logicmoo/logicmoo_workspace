
:-include('sigma_header.pl').

invokeKbLoading(KB):-isKbLoading(KB,Status),!.
invokeKbLoading(KB):-
	sigmaThreadCreate(loadKBfromSource(KB,ToplevelContext),loadKBfromSource(KB,ToplevelContext),ID,[detatched(false)]),
	assert(isSigmaThread(ID,loadKBfromSource(KB,ToplevelContext))).

loadKBfromSource(KB,Ctx):-
		getFilenameOfKBSource(KB,Filename),!,
		once(invokeKifFileChecking(KB,Filename,Ctx,User)).

getFilenameOfKBSource(KB,Filename):-
		fmtString(FileChars,'../sigma-rt/work/~w.can',[KB]),!,
		string_to_atom(FileChars,Filename),!.

		

% ===========================================================
% Test Knowledge Base File (Currently only Test until browser integration)
% ===========================================================
loadKnowledgebaseSourcefile(Name,Filename):-  make,
		catch(atom_codes(Name,Codes),_,Codes=[]),
		length(Codes,L),L<3,
		file_base_name(Filename,BaseName),
		file_name_extension(KB,Extension,BaseName),
		writeFmt('<B color=red>No name was given, so a Knowledge Base called <font color=green>~w</font> is being created.<p>',[KB]),
		load_kif_to_kb_ctx(KB,Filename,'ToplevelContext','SigmaWeb').

loadKnowledgebaseSourcefile(KB,Filename):-!,
		(unsetSigmaOption(opt_surface_check=_)),
		(setSigmaOption(opt_surface_check=trusted)),
		idGen(TN1),
		idGen(TN2),
		idGen(TN3),
		retractall(sigmaCache(PredR,_,_,KB,_,_,_,_)),
		assertaClean(sigmaCache(PredR,surface,'instance'(KB,'KnowledgeBase'),'$VAR'(0),'SigmaKernel','ToplevelContext',TN1,'WebUser',gaf)),
		assertaClean(sigmaCache(PredR,surface,'instance'('ToplevelContext','Context'),'$VAR'(0),KB,'ToplevelContext',TN2,'WebUser',gaf)),
		assertaClean(sigmaCache(PredR,surface,'sourcefile-of'(KB,Filename),'$VAR'(0),'SigmaKernel','ToplevelContext',TN3,'WebUser',gaf)),
		invokeKifFileChecking(KB,Filename,'ToplevelContext','SigmaWeb').



invokeKifFileChecking(KB,Filename,Ctx,User):-!, make,tell(user_error),
	ignore(User='SigmaWeb'),
	 get_default_assertion_context(DCtx), ignore((Ctx=DCtx)),
         getDefaultKB(DKB), !,  ignore((KB=DKB)),
	 retractall(sigmaCache(PredR,_,_,_,KB,Ctx,_,_,_)),!,	
	 saveSigmaCache,
	 writeFmt(user_error,'Reading In ~w to ~w with a default context of ~w \n',[Filename,KB,Ctx]),
	 flag('Axioms Compiled',_,0),
         safe_file_open(Filename,'r',INPUT),!,
         writeFmt(user_error,'~q\n',[safe_file_open(Filename,'r',INPUT)]),
	 repeat, 
		  compile_each_line(INPUT),
	at_end_of_stream(INPUT),
	close(INPUT),
	flag('Axioms Compiled',AX,AX),
         writeFmt('\n% Compiled ~w axioms.\n',[AX]),
	 %(test_syntax_save_kb_ctx(KB,Ctx,Filename)),
         ignore(retract(findings(CPU,RESULT))) ,
         ignore(findall(T,retract(title(T)),Title)).
	 
compile_each_line(Stream):-
		sleep(1),
		once(readKIF_priv(Stream,RRAW)),
		once(getCleanCharsWhitespaceProper(RRAW,Trimed)),
		once(ltrim(Trimed,L_trimmed)),
			catch(writeFmt(user_error,'"~s" \n',[L_trimmed]),_,true).

/*
		        source_from_stream(INPUT,Trimed,SOURCEFORM,Vars),nonvar(Trimed),
			catch(writeFmt(user_error,'~s \n',[Trimed]),_,true),
			rememberAxioms(KB,Ctx,SOURCEFORM,Vars,User),!.
			
*/






%tkb:-loadKBfromSource('Merge','ToplevelContext').
/*
skipKIFChar(Stream):-stream_property(Stream,position('$stream_position'(CharIndex, LineNo, LinePos)),
	NewI is CharIndex+1,
*/
/*
(pnf (documentation instance "An object is an &%instance a &%Class if 

it is a member of that &%Class.  An individual may be an instance of many 

classes, some of which may be subclasses of others.  Thus, there is no 

assumption in the meaning of &%instance about specificity or uniqueness.")
 ToplevelContext T-4)
(pnf (instance subclass PartialOrderingRelation)
 ToplevelContext T-5)

*/


	 
		      
rememberAxioms(KB,Ctx,file_comment(_),Vars,User):-!.
rememberAxioms(KB,Ctx,surf,Vars,User):-!.
rememberAxioms(KB,Ctx,SOURCEFORM,Vars,User):-!,
		global_increment('Axioms Compiled'),
		flag('Axioms Compiled',AssertionID,AssertionID),
		idGen(INTID),
		assert(sigmaCache(PredR,surface,SOURCEFORM,Vars,KB,Ctx,AssertionID,User,uncanonicalized)).
		
test_syntax_save_kb_ctx(KnowledgeBase,Context,Filename):-
         safe_file_open(Filename,'w',OUTPUT),
	 test_syntax_write_kb_ctx(KnowledgeBase,Context,OUTPUT),
	 close(OUTPUT).
	 
test_syntax_save_kb_ctx:-test_syntax_save_kb_ctx('Merge','ToplevelContext','C:/sigmal/SUO/MFixed.txt').

	
test_syntax_write_kb_ctx(KnowledgeBase,Context,OUTPUT):-
		sigmaCache(PredR,surface,Source,Vars,KnowledgeBase,Context,AssertionID,Creator,Status),
		once(catch((toMarkUp(kif,Source,Vars,OutChars),writeFmt(OUTPUT,'~w\n',[OutChars])),_,true)),
		fail.

test_syntax_write_kb_ctx(KnowledgeBase,Context,OUTPUT):-!,saveSigmaCache.


