:-include('sigma_header.pl').


% ==============================================
% Enable or Disable Prolog Memory
% ==============================================

% Assertions have these states

% Surface (on/disabled)
% Can #1 (on/disabled) (in_mem/out_mem)
% Can #2 (on/disabled) (in_mem/out_mem)

% A user makes an assertion into an inactive KB
% Surface (on)
% Can #1 (on) (out_mem)
% Can #2 (disabled) (out_mem)

% A user makes an assertion into an active KB
% Surface (on)
% Can #1 (on) (in_mem)
% Can #2 (disabled) (out_mem)

/*

So ... this means we have Three States of any Canonical assertion...  on/in_mem/disabled

*/


% ===========================================================
% Search For Constant (Atom)
% ===========================================================
parse_sigma_enable(Options):-memberchk(show='find',Options),!,
	getSigmaOption(word='instance',Word),
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	disp_word_to_surface(Word,KB,Ctx,Out),
	draw_update_enable(KB,Out).

disp_word_to_surface(Word,KB,_Ctx,Out):-
	retractAllProlog(pkids(_)),
	sigmaCache(PredR,SurfaceTODO,SURF,PROLOG,KB,Ctx,AID,Author,OnOff),
	getConstants(atomic,SURF,Atoms,_,_),memberchk(Word,Atoms),  
	assertion_display(SurfaceTODO,SURF,PROLOG,KB,Ctx,AID,Author,OnOff),
	fail.	

disp_word_to_surface(Word,KB,Ctx,Out):-!,setof(retract(pkids(INTID)),Out).

% ===========================================================
% Search For Tracking Number
% ===========================================================
parse_sigma_enable(Options):-(memberchk(submit=editaid,Options);memberchk(dispasid=dispasid,Options)),!,  
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	retractAllProlog(pkids(_)),!,
	disp_tn_to_surface(KB,AID,Out),
	draw_update_enable(KB,Out),!.
	
disp_tn_to_surface(KB,bogus,Out):-!.

disp_tn_to_surface(KB,TN,Out):-
	sigmaCache(PredR,Format,SURF,Prolog,KB,Ctx,TN,Author,OnOff), 
	assertion_display(Format,SURF,Prolog,KB,Ctx,TN,Author,OnOff),
	fail.	

disp_tn_to_surface(KB,Out):- %isSigmaOption(disp_notes_nonuser=on),
	sigmaCache(PredR,Format,SURF,Prolog,KB,Ctx,TN,Author,OnOff),  
	assertion_display(Format,SURF,Prolog,KB,Ctx,TN,Author,OnOff),
	fail.	

disp_tn_to_surface(KB,_,Out):-!,setof(retract(pkids(INTID)),Out).

aid_to_number(Number,Number):-number(Number),!.
aid_to_number(AID,Number):- atom_to_term(AID,Number,_),!.


% ===========================================================
% Search For State (Atom) (rejected,on,gaf).. etc
% ===========================================================
parse_sigma_enable(Options):-memberchk(show='state',Options),!,
	getSigmaOption(word='disabled',Word),
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	disp_word_to_surface(Word,KB,Ctx,Out),
	draw_update_enable(KB,Out).

disp_status_to_surface(Status,KB,Ctx,Out):-
	retractAllProlog(pkids(_)),
	sigmaCache(PredR,SurfaceTODO,SURF,Prolog,KB,Ctx,AID,Author,Status),
	assertion_display(SurfaceTODO,SURF,Prolog,KB,Ctx,AID,Author,Status),
	fail.	

disp_status_to_surface(Status,KB,Ctx,Out):-!,setof(retract(pkids(INTID)),Out).

% ===========================================================
% Search Disabled Assertion
% ===========================================================
parse_sigma_enable(Options):-
	memberchk(cmd='Show Disabled',Options),!,
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	writeFmt('<H3><Font Color=Red>Listing Disabled Assertions...</Font></H3>',[]),!,
	show_disabled_assertions(KB,Ctx),!.
	
show_disabled_assertions(KB,Ctx):-
	sigmaCache(PredR,Form,Formula,Prolog,KB,Ctx,AID,Author,OnOff),not(OnOff=on),not(OnOff=uncanonicalized),
	assertion_display(Form,Formula,Prolog,KB,Ctx,AID,Author,OnOff),
	fail.
	
show_disabled_assertions(KB,Ctx):-writeFmt('<H3><Font Color=Red>Done.</Font></H3>',[]).




% ===========================================================
% Show Assertion Updater
% ===========================================================
draw_update_enable(KB,[]):- writeFmt('Search yielded no Results.\n',[]).
draw_update_enable(KB,As):-!,
	format_o(
		'<HR>~w&nbsp;<INPUT type=submit name=submit value="Change"/>',select(ue,['Enable','Disable','Delete'])),!,
       %show_available_contexts_in_combobox(destination,[],Out),!,writeFmt('<BR><input type=radio name=CopyOrTransfer checked value=Transfer><B>Transfer</B></input>&nbsp;<input type=radio name=CopyOrTransfer value=Copy><B>Copy</B></input> selected assertions to ~w <input type=submit name=move value=Submit>',[Out]),!.
       	writeFmt('&nbsp;&nbsp;&nbsp;&nbsp;<A href="ask_tell.jsp?kb=~w">Return to Ask/Tell</A>',[KB]).
		

% ===========================================================
% Show Assertions (Surface)
% ===========================================================

assertion_display(surface,SURF,Vars,KB,Ctx,AID,Author,OnOff):- !,
	assert(pkids(INTID)),
	toMarkUp(html,SURF,Vars,SAtom),
	%toMarkUp(kif,SURF,Vars,KIF),
	%setSigmaOption(sf=KIF),
	on_to_check(on,OnOff,Checked),
	writeFmt('<hr><A HREF="ask_tell.jsp?kb=~w&asid=~w&t=ea" title="Edit Assertion"><IMG border=0 src="pixmaps/cyan.gif" asrtid=~w></A><input type=checkbox class=assertionChecks  name=~w ~w/><nobr>',[KB,AID,AID,Checked]),
	writeFmt('<b>Surface</b> ID<font color=red>~w:~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font> Status: <font color=puple>~w</font>',[AID,KB,Ctx,Author,OnOff]),
	writeFmt('~w',[SAtom]),!.
	
% ===========================================================
% Show Assertions (HL)
% ===========================================================

assertion_display(surface,WFS,Vars,KB,Ctx,AID,Author,OnOff):- %isSigmaOption(disp_notes_nonuser=on),
	assert(pkids(INTID)),
	toMarkUp(html,formula(WFS),Vars,SAtom),
	%toMarkUp(kif,SURF,Vars,KIF),
	%setSigmaOption(sf=KIF),
	on_to_check(on,OnOff,Checked),
	writeFmt('<hr><A HREF="ask_tell.jsp?kb=~w&submit=editaid&asid=~w&t=ea" title="Show Source"><IMG border=0 src="pixmaps/purple.gif" asrtid=~w></A><input type=checkbox class=assertionChecks  name=~w ~w/><nobr>',[KB,AID,AID,Checked]),
	writeFmt('<b>~w</b> ID<font color=red>~w:~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font> Status: <font color=puple>~w</font>',['Surface',AID,KB,Ctx,Author,OnOff]),
	writeFmt('~w',[SAtom]),!.
/*
assertion_display(skolem,WFS,Vars,KB,Ctx,AID,Author,OnOff):- %isSigmaOption(disp_notes_nonuser=on),
	assert(pkids(INTID)),
	toMarkUp(html,formula(WFS),Vars,SAtom),
	%toMarkUp(kif,SURF,Vars,KIF),
	%setSigmaOption(sf=KIF),
	on_to_check(on,OnOff,Checked),
	writeFmt('<hr><A HREF="ask_tell.jsp?kb=~w&submit=editaid&asid=~w&t=ea" title="Show Source"><IMG border=0 src="pixmaps/purple.gif" asrtid=~w></A><input type=checkbox class=assertionChecks  name=~w ~w/><nobr>',[KB,AID,AID,Checked]),
	writeFmt('<b>~w</b> ID<font color=red>~w:~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>   Status: <font color=puple>~w</font>',['Skolem',AID,KB,Ctx,OnOff]),
	writeFmt('~w',[SAtom]),!.
*/
assertion_display(Head,Tail,Vars,KB,Ctx,AID,Author,OnOff):- %isSigmaOption(disp_notes_nonuser=on),
	assert(pkids(INTID)),
	toMarkUp(html,formula((Head:-Tail)),Vars,SAtom),
	%toMarkUp(kif,SURF,Vars,KIF),
	%setSigmaOption(sf=KIF),
	on_to_check(on,OnOff,Checked),
	writeFmt('<hr><A HREF="ask_tell.jsp?kb=~w&submit=editaid&asid=~w&t=ea" title="Show Source"><IMG border=0 src="pixmaps/purple.gif" asrtid=~w></A><input type=checkbox class=assertionChecks  name=~w ~w/><nobr>',[KB,AID,AID,Checked]),
	writeFmt('<b>~w</b> ID<font color=red>~w:~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Status: <font color=puple>~w</font>',['Heuristic',AID,KB,Ctx,OnOff]),
	writeFmt('~w',[SAtom]),!.


on_to_check(OnOff,OnOff,'Checked').
on_to_check(_,_,' ').






% ===============================================
% Read Checkboxes and Make Calls
% ===============================================


% Disables
parse_sigma_enable(Options):-     
		memberchk(submit='Change',Options),
		memberchk(ue='Disable',Options),  %trace,
		findall(N,(member(N=on,Options),number(N)),List),
		do_each_disable(List).

	do_each_disable([]):-!.
	do_each_disable([H|T]):-!,
		do_each_disable_id(H),
		do_each_disable(T),!.
	
	do_each_disable_id(ID):-sigmaCache(PredR,ID,surface,_,_,_,_,SurfNumber,_,_),!,disable_surf_number(SurfNumber).
	do_each_disable_id(ID):-sigmaCache(PredR,ID,wfs,_,_,_,_,_,_,_),!,disable_can_number(ID).


% Enables
parse_sigma_enable(Options):-   
		memberchk(submit='Change',Options),
		memberchk(ue='Enable',Options),   %trace,
		findall(N,(member(N=on,Options),number(N)),List),
		do_each_enable(List).


	do_each_enable([]):-!.
	do_each_enable([H|T]):-!,
		do_each_enable_id(H),
		do_each_enable(T),!.
	
	do_each_enable_id(ID):-sigmaCache(PredR,ID,surface,_,_,_,_,SurfNumber,_,_),!,enable_surf_number(SurfNumber).
	do_each_enable_id(ID):-sigmaCache(PredR,ID,wfs,_,_,_,_,_,_,_),!,enable_can_number(ID).
	do_each_enable_id(ID):-!.

% ==============================================
% Enable + Canonizlize an Existing Surface Number
% ==============================================
enable_surf_number(SurfNumber):-
	 (sigmaCache(PredR,surface,Surf,Vars,KB,Ctx,SurfNumber,Author,OnOff)),!,
	 retractAllProlog(sigmaCache(PredR,wfs,_,_,KB,Ctx,SurfNumber,_,_)),
	 logOnFailure(sigma_invoke_accept_surface(tell,[canonicalize,untrusted],surface,Surf,Ctx,SurfNumber,KB,Vars,Author,accept('Previous Assertion being Recanonicalized'))).


% ==============================================
% Disable and De-Canonizlize an Existing Surface Number
% ==============================================
disable_surf_number(SurfNumber):-
	 retract(sigmaCache(PredR,N,surface,Surface,Vars,KB,Ctx,SurfNumber,Author,OnOff)),!,
	 assert(sigmaCache(PredR,N,surface,Surface,Vars,KB,Ctx,SurfNumber,Author,disabled)),!,
	 disable_each_can_with_surf_id(SurfNumber).
	 
disable_each_can_with_surf_id(SurfNumber):-
	 sigmaCache(PredR,CanNumber,wfs,UCL,PROLOG,KB,Ctx,SurfNumber,Author,_),
	 once(disable_can_number(CanNumber)),fail.
disable_each_can_with_surf_id(SurfNumber):-!.

% ==============================================
% Enable or CAN ID and reflect in Prolog Memory
% ==============================================

enable_can_number(CanNumber):-
	 sigmaCache(PredR,CanNumber,wfs,UCL,PROLOG,KB,Ctx,TN,Author,on_mem).	 
	 
enable_can_number(CanNumber):-
	 sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on),
	 isKnowledgeBaseLoaded(KB,Ctx),!,
	 enable_can_conj(PrologFormS).

enable_can_number(CanNumber):-
	 (sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,Off)),
	 isKnowledgeBaseLoaded(KB,Ctx),!,
	 retract(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,Off)),
	 assert(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on_mem)),
	 enable_can_conj(PrologFormS).

enable_can_number(CanNumber):-
	 sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on),
	 not(isKnowledgeBaseLoaded(KB,Ctx)),!.
	 
enable_can_number(CanNumber):-
	 retract(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,_)),
	 not(isKnowledgeBaseLoaded(KB,Ctx)),!,
	 assert(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on)).


enable_can_number(CanNumber):-
	 sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on_mem),
	 not(isKnowledgeBaseLoaded(KB,Ctx)),!. %TODO Problem
	 
enable_can_number(CanNumber):-!.

enable_can_conj(true):-!.
enable_can_conj((Prolog,Form)):-!,
	enable_can_conj(Prolog),
	enable_can_conj(Form),!.
enable_can_conj(PrologForm):-
	enable_can(PrologForm).

enable_can(PrologFormS):-
	logOnFailure(assert(PrologFormS,AssertID)).

	 
% ==============================================
% Disable CAN ID and reflect in Prolog Memory
% ==============================================

disable_can_number(CanNumber):-
	 retract(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,on)),
	 assert(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,disabled)),
	 sendNote(user,contentMananger,'Details of Disable',sigmaCache(PredR,CanNumber,wfs,UCL,prolog_code,KB,Ctx,TN,Author,disabled)),
	 disable_can_conj(PrologFormS).

disable_can_number(CanNumber):-
	 retract(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,State)),
	 assert(sigmaCache(PredR,CanNumber,wfs,UCL,PrologFormS,KB,Ctx,TN,Author,disabled)),
	 disable_can_conj(PrologFormS).
disable_can_number(CanNumber):-!.
	 
disable_can_conj(true):-!.
disable_can_conj((Prolog,Form)):-!,
	disable_can_conj(Prolog),
	disable_can_conj(Form),!.
disable_can_conj(PrologForm):-
	disable_can(PrologForm).
	 
	 
disable_can(PrologFormS):-catch(erase(AssertID),_,true),fail.
disable_can(PrologForm):-!.

	
% ===========================================================
% Disable Assertion
% ===========================================================
parse_sigma_enable(Options):-memberchk(submit='Disable Assertion',Options),!,
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	writeFmt('<H3><Font Color=Red>Disabling....</Font></H3>',[]),
	disable_assertion(AID).
	
disable_assertion(AID):-
	retract(sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,on)),
	disable_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author),
	assertaClean(sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,disabled)),fail.
	
disable_assertion(AID):-writeFmt('<H3><Font Color=Red>Done.</Font></H3>',[]).

disable_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author):-
	toMarkUp(html,SURF,Vars,SAtom),
	writeFmt('<IMG src="pixmaps/bullet.gif" asrtid=~w><nobr>',[AID]),
	writeFmt('<b>~w</b> ID<font color=red>~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font>',[Form,AID,KB,Ctx,Author]),
	%format_o('&nbsp;&nbsp;~w&nbsp;Enabled&nbsp;&nbsp;<br>',checkbox(AID,OnOff)),
	writeFmt('~w<br>',[SAtom]),!.

show_disable_assertions(Form,SURF,Vars,KB,Ctx,AID,Author,OnOff):-
	toMarkUp(html,SURF,Vars,SAtom),
	writeFmt('<IMG src="pixmaps/bullet.gif" asrtid=~w><nobr>',[AID]),
	writeFmt('~w <b>~w</b> ID<font color=red>~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font>',[OnOff,Form,AID,KB,Ctx,Author]),
	%format_o('&nbsp;&nbsp;~w&nbsp;Enabled&nbsp;&nbsp;<br>',checkbox(AID,OnOff)),
	writeFmt('~w<hr>',[SAtom]),!.

% ===========================================================
% Enable Assertion
% ===========================================================
parse_sigma_enable(Options):-memberchk(submit='Enable Assertion',Options),!,
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	writeFmt('<H3><Font Color=Red>Enabling....</Font></H3>',[]),
	enable_assertion(AID).
	
enable_assertion(AID):-
	retract(sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,_)),
	enable_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author),
	assertaClean(sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,on)),fail.
	
enable_assertion(AID):-writeFmt('<H3><Font Color=Red>Done.</Font></H3>',[]).

enable_assertion_disp(Form,SURF,Vars,KB,Ctx,AID,Author):-
	toMarkUp(html,SURF,Vars,SAtom),
	writeFmt('<IMG src="pixmaps/bullet.gif" asrtid=~w><nobr>',[AID]),
	writeFmt('<b>~w</b> ID<font color=red>~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font>',[Form,AID,KB,Ctx,Author]),
	%format_o('&nbsp;&nbsp;~w&nbsp;Enabled&nbsp;&nbsp;<br>',checkbox(AID,OnOff)),
	writeFmt('~w<br>',[SAtom]),!.
		

show_enable_assertions:-
	sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,on),
	show_enable_assertions(Form,SURF,Vars,KB,Ctx,AID,Author,OnOff),
	fail.
	
show_enable_assertions:-writeFmt('<H3><Font Color=Red>Done.</Font></H3>',[]).

show_enable_assertions(Form,SURF,Vars,KB,Ctx,AID,Author,OnOff):-
	toMarkUp(html,SURF,Vars,SAtom),
	writeFmt('<IMG src="pixmaps/bullet.gif" asrtid=~w><nobr>',[AID]),
	writeFmt('~w <b>~w</b> ID<font color=red>~w</font> in KB: <font color=green>~w</font>  CTX: <font color=green>~w</font>  Author: <font color=green>~w</font>',[OnOff,Form,AID,KB,Ctx,Author]),
	%format_o('&nbsp;&nbsp;~w&nbsp;Enabled&nbsp;&nbsp;<br>',checkbox(AID,OnOff)),
	writeFmt('~w<hr>',[SAtom]),!.

% ===========================================================
% Edit Assertion
% ===========================================================
parse_sigma_enable(Options):-memberchk(t='ea',Options),!,
	getSigmaOption(opt_ctx_assert='ToplevelContext',Ctx),
	getSigmaOption(opt_kb='Merge',KB),
	getSigmaOption(asid=_,AID),
	getSigmaOption(user='Web',User),
	getSigmaOption(interp='kif',Interp),
	sigmaCache(PredR,Form,SURF,Vars,KB,Ctx,AID,Author,_),
	%retractAllProlog(sigmaCache(PredR,_,_,_,KB,Ctx,AID,_,_)),
	toMarkUp(kif,SURF,Vars,Formula),
	writeFmt('<textarea rows=6 cols=90 name="sf">~w</textarea><br>',[Formula]),
	writeFmt('<br>&nbsp;&nbsp;<INPUT type=submit name=submit value="Update Source">&nbsp;<input type=hidden name=editid value="~w">',[AID	]),
	writeFmt('&nbsp;&nbsp;&nbsp;&nbsp;<INPUT type=radio name="interp" value="kif" checked>KIF</INPUT><INPUT type=radio name="interp" value="ace" DISABLED>ACE</INPUT>&nbsp;&nbsp;<A href="ask_tell.jsp?kb=~w">Cancel</a>',[KB]).



