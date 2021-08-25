:- ensure_loaded('sigma_swiprolog.pl').
:- include('sigma_header.pl').

% ===================================================================
% IMPORTS
% ===================================================================
%:-include('sigma_header.pl').

%S=(instance(A, 'BodyJunction')=>exists(B, exists(C, connected(A, C)and connected(A, B)and instance(C, 'AnatomicalStructure')and instance(B, 'AnatomicalStructure')and not equal(C, B)))),





cons:- !,
      %catch(cd('../../knowledge_bases/'),_,true),
      pwd,
      repeat, %told,seen,
      (unsetSigmaOption(client=html)),
      once(console_loop(C)),
      fail.

writeSigmaMenu:-
         writeFmt('ask - switches to ask mode \ntell - switches back to tell mode\n '),
         writeFmt('other cmds: statistics, ls, halt, contexts \n'),!.



:-dynamic(console_mode/1).
:-assert_n(console_mode(ask)).

console_loop(C):- console_mode(M),!,
      console_loop(M ,C).

console_loop(tell,SOURCEFORM):- 
            once(console_read('Tell> ',SOURCEFORM,Vars)),    
            invokeOperation(verbose,assert(SOURCEFORM),'ToplevelContext',TN,'Merge',CM,Vars).

console_loop(ask,SOURCEFORM):- 
            once(console_read('Ask> ',SOURCEFORM,Vars)),
            invokeOperation(verbose,query(SOURCEFORM),Ctx,TN,KB,CM,Vars).
      
console_loop(cmd,SOURCEFORM):- 
            once(console_read('Command> ',SOURCEFORM,Vars)),
            invokeOperation(verbose,cmd(SOURCEFORM),Ctx,TN,KB,CM,Vars).

console_read(P,FORM,Vars):-
         nl,write(P),cons_read(Askion_Chars),
         logOnFailure(getCleanCharsWhitespaceProper(Askion_Chars,Show)),!,
         logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
         logOnFailure(getSigmaTermFromSurface(STERM,NEWFORM)),!,
              catch(once(( NEWFORM=comment(_) -> 
                     (ignore(do_chars(Show)),!,FORM=_) ;(!,
		     FORM=NEWFORM      ))),_,fail).

cons_read(Chars):-repeat,told,seen,readKIF(Chars),Chars=[_,_|_].

lp_tell(Tell_chars):-
      tell(Tell_chars,Ctx,TN,KB,CM).

lp_tell_file(File):-lp_file(File).

lp_list:-
      writeFmt('Listing of the current assertions:\n',[]),
      listing(sigmaCache),
      writeFmt('Done.\n',[]).

lp_clear:-do_clear.

lp_save(FileName):-!.

lp_ask(Chars):-ask(Chars,_Cxt,KB).

   
cons_help:-!.

do_chars([59|Chars]):-!,do_chars(Chars).
do_chars(Chars):-tokenize3(Chars,Tokens),!,do_tokens(Tokens).

 

do_tokens([(;)|Toks]):-!,do_tokens(Toks).
do_tokens([tell]):-writeFmt('% Entering tell mode.\n',[]),retractAllProlog(console_mode(_)),assert(console_mode(tell)).
do_tokens([ask]):- writeFmt('% Entering ask mode.\n',[]),retractAllProlog(console_mode(_)),assert(console_mode(ask)).
do_tokens([cmd]):- writeFmt('% Entering command mode.\n',[]),retractAllProlog(console_mode(_)),assert(console_mode(cmd)).
do_tokens([clear]):- clear_sigma_memory.
do_tokens([list]):- lp_list.
do_tokens([load,Filename]):- do_tokens([load,Filename,'ToplevelContext']).
do_tokens([load,Filename,Ctx]):- ctxFromFile(Ctx,Filename,'Merge').

%do_chars(X):-string_concat(";find ",Word,X),find_word(Word).
do_tokens([make]):- make.
do_tokens([help]):- writeSigmaMenu.
do_tokens([halt]):- halt.
do_tokens([bye]):- abort.
%do_chars([116,109|Number]):-catch((number_codes(Value,Number),set_tm_level(Value)),_,show_tm_level).
do_tokens([prolog]):- writeFmt('Type ""cons"" to return to Logic Engine.\n ',[]),abort.
do_tokens([can,X]):-canonicalizeSigmaKBHTML('Merge',X).
do_tokens([can,X]):-canonicalizeSigmaKBHTML('Merge',X).
do_tokens([can]):-do_tokens([can,'ToplevelContext']).
do_tokens(L):-P=..L,once(P),!.
do_tokens(X):-writeFmt('could not parse: ~q.\n',[X]).
%do_chars(PCHARS):- atom_codes(Atom,PCHARS),catch(atom_to_term(Atom,Term,Vars),_,fail),!,catch(my_call(Term,Vars),_,fail).



kb_define(KB):-invokeTell(forall,surface,'instance'(KB,'KnowledgeBase'),'ToplevelContext',TN,KB,Vars,Author),
                     invokeTell(forall,surface,'instance'('ToplevelContext','Context'),'ToplevelContext',TN2,KB,Vars,Author),
                     ensureSigmaKB(KB,'ToplevelContext').

ctx_define(Ctx):-invokeTell(forall,surface,'instance'(Ctx,'Context'),'ToplevelContext',TN,SKB,Vars,Author),ensureSigmaKB(KB,Ctx).

set_ctx(Context):-ensureSigmaKB(KnowledgeBase,Context).
set_kb(KnowledgeBase):-ensureSigmaKB(KnowledgeBase,'ToplevelContext').

my_call(fol,_):-retractAllProlog(version_tag(_)),assert(version_tag(fol)),writeFmt('% Entering FOL mode.\n',[]),!.
my_call(kbl,_) :-retractAllProlog(version_tag(_)),assert(version_tag(kbl)),writeFmt('% Entering KBL mode.\n',[]),!.

my_call(cd(Term),_Vars):-cd(Term),pwd.

my_call(file(Term),_Vars):-lp_file(Term),writeFmt('% Entering ask mode.\n',[]),retractAllProlog(console_mode(_)),assert(console_mode(ask)).
my_call(file(Term),_Vars):- writeFmt('% Entering ask mode.\n',[]),retractAllProlog(console_mode(_)),assert(console_mode(ask)).

my_call(Term,_Vars):-once(Term).


do_chars(Show):-%,catch(fmtString(Atom,'"~s"',[Show]),_,ignore(Atom=surf)),
	sendNote(user,'kifParser','KIF Unreadable (Syntax error: Unbalanced parentheses  )',Atom).


call_nth_times(Number,Prolog):-
                     retractAllProlog(call_count(_)),assert(call_count(Number)),repeat,
                     (Prolog),
                     ignore((retract(call_count(Q)),QQ is Q -1,
                     assert(call_count(QQ)))),
                     call_count(0).

test_batch_stdout(FileName):- test_batch_stdout(FileName,CM,CPU,RESULT,Title).

test_batch_stdout(FileName,CM):- test_batch_stdout(FileName,CM,CPU,RESULT,Title).






