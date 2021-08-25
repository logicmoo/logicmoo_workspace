% ===================================================================
% File 'Sigma_translit.pl' 
% Author: Douglas Miles
% Contact: dmiles@teknowledge.com ;  apease@teknowledge.com
% Version: 'Sigma_translit.pl' 2.0.0 
% 
% Purpose: is to provide forall character processing and transliteration for KIF, Prolog, Sigma, SUMO
% ===================================================================
:-include('sigma_header.pl').

% TODO Need support for
/*
Predicate AFTER

% 3037 (pnx_nf (FORWARD (arg1Isa hasMembers Organization)) ToplevelContext T-3015) 
surface(DynStat,'clause-form'(arg1Isa(hasMembers,'Organization')),'BaseIKB','ToplevelContext','T-3015',_h75135). 
clf(arg1Isa(hasMembers,'Organization'),true,'BaseIKB','ToplevelContext','T-3015',3110).

)
% ===================================================================
% EXPORTS
% ===================================================================
 */
 
ssleep(_).       
                  
source_from_stream(Stream,[],surf,Vars):-at_end_of_stream(Stream),!.
source_from_stream(Stream,Trimed,Surface,Vars):-
		logOnFailure(once(readKIF_priv(Stream,RRAW))), %writeFmt('~s\n',[RRAW]),
		once(after_readKIF(Stream,RRAW,Trimed,Surface,Vars)),!.
source_from_stream(Stream,Trimed,Surface,Vars):-line_count(Stream,Line),writeFmt(user_error,'\nLine ~w  Unreadable KIF (source_from_stream) \n',[Line]),ssleep(2),!.

after_readKIF(Stream,RRAW,L_trimmed,Surface,Vars):-
			logOnFailure(getCleanCharsWhitespaceProper(RRAW,Trimed)),
			(once(ltrim(Trimed,L_trimmed))),
			source_from_clean_chars(Stream,L_trimmed,Surface,Vars).

after_readKIF(Stream,RRAW,Trimed,surf,Vars):- line_count(Stream,Line),writeFmt('\nLine ~w  Unreadable KIF: ~s (after_readKIF) \n',[Trimed,RRAW]),ssleep(2),!.

source_from_chars(RRAW,Surface,Vars):-
			source_from_chars(user_input,RRAW,Surface,Vars).

source_from_chars(Stream,RRAW,Surface,Vars):-
			logOnFailure(once(getCleanCharsWhitespaceProper(RRAW,Trimed))),
			logOnFailure(once(ltrim(Trimed,L_trimmed))),
			source_from_clean_chars(Stream,L_trimmed,Surface,Vars).
       
source_from_clean_chars(Stream,"var in stream",surf,Vars):-!,line_count(Stream,Line),writeFmt('\nLine ~w  Var in Stream KIF " \n',[Line]),ssleep(2),!.
source_from_clean_chars(Stream,[40|REST],Surface,Vars):-
				once(getSurfaceFromChars([40|REST],KIFSTERM,Vars)),
				once(source_from_sterm(Stream,KIFSTERM,Surface,Vars)),!.

source_from_clean_chars(Stream,[59|REST],file_comment(Atom),Vars):-!,string_to_atom([59|REST],Atom).% ignore(catch(fmtString(CMT,'~s',[59|REST]),_,fmtString(CMT,'~w',[59|REST]))).

source_from_clean_chars(Stream,[],surf,Vars).
source_from_clean_chars(Stream,[10],surf,Vars).
source_from_clean_chars(Stream,[13],surf,Vars).
source_from_clean_chars(Stream,Trimed,file_comment(nil),Vars):-!,line_count(Stream,Line),ignore(catch(writeFmt('\nLine ~w  Unreadable KIF "~s"\n',[Line,Trimed]),_,true)),ssleep(2),!.


source_from_sterm(Stream,KIFSTERM,Surface,Vars):-
			getSigmaTermFromSurface(KIFSTERM,Surface).

source_from_sterm(Stream,KIFSTERM,Surface,Vars):-(line_count(Stream,Line),writeFmt('\nLine ~w  Uninterpretable "~q" (~q)\n',[Line,KIFSTERM,kif])),ssleep(2).

/*

Purpose:                                                         
  
Transliterator to get KIF-like KR turned into a more expressive ground form sometimes using only syntax rules

Used by sigma_server

% These predicates are used by sigma_useragent.pl and sigma_operation.pl

:-export 
      pterm_to_sterm/2, 
      sterm_to_pterm/2,
      conv_to_sterm/3,
      conv_to_pterm/3,
      conv_full_trans_query/3,
      conv_full_trans_generic/3,
      conv_full_trans_assert/3,
      getSurfaceFromChars/3.

  In XSB modules cannot use DCGs
*/

                           
% ===================================================================
% EXPORTS
% ===================================================================
                         
/*

:-export readKIF/1.
:-export readKIF/2.

:-export conv_readS/3.
:-export conv_readP/3.
:-export conv_readS/4.
:-export conv_readP/4.
:-export e_o_f/1.
:-export conv_file_line_format/6.

  */                         
                           
% ===================================================================
% IMPORTS
% ===================================================================

                                                         
:-assert(re_entry(conv_kr_rule_assert)).

kif_to_xml(KIF,ML):-
         tell_retract_parse_chars(KIF,X,V),
         toMarkUp(leml,X,V,ML).


% =====================================================================================
%  tell_retract_parse_chars(Chars,FORM,Vars) and  ask_parse_chars(Chars,FORM,Vars)
%  Both Return FORM='nil' if not well formed
% =====================================================================================

tell_retract_parse_chars(Chars,FORM,Vars):-
         logOnFailure(getCleanCharsWhitespaceProper(Chars,Show)),!,
         once(getSurfaceFromChars(Show,STERM,Vars)),!,
         getSigmaTermFromSurface(STERM,NEWFORM),!,  
              once(( 
                    NEWFORM=browser_only(browser_only(comment(end_of_file))) -> 
                           ((
                             catch(fmtString(What,'~s',[Show]),_,What='Unreadable'),!,
                             sendNote(user,kifParser,'Assertion/Retraction Syntax error: Unmatched parentheses',['"',What,'"',nl,helplink('Syntactical Well Formedness','syntax.html')]),
                             FORM=nil 
                             ))
                                 ;
                           (!,FORM=NEWFORM)
                     )).


ask_parse_chars(Chars,FORM,Vars):-
         logOnFailure(getCleanCharsWhitespaceProper(Chars,Show)),!,
         once(getSurfaceFromChars(Show,STERM,Vars)),!,
         getSigmaTermFromSurface(STERM,NEWFORM),!,
              once(( 
                    NEWFORM=browser_only(browser_only(comment(end_of_file))) -> 
                           ((
                             catch(fmtString(What,'~s',[Show]),_,What='Unreadable'),!,
                             sendNote(user,kifParser,'Query Syntax error: Unmatched parentheses',['"',What,'"',nl,helplink('Syntactical Well Formedness','syntax.html')]),
                             FORM=nil 
                             ))
                                 ;
                           (!,FORM=NEWFORM)
                     )).


/*===================================================================
Convert Prolog Term to S-Expression

Recursively Applies the Univ Op to create an easier to compile prolog writeFmt
                                                 
Examples:

| ?- pterm_to_sterm((G(X,Y):-A(X,Y)),Sterm).
Sterm = [:-,[_h76,_h90,_h104],[_h122,_h90,_h104]]

| ?- pterm_to_sterm(t,Sterm).
Sterm = [t]

| ?- pterm_to_sterm(t(a),Sterm).
Sterm = [t,[a]]

| ?- pterm_to_sterm(and(a,b),Sterm).
Sterm = [and,[a],[b]]

====================================================================*/
%pterm_to_sterm(X,X):-!,writeq(pterm_to_sterm(X,X)).


pterm_to_sterm(VAR,VAR):-isSlot(VAR),!.
pterm_to_sterm([],['AssignmentFn','Set',[]]):-!.
pterm_to_sterm(ATOM,[ATOM]):-atomic(ATOM),!.
pterm_to_sterm(PTERM,STERM):-compound(PTERM),
            PTERM=..[holds,P|PARGS],         !,
            pterm_to_sterm_list(PARGS,SARGS),
            STERM=[P|SARGS].
pterm_to_sterm(PTERM,STERM):-compound(PTERM),!,
            PTERM=..[P|PARGS],
            pterm_to_sterm_list(PARGS,SARGS),
            STERM=[P|SARGS].

pterm_to_sterm_list([],[]):-!.
pterm_to_sterm_list([P|PTERM],[S|STERM]):-!,
              pterm_to_sterm(P,S),
              pterm_to_sterm_list(PTERM,STERM).

/*===================================================================
Convert Prolog Term to S-Expression

Recursively Applies the Univ Op to create an easier to compile prolog writeFmt
                                                 
Examples:

| ?- pterm_to_sterm_native((G(X,Y):-A(X,Y)),Sterm_native).
Sterm_native = [:-,[holds,_h76,_h90,_h104],[holds,_h122,_h90,_h104]]

| ?- pterm_to_sterm_native(t,Sterm_native).
Sterm_native = [t]

| ?- pterm_to_sterm_native(t(a),Sterm_native).
Sterm_native = [t,[a]]

| ?- pterm_to_sterm_native(and(a,b),Sterm_native).
Sterm_native = [and,[a],[b]]

====================================================================*/
%pterm_to_sterm_native(X,X):-!,writeq(pterm_to_sterm_native(X,X)).


pterm_to_sterm_native(VAR,VAR):-isSlot(VAR),!.
pterm_to_sterm_native([],['AssignmentFn','Set',[]]):-!.
pterm_to_sterm_native(ATOM,[ATOM]):-atomic(ATOM),!.
pterm_to_sterm_native(PTERM,STERM_NATIVE):-compound(PTERM),!,
            PTERM=..[P|PARGS],
            pterm_to_sterm_native_list(PARGS,SARGS),
            STERM_NATIVE=[P|SARGS].

pterm_to_sterm_native_list([],[]):-!.
pterm_to_sterm_native_list([P|PTERM],[S|STERM_NATIVE]):-!,
              pterm_to_sterm_native(P,S),
              pterm_to_sterm_native_list(PTERM,STERM_NATIVE).

/*===================================================================
Convert S-Expression originating from user to a Prolog Clause representing the surface level

Recursively creates a Prolog term based on the S-Expression to be done after compiler
                                                 
Examples:

| ?- sterm_to_pterm([a,b],Pterm).
Pterm = a(b)

| ?- sterm_to_pterm([a,[b]],Pterm).    %Note:  This is a special Case
Pterm = a(b)

| ?- sterm_to_pterm([holds,X,Y,Z],Pterm).    %This allows Hilog terms to be Converted
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm([X,Y,Z],Pterm).   %But still works in normal places
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm(['AssignmentFn',X,[Y,Z]],Pterm).                                
Pterm = 'AssignmentFn'(_h84,[_h102,_h116])
====================================================================*/

sterm_to_pterm(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm([VAR],VAR):-isSlot(VAR),!.
sterm_to_pterm([X],Y):-!,nonvar(X),sterm_to_pterm(X,Y).

sterm_to_pterm([S|TERM],PTERM):-isSlot(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-number(S),!,
            sterm_to_pterm_list([S|TERM],PTERM).            
	    
sterm_to_pterm([S|TERM],PTERM):-nonvar(S),atomic(S),!,
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-!,  atomic(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm(VAR,VAR):-!.

sterm_to_pterm_list(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_list([],[]):-!.
sterm_to_pterm_list([S|STERM],[P|PTERM]):-!,
              sterm_to_pterm(S,P),
              sterm_to_pterm_list(STERM,PTERM).
sterm_to_pterm_list(VAR,[VAR]).

/*===================================================================
Convert S-Expression originating from user to a Prolog Clause representing the surface level

Recursively creates a Prolog term based on the S-Expression to be done after compiler
                                                 
Examples:

| ?- sterm_to_pterm_native([a,b],Pterm_native).
Pterm_native = a(b)

| ?- sterm_to_pterm_native([a,[b]],Pterm_native).    %Note:  This is a special Case
Pterm_native = a(b)

| ?- sterm_to_pterm_native([holds,X,Y,Z],Pterm_native).    %This allows Hilog terms to be Converted
Pterm_native = _h76(_h90,_h104)                    

| ?- sterm_to_pterm_native([X,Y,Z],Pterm_native).   %But still works in normal places
Pterm_native = _h76(_h90,_h104)                    

| ?- sterm_to_pterm_native(['AssignmentFn',X,[Y,Z]],Pterm_native).                                
Pterm_native = 'AssignmentFn'(_h84,[_h102,_h116])
====================================================================*/

sterm_to_pterm_native(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_native([VAR],VAR):-isSlot(VAR),!.
sterm_to_pterm_native([X],Y):-!,nonvar(X),sterm_to_pterm_native(X,Y).
sterm_to_pterm_native([S|TERM],PTERM_NATIVE):-isSlot(S),
            sterm_to_pterm_native_list(TERM,PLIST),            
            PTERM_NATIVE=..[holds,S|PLIST].
sterm_to_pterm_native([S|TERM],PTERM_NATIVE):-number(S),!,
            sterm_to_pterm_native_list([S|TERM],PTERM_NATIVE).            
sterm_to_pterm_native([S|TERM],PTERM_NATIVE):-nonvar(S),atomic(S),!,
            sterm_to_pterm_native_list(TERM,PLIST),            
            PTERM_NATIVE=..[S|PLIST].
sterm_to_pterm_native([S|TERM],PTERM_NATIVE):-!,  atomic(S),
            sterm_to_pterm_native_list(TERM,PLIST),            
            PTERM_NATIVE=..[holds,S|PLIST].
sterm_to_pterm_native(VAR,VAR):-!.

sterm_to_pterm_native_list(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_native_list([],[]):-!.
sterm_to_pterm_native_list([S|STERM],[P|PTERM_NATIVE]):-!,
              sterm_to_pterm_native(S,P),
              sterm_to_pterm_native_list(STERM,PTERM_NATIVE).
sterm_to_pterm_native_list(VAR,[VAR]).

% [and,A,B]                           lc(pos,and(A,B))                     A equal B
% [not,[and,A,B]]                   lc(neg,and(A,B))                  ~ A equal B
% [or,A,B]                             lc(pos,or(A,B))                        A v B
% ['relation-const',V,B]            lc(pos,pc(pos,'relation-const',[V,B]))          relation-const(V,B)
% [thereExists,V,['relation-const',V,B]]        lc(pos,exists(1,V,pc(pos,'relation-const',[V,B])))          ex V:  r(V,B)
% [forall,V,[not,['relation-const',V,B]]]          lc(pos,univ(forall,V,pc(neg,'relation-const',[V,B])))               ex V:  ~r(V,B)
% ['AssignmentFn',A,[B]]                         'AssignmentFn'(A,[B])
% Fido   ->                            'AssignmentFn'(_,['Fido'])
% [+,1,1] ->                          comp(+(1,1))
% [list,1,2] ->                        varparams([1,2])


% lc -> or clause
% lc -> and clause
% lc -> predicate constant clause
% lc -> exists clause
% lc -> univ clause
% lc -> => clause
% lc -> <=> clause
                                        
         
% ========================================================
%          Atom Transliteration
% ========================================================




% Prolog Declarations
'surface-instance'((':-'),'ImplicationConnective',_).
'surface-instance'((','),'ConjunctionalConnective',_).
'surface-instance'((';'),'DisjunctionalConnective',_).
'surface-instance'((':-'),'Connective',_).
'surface-instance'((','),'Connective',_).
'surface-instance'((';'),'Connective',_).
'surface-instance'(('=>'),'ImplicationConnective',_).
'surface-instance'(('and'),'ConjunctionalConnective',_).
'surface-instance'(('or'),'DisjunctionalConnective',_).
'surface-instance'(('=>'),'Connective',_).
'surface-instance'(('and'),'Connective',_).
'surface-instance'(('or'),'Connective',_).

% Brought in from sigma_language.[KIF|P]
'surface-instance'(A,B,_):-'surface-instance'(A,B).
'surface-instance'(A,C,_):-'surface-instance'(A,B),'surface-subclass'(B,C).
'surface-instance'(A,D,_):-'surface-instance'(A,B),'surface-subclass'(B,C),'surface-subclass'(C,D).
'surface-instance'(A,E,_):-'surface-instance'(A,B),'surface-subclass'(B,C),'surface-subclass'(C,D),'surface-subclass'(D,E).


% Brought in from sigma_language.[KIF|P]
'surface-instance'(Arity2Pred,'ArityTwoPredicate',_C):-'surface-multiple-arity'(Arity2Pred).
'surface-instance'(Arity1Pred,'ArityOnePredicate',_C):-'surface-single-arity'(Arity1Pred).
'surface-instance'(AE,'Quantifier',_):-'surface-quantifier'(AE).
'surface-instance'(findall,'Quantifier',_).


'surface-instance'(and,'ArityTwoPredicate',_C).
'surface-instance'(or,'ArityTwoPredicate',_C).
'surface-instance'('<=>','ArityTwoPredicate',_C).
'surface-instance'('=>','ArityTwoPredicate',_C).
         
% ========================================================
%           Generic Transliteration
% ========================================================

conv_kr_rule_generic(V,V):- isSlot(V),!.


conv_kr_rule_generic(IN,Out):- conv_pred(IN,Out),!.
conv_kr_rule_generic(G,G).

conv_kr_re_entry(IN,Out):-conv_pred(IN,Out),!.

% ========================================================
%           Common Transliteration  Expects S-Expression
% ========================================================

conv_pred(V,V):- isSlot(V),!.
conv_pred([],[]).
conv_pred([V],[V]):- isSlot(V),!.
conv_pred( Before, After) :- atom(Before),'surface-word'( Before, After),!.

% Variable in the 1st Position
conv_pred([Op|A],[Op|List]):-isSlot(Op),!,conv_pred_list(A,List).


conv_pred([V,A|R],New):- copy_term([V,A|R],Process),	
		       'surface-macro'(Process,_),
		       once((getPrologVars([V,A|R],BV,_,_), 
		       getPrologVars(Process,PV,_,_))),
		       length(PV,X),length(BV,X),!,
		       'surface-macro'([V,A|R],NewTerm),!,
		       conv_kr_re_entry(NewTerm,New).


% Arity2Predicates that have only More then 2 Arguments
conv_pred([Arity2Pred,A,B,C|M],List):-   
                              'surface-instance'(Arity2Pred,'ArityTwoPredicate',_C),!,
                                conv_functsymbol_two([Arity2Pred,A,B,C|M],MID),!,
                                conv_kr_re_entry(MID,List).

	conv_functsymbol_two([],[]).
	conv_functsymbol_two([Pred,A,B,C|More],List):-!,
		    conv_functsymbol_two([Pred,B,C|More],SEMORE),
		    conv_functsymbol_two([Pred,A,SEMORE],List).
	conv_functsymbol_two([_Pred,_A,_B],[_Pred,_A,_B]):-!.
	conv_functsymbol_two([_Pred,_A],_A):-!.


% Arity2Predicates that have only one Argument (Are squashed)
conv_pred([Arity2Pred,MID],List):-   
                              'surface-instance'(Arity2Pred,'ArityTwoPredicate',_C),!,
                                conv_kr_re_entry(MID,List).

% multiple arity not/not
conv_pred([Arity1Pred,A,B|More],List):-  
	    'surface-instance'(Arity1Pred,'ArityOnePredicate',_C),!,
            conv_kr_re_entry([Arity1Pred,B|More],BEMORE),
            conv_kr_re_entry([and,[Arity1Pred,A],BEMORE],List).

% Arity1Predicates that are logical connectives
conv_pred([Arity1Pred,A],[Arity1Pred,List]):- 'surface-instance'(Arity1Pred,'Connective',_C),!,
              conv_kr_re_entry(A,List).


% ========================================================
% Agregation Predicates
% ========================================================
%conv_kr_re_entry([Agregation,Entity,[=>,Ant,Con]],Formula):-nonvar(Agregation),member(Agregation,[forall,forall,for_all,forall]),conv_kr_re_entry([=>,[and,exists(Entity),Ant],Con],Formula).

% Single Entity Exists  %[exists,[list],tr]                                           
conv_pred([AE,[Entity,Collection],FormulaA],Result):- ('surface-instance'(AE,'ExistentualQuantifier',_)),
	conv_kr_re_entry(AE,AO),
            isSlot(Entity),nonvar(Collection),!,conv_kr_re_entry(FormulaA,FormulaAO),
            conv_kr_re_entry(['and',[instance,Entity,Collection],FormulaAO],FormulaB),
            conv_kr_re_entry([AO,Entity,FormulaB],Result).

conv_pred([AE,[Entity,Collection],FormulaA],Result):- ('surface-instance'(AE,'UniversalQuantifier',_)),
            isSlot(Entity),nonvar(Collection),!,conv_kr_re_entry(FormulaA,FormulaAO),
            conv_kr_re_entry(['=>',[instance,Entity,Collection],FormulaAO],Result).

conv_pred([AE,Entity,FormulaA],[AO,Entity,FormulaB]):- ('surface-instance'(AE,'Quantifier',_)),
	conv_kr_re_entry(AE,AO),
            isSlot(Entity),!,
            conv_kr_re_entry(FormulaA,FormulaB).

conv_pred([AE,[],FormulaA],FormulaB):- ('surface-instance'(AE,'Quantifier',_)),!,
            conv_kr_re_entry(FormulaA,FormulaB).

conv_pred([AE,[Struct|More],FormulaA],Result):- ('surface-instance'(AE,'Quantifier',_)),
            !,   conv_kr_re_entry(FormulaA,FormulaAO),  %%%% AO
	conv_kr_re_entry(AE,AO),
            conv_kr_re_entry([AO,Struct,FormulaAO],ResultFormulaA),
            conv_kr_re_entry([AO,More,ResultFormulaA],Result).

%conv_pred([Pred|ARGS],[browser_only,[Pred|ARGS]]):-'browser-only'(Pred),!.

conv_pred([Class,Pred], ['instance',Pred,Class]):-nonvar(Pred),'surface-class'(Class),!.               

conv_pred([A|T],[AO|TO]):-!,conv_pred_list([A|T],[AO|TO]).

conv_pred(A,A):-!.

conv_pred_list(Var,Var):-isSlot(Var),!.
conv_pred_list([],[]):-!.
conv_pred_list([H|T],[HH|TT]):-!,
         conv_kr_re_entry(H,HH),
         conv_pred_list(T,TT).


% ========================================================
% Mine Out Formulas Durring Transliteration
% ========================================================

reduce_arg_nth(_C,_,_,[],[]):-!.
reduce_arg_nth(_C,Pred,N,[ArgS|ArgSS],[ArgSO|ArgSOS]):-!,
            conv_kr_re_entry(ArgS,ArgSO),
            reduce_arg_nth(_C,Pred,NN,ArgSS,ArgSOS).

% Need to be moved to KIF File
kif_to_pterm_nv_fn([FN,ID,LITS],LITP):-nonvar(ID),ID='Set',kif_to_pterm_list(LITS,LITP),!.
kif_to_pterm_nv_fn([FN,ID,LITS],'AssignmentFn'(ID,LITP)):-kif_to_pterm_list(LITS,LITP),!.
kif_to_pterm_nv_fn([FN,ID|LITS],LITP):-nonvar(ID),ID='Set',kif_to_pterm_list(LITS,LITP),!.
kif_to_pterm_nv_fn([FN,ID|LITS],'AssignmentFn'(ID,LITP)):-kif_to_pterm_list(LITS,LITP),!.


kif_to_pterm(VAR,VAR):-isSlot(VAR),!.
kif_to_pterm([KIF],P):-!,kif_to_pterm(KIF,P).
kif_to_pterm('zzskFn'(X),'zzskFn'(X)):-!.
kif_to_pterm([AS,X,Y],'surface-macro'(X,Y)):-nonvar(AS),AS='surface-macro',!.
%kif_to_pterm([Fn, Fn, [X|LIST]],'AssignmentFn'(X,PLIST)):-nonvar(Fn),Fn='AssignmentFn',!,kif_to_pterm_list(LIST,PLIST).
%%kif_to_pterm([ID,FN|LITS],OUT):-nonvar(FN),FN='AssignmentFn',kif_to_pterm([FN,ID|LITS],OUT),!.
%kif_to_pterm([FN,ID|LITS],OUT):-nonvar(FN),FN='AssignmentFn',kif_to_pterm_nv_fn([FN,ID|LITS],OUT),!.
kif_to_pterm([X],Y):-!,nonvar(X),kif_to_pterm(X,Y).
kif_to_pterm(ATOM,ATOM):-atomic(ATOM),!.

kif_to_pterm([S|TERM],PTERM):-isSlot(S),
            kif_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

kif_to_pterm([holds|TERM],PTERM):-!,
            kif_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds|PLIST].
	    	    
kif_to_pterm([S|TERM],PLIST):-nonvar(S),number(S),!,
            kif_to_pterm_list([S|TERM],PLIST).    
	            
kif_to_pterm([S|TERM],PTERM):-nonvar(S),atomic(S),!,
            kif_to_pterm_list(TERM,PLIST),            
            PTERM=..[S|PLIST].
	    
kif_to_pterm(VAR,VAR):-!.

kif_to_pterm_list(VAR,VAR):-isSlot(VAR),!.
kif_to_pterm_list([],[]):-!.
kif_to_pterm_list([S|STERM],[P|PTERM]):-!,
              kif_to_pterm(S,P),
              kif_to_pterm_list(STERM,PTERM).
	      
kif_to_pterm_list(VAR,[VAR]).


'surface-domain'(salientAssertions,2,'Formula',_Cxt).
'surface-domain'(ist,2,'Formula',_Cxt).

'surface-domain'(P,1,'Formula',_Cxt):-nonvar(P),'surface-instance'(P,'Connective',_).
'surface-domain'(P,2,'Formula',_Cxt):-nonvar(P),'surface-instance'(P,'Connective',_).
'surface-domain'(P,N,T,_):-'domain'(P,N,T,_Cxt).
'surface-domain'(P,N,T,_):-'domain'(P,N,T2,_Cxt),'subclass'(T,T2,_Cxt).

:-dynamic('domain'/4).
:-dynamic('subclass'/3).




/*===================================================================
% getSurfaceFromChars/3 is does less consistantsy checking then conv_to_sterm

Always a S-Expression: 'WFFOut' placing variables in 'VARSOut'

|?-getSurfaceFromChars("(isa a b)",Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(isa a (b))",Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-getSurfaceFromChars("(list a b )",Clause,Vars)
Clause = [list,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(genlMt A ?B)",Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- getSurfaceFromChars("(goals Iran  (not   (exists   (?CITIZEN)   (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN
)))))",Clause,Vars).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]

====================================================================*/

getSurfaceFromChars([],[end_of_file],_):-!.

getSurfaceFromChars([CH|ARSIn],TERM,VARS):-!, 
         %getCleanCharsWhitespaceProper(CHARSIn,NoWhiteCHARS),!,  
         logOnFailure(ltrim([CH|ARSIn],CHARS)),!,
              CHARS=[FC|REST],!,
          (( 
            ([FC]=";",TERM=[comment,end_of_file], VARS= _ ) ;   %Comment Char found in Line
            (CHARS=[],TERM=nil,VARS=_,! 	  )    %String came empty
            ;
            (FC=40,getSurfaceFromChars_2(CHARS,TERM,VARS) ,! )    %Use vanila KIF parser
            ;
            ( TERM=[comment,end_of_file],VARS= _,! )     %All above methods of parsing failed.. Convert to comment
            )).
	    
getSurfaceFromChars(C,TERM,VARS):-string_to_list(C,List),!,getSurfaceFromChars(List,TERM,VARS),!.


getSurfaceFromChars_2(Chars,WFFOut,VARSOut):- 
    retractAllProlog(var_counter(_)),retractAllProlog(numbered_var(_,_,_)),asserta(var_counter(0)), 
               once(getKIFTokens(Chars,Tokens)), 
               once((clean_sexpression(Tokens,WFFClean))),
               phrase(sigma(WFF),WFFClean),
               collect_temp_vars(VARS),
              !, ( 
                     (VARS=[],VARSOut=_,WFFOut=WFF)
               ;
                     (
                     unnumbervars(VARS,LIST),
                     kifVarNums(LIST,WFF,WFFOut,VARSOut2) ,
                     list_to_set(VARSOut2,VARSOut1),
                     open_list(VARSOut1,VARSOut)
                     ) 
               ).

    

/*===================================================================
% clean_sexpression(Tokens,CleanTokens)

Removes out SUMO tokens

====================================================================*/

clean_sexpression([],[]).
clean_sexpression(['#$'|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression(['#'|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression(['$'|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression([E|WFF],[E|WFFClean]):-clean_sexpression(WFF,WFFClean).


%isCharCodelist([]):-!.
%isCharCodelist([H|T]):-!,integer(H),isCharCodelist(T).

/*===================================================================
% S-Expression Version of ISO-Prolog chars_to_tem/3
====================================================================*/
chars_to_term_s(CHARS,TERM,VARS):-
             once(chars_to_term(CHARS,PTERM,VARS)),
             once(pterm_to_sterm(PTERM,TERM)).


/*===================================================================
% Safe Entry Call Into ISO-Prolog tokenize_chars/2
====================================================================*/

getKIFTokens(X,Z):-is_list(X),!,  tokenize_chars(X,Y),convert_the_atoms(Y,Z).

convert_the_atoms([],[]):-!.
convert_the_atoms([H|T],[HH|TT]):-!,  
                convert_the_atom(H,HH),
                convert_the_atoms(T,TT).

%convert_the_atom(H,HH):-atom_codes(H,[34|Rest]),reverse(Rest,[_|AtomCharsR]),reverse(AtomCharsR,AtomChars),atom_codes(HH,AtomChars).
%convert_the_atom(H,HH):-atom_codes(H,[39|Rest]),reverse(Rest,[_|AtomCharsR]),reverse(AtomCharsR,AtomChars),atom_codes(HH,AtomChars).
convert_the_atom(H,H):-!.


getKIFTokens(X,[X]). 

/*===================================================================
% Removes Leading whitespaces and not ANSI charset
====================================================================*/

ltrim([],[]):-!.
ltrim([P|X],Y):-P<33,ltrim(X,Y),!.
ltrim([P|X],Y):-P>128,ltrim(X,Y),!.
ltrim(X,X):-!.

/*===================================================================
%   SUO-KIF String to SXpression
% Converts up to 6 SUO-KIF Flags into set(Name,Val) pairs
====================================================================*/

suokif((XSB_Prolog))--> ['(',':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),kif_flag(_),kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
suokif((XSB_Prolog))--> ['('],kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),kif_flag(_),[':','SUOKIF'],sigma(XSB_Prolog),[')'].
  
suokif(XSB_Prolog)--> sigma(XSB_Prolog).

flag_list((A,B)) --> kif_flag(A),flag_list(B).
flag_list(A) --> kif_flag(A).
flag_list(true) --> [].

kif_flag('set'(A,V)) -->  [(':'),A,V], { atomical(V) }.
kif_flag('set'(A,true)) -->  [(':'),A], { atomical(A) }.

/*===================================================================
%   Sigma-KIF String to DCG Converter
% Converts up to 13 forms
%     13 Terms long
%  
% =169 Parens Pairs at the First 2 levels  
% 
====================================================================*/


sigma([A]) --> expr(A).
sigma([and,A|L]) --> expr(A) , sigma(L).

   %%expr(RF) --> reifiableFN(RF),!.
expr([]) -->  ['(',')'],!.
expr([Head]) -->  ['('],opr(Head),[')'],!.
expr([Head|LIST]) -->  ['('],opr(Head),many_slots(LIST),[')'].

many_slots([A]) --> slot(A).
many_slots([A|L]) --> slot(A) , many_slots(L).

opr(Head) --> simple(Head) .
opr(Head) --> expr(Head).

%slot(Name) --> simple(Name),['AssignmentFn'], { nonvar(Name), ! }.
slot(SKFName) --> ['SKF'],simple(Name), { nonvar(Name), ! , skf_name(Name,SKFName) }.
slot(WFF) -->  simple(WFF), { nonvar(WFF), ! }.
%slot(['AssignmentFn',Name,List]) -->  reifiableFN(['AssignmentFn',Name,List]).
slot(WFF) -->  expr(WFF), { nonvar(WFF), ! }.


expr(WFF) -->  variable(WFF), { nonvar(WFF) ,!}.
%expr(WFF) -->  reifiableFN(WFF), { nonvar(WFF),! }.   %slot(WFF) -->  literal_list(WFF), { nonvar(WFF) }.


variables_list([list,A]) --> qual_var(A).
variables_list([list,A]) -->  ['('],qual_var(A),[')'],!.
variables_list([list,A,B]) -->  ['('],qual_var(A),qual_var(B),[')'],! .
variables_list([list,A|QV]) -->  ['('],qual_var(A),many_qual_var(QV),[')'],!.
many_qual_var([A]) -->  qual_var(A).
many_qual_var([A|T]) -->  qual_var(A),many_qual_var(T).

% Var/Quality pairs that Sowa's ACE examples use

qual_var(VN) --> ['('],variable(VN),[')'].
qual_var(VN) --> variable(VN).
qual_var(VN) --> ['('],variable(VN),qual(_Quality),[')'].

qual(Q) --> constant(Q), { nonvar(Q) }. % , 'surface-instance'(_,Q,_) }.

number(Number) -->  [Number] , {  nonvar(Number), number(Number),! } .

quantity(Number) --> number(Number).

simple(WFF) -->  quantity(WFF), { nonvar(WFF), ! }.
simple(WFF) -->  variable(WFF), { nonvar(WFF), ! }.
simple(WFF) -->  constant(WFF), { nonvar(WFF), ! }.
%simple(['AssignmentFn',Name,[]]) --> ['SKF'],constant(Name).
%simple(['AssignmentFn',Name,[]]) --> ['SKF'],simple(Name),{ nonvar(Name) , nonvar(List), ! } .
%simple(['AssignmentFn',Name,[]]) --> ['AssignmentFn'],simple(Name), { nonvar(Name) , nonvar(List), ! } .

%reifiableFN(['AssignmentFn',SKFName,[]]) --> ['(','SKF'],simple(Name),[')'], { nonvar(Name) ,! , skf_name(Name,SKFName),sendNote('(skf)') } .
%reifiableFN(['AssignmentFn',SKFName,List]) --> ['(','SKF'],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List), ! , skf_name(Name,SKFName),sendNote('(skf)') } .
%reifiableFN(['AssignmentFn',Name,List]) --> ['(','AssignmentFn'],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List), ! } .
%%reifiableFN(['AssignmentFn',Name,_]) --> ['SKF'],simple(Name).
%reifiableFN(['AssignmentFn',Name,List]) --> ['('],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List),'surface-instance'(Name,'Function',_) ,! } .

skf_name(Num,SKFName):-!,number(Num), number_codes(Num,Codes),atom_codes(SKFName,[115,107|Codes]).

% Construct arbitrary list of args
          
arbitrary([]) -->  [].
arbitrary(VN)-->  ['?',A], { var_number(A,VN)   } . 
arbitrary([Head]) -->  slot(Head).
arbitrary([A|L]) --> slot(A) , many_slots(L).


variable(VN)-->  ['?',A], { var_number(A,VN)   } . 
variable(VN)-->  ['??'], { var_gen(A),var_number(A,VN)   } .     %Anonymous
variable(VN)-->  ['?'], { var_gen(A),var_number(A,VN)   } . 

% Makes up sequencial Variable names for anonymous kif getPrologVars
var_gen(Atom):-idGen(Number),number_codes(Number,Codes),atom_codes(Atom,[86,65,82|Codes]). % "VAR"

constant(Number) --> number(Number) .
   
constant(Unquoted) -->  [Unquoted] , {  nonvar(Unquoted), not((Unquoted='?';Unquoted='(';Unquoted=')')),! } .
     
var_number(A,'$VAR'(VN)):-numbered_var(A,'$VAR'(VN),_),!.
var_number(A,'$VAR'(VN)):-get_next_num(VN),assert(numbered_var(A,'$VAR'(VN),_)),!.

:-dynamic(numbered_var/3).

:-assert(var_counter(0)).

% This creates ISO Prolog getPrologVars w/in a KIF/SUMO expression to be reconstrated as after parsing is complete 

get_next_num(VN):-!,retract(var_counter(VN)),NVN is VN +1,asserta(var_counter(NVN)).

kifVarNums(LIST,'$VAR'(NUM),VAR,[=(SYM,VAR)]):-numbered_var(SYM,'$VAR'(NUM),_VAR),
               member(=(SYM,VAR),LIST).

kifVarNums(_,Atom,Atom,[]):-atomic(Atom).
kifVarNums(LIST,Term,NewTerm,VARLIST):-Term=..[F|ARGS],kifVarNums_list(LIST,ARGS,VARARGS,VARLIST),NewTerm=..[F|VARARGS].

kifVarNums_list(_LIST,[],[],[]).
kifVarNums_list(LIST,[A|RGS],[V|ARARGS],VARLIST):-
            kifVarNums(LIST,A,V,VARS1),
            kifVarNums_list(LIST,RGS,ARARGS,VARS2),
            append(VARS1,VARS2,VARLIST).



unnumbervars_nil(X,Y):-!,unnumbervars(X,Y).

collect_temp_vars(VARS):-!,(setof(=(Name,Number),numbered_var(Name,Number,_),VARS);VARS=[]).

%================================================================
% ISO-Prolog STRING TOKENIZATION                            
%================================================================
:-assert(show_this_hide(tokenize,2)).

%tokenize_chars(M,['(',surf,')']):-nonvar(M),member(34,M),!.
tokenize_chars(X,Y):-once( tokenize3(X,Y) ).

tokenize3([],[]).
tokenize3([32|T],O):-!,tokenize3(T,O),!.
tokenize3(CharList,[Token|TList])  :- 
  append(_,[C|List],CharList), C \= 32,!,
  get_token([C|List],Token,Rest),!,
  tokenize3(Rest,TList),!.

get_token(List,Token,Rest)  :- 
  get_chars_type(List,Lchars,Rest,Type),!,
  type_codes(Type,Lchars,Token),!.

type_codes(num,CODES,Num):-catch(number_codes(Num,CODES),_,fail),!.
type_codes(_,[34|Lchars],string(S)):-!,atom_codes(S,[34|Lchars]).
type_codes(_,Lchars,Token):-!,atom_codes(Token,Lchars).

get_chars_type(L,S,L1,sep)  :-  separator(L,S,L1),!.
get_chars_type([C|L],[C|Lc],L1,S)  :- 
  check_start(S,C),
  get_word_chars(S,L,Lc,L1).

get_word_chars(S,L,Lc,L1)  :- 
  check_end(S,L,Lc,L1).
get_word_chars(S,[C|L],[C|Lc],L1)  :- 
  legal_char(S,C),
  get_word_chars(S,L,Lc,L1).

legal_char(num,C)    :-  digit(C).
legal_char(quote,C)  :-  not(bracket(_,C,_)).
legal_char(symb,C)   :-  valid_char(C).

check_start(Name,S):-bracket(Name,S,_E).
check_start(num, C)   :-  start_digit(C).
check_start(symb,C)   :- valid_char(C). %, 'not'(digit(C)).

check_end(_,[],[],[])  :-  !.
check_end(num, [C|L],[],[C|L])  :-  'not'(digit(C)),!.
check_end(Name,[E|L],[E],L)  :-  bracket(Name,S,E),!.
%check_end(symb,[C1,C2|L],[],[C1,C2|L])  :-  member([C1,C2],["Fn"]),!.
check_end(symb,[C|L],[],[C|L])  :-  'not'(valid_char(C)).

separator([C,D,E,F|L],[C,D,E],L)  :-member([C,D,E,F],["SKF-"]),!.
separator([C,D,E|L],[C,D,E],L)  :-member([C,D,E],["<=>","=:=","=\\=","\\==","@=<","@>=","=..","-->","SKF"]),!.
separator([C,D|L],[C,D],L)  :-member([C,D],["=>",":-","\\+","->","\\=","==","@<","@>","=<",">=","#$","//","??"]),!. %,"Fn"
separator([C|L],[C],L)  :- member(C,"*,():[];= < >^{}?%$#/"),!.

valid_char(C)  :-  letter(C); digit(C); C = 95 ; C=45 ; C=39.
letter(C)  :-   C=45 ; (97 =< C, C =< 122) ; (65 =< C, C =< 90) ; C = 95 .
start_digit(C)   :- member(C,"-01234567890").
digit(C)   :- member(C,"-.01234567890+eE").

%get_word([C|T],C,T)  :-  member(C,":,.?&%"),!. % ( : , . ?)
get_word([C|T],[C],T)  :- member(C,"=&"),!. % (=)
get_word([C,C1|T],[C,C1],T)  :- member([C,C1],["??"]),!. %"Fn",
get_word([C|T],[C|W],T2)  :-  bracket(_,C,C1),!,get_chars(0,C1,T,W,T2).
get_word([C|T],[C|W],T2)  :-  valid_start(C),!, get_chars(1,32,T,W,T2).

get_chars(K,C1,[C|T],[C|W],T2)  :-  valid_char(K,C,C1),!,get_chars(K,C1,T,W,T2).
get_chars(0,C,[C|T],[],T)  :- bracket(_,C,_), !.
get_chars(0,C,[C|T],[C],T)  :-  (C = 41; C = 93),!. % ) or ]
get_chars(1,_C1,[C|T],[],[C|T])  :-  member(C, [10,13|"=:,?"]).
%get_chars(2,_C1,[C,C2|T],[],[C,C2|T])  :-  member([C,C2], ["Fn"]).

valid_start(C)  :-  valid(C). %; C = 37.  % (%)
valid_char(K,C,C1)  :-  K = 0,!, C \= C1; K = 1, valid(C).

%bracket(quote,39,39).  % single quotes
bracket(quote,34,34).  % double quotes
%bracket(list,91,93).  % square brackets []
%bracket(quote,37,37).  % Literal Percent %%
%bracket(quote,35,35).  % Literal Percent ##

quote_found(0,B,B)  :-  member(B,[34]),!.
quote_found(Q,Q,0).

var_found(0,B,C)  :-  'not'(valid(B)),var_start(C).

var_start(C)  :-  (65 =< C,C =< 90);C = 95;C = 39.
valid(C)  :-   (65 =< C, C =< 90);    % A - Z
             (97 =< C, C =< 122);   % a - z
             (48 =< C, C =< 57);    % 0 - 9
             C = 95; C = 39;C = 45.  % underscore; hyphen


:-assert(show_this_hide(getCleanCharsWhitespaceProper,2)).






ltrim([],[]):-!.
ltrim([32,32,32,32,32,32,32|String],Out) :-!, ltrim(String,Out),!.
ltrim([32,32,32,32,32|String],Out) :- !,ltrim(String,Out),!.
ltrim([32,32,32|String],Out) :-!, ltrim(String,Out),!.
ltrim([32,32|String],Out) :- !,ltrim(String,Out),!.
ltrim([32|String],Out) :- !,ltrim(String,Out),!.
ltrim(X,X):-!.



%:-discontiguous(conv_pred/3).
%:-discontiguous(conv_pred/3).


% ====================================================================
% conv_readS/3-4 and conv_readP/3-4 
% ====================================================================

conv_readP(RAW,OUTP,VARS):-
            conv_readS(RAW,NEWTERM,VARS),
            getSigmaTermFromSurface(NEWTERM,OUTP).

conv_readP(Stream,RAW,OUTP,VARS):-
            conv_readS(Stream,RAW,NEWTERM,VARS),
            getSigmaTermFromSurface(NEWTERM,OUTP).

chars_to_pterm(Chars,PTerm,Vars):-
         once(getSurfaceFromChars(Chars,STERM,Vars)),!,
         getSigmaTermFromSurface(STERM,PTerm),!.


getSigmaTermFromSurface([end_of_file],end_of_file):-!.
getSigmaTermFromSurface(NEWTERM,OUTP):-
               conv_kr_rule_generic(NEWTERM,NEWTERMATOMS),
	       conv_kr_rule_generic(NEWTERMATOMS,OUT),
               kif_to_pterm(OUT,OUTP).

conv_readS(RAW,NEWTERM,VARS):-!,
                once(readKIF(RRAW)),
               once(getSurfaceFromChars(RRAW,RTERM,RVARS)),
					 once((
                 (RTERM=nil,!,conv_readS(RAW,NEWTERM,VARS),!)
                 ;(once(getCleanCharsWhitespaceProper(RRAW,RAW)),NEWTERM=RTERM,VARS=RVARS)
                 )).

conv_readS(Stream,RAW,NEWTERM,VARS):-!,
                     once(readKIF(Stream,RRAW)),
                     once(getSurfaceFromChars(RRAW,RTERM,RVARS)),
					 once((
                 (RTERM=nil,!,conv_readS(Stream,RAW,NEWTERM,VARS),!)
                 ;(once(getCleanCharsWhitespaceProper(RRAW,RAW)),NEWTERM=RTERM,VARS=RVARS)
                 )).


% ====================================================================
% Sigma File IO
% ====================================================================
      
      

:-dynamic(sigma_B_seeing/3).
:-dynamic(sigma_B_telling/3).
:-asserta((sigma_B_seeing(userin,userin,1))).
:-asserta((sigma_B_telling(userin,userin,2))).
/*
sigma_B_seen:-sigma_B_seeing_console,!.
sigma_B_seen:-retract(sigma_B_seeing(FileName,LocalFile,_IOPort)),!,file_close(_IOPort).
*/
sigma_B_told:-sigma_B_telling_console,!.
sigma_B_told:-retract(sigma_B_telling(FileName,LocalFile,_IOPort)),!,file_close(_IOPort).

/*
sigma_B_see(userin):-!.
sigma_B_see(FileName):-real_kif_file_name(FileName,LocalFile),!,sigma_file_open(LocalFile,'r',_IOPort),asserta(sigma_B_seeing(FileName,LocalFile,_IOPort)).
*/

sigma_B_tell(userin):-!.
sigma_B_tell(FileName):-real_kif_file_name(FileName,LocalFile),!,sigma_file_open(LocalFile,'w',_IOPort),asserta(sigma_B_telling(FileName,LocalFile,_IOPort)).

sigma_B_get0(Char):-!,get(Char).
sigma_B_get0(Char):-sigma_B_seeing(FileName,LocalFile,_IOPort),!,file_get0(_IOPort,Char).
sigma_B_get0(Stream,OChar):-!,
            catch(file_get0(Stream,OChar),_,OChar=end_of_file).

sigma_B_get(Char):-sigma_B_seeing_console,!,get(Char).
sigma_B_get(TERM):-sigma_B_seeing(_FileName,_LocalFile,IOPort),!,file_get(IOPort,TERM,_).

sigma_B_read(TERM):-sigma_B_seeing_console,!,read(TERM).
sigma_B_read(TERM):-sigma_B_seeing(_FileName,_LocalFile,IOPort),!,file_read(IOPort,TERM,_).

sigma_B_put(Char):-sigma_B_telling_console,!,put(Char).
sigma_B_put(Char):-sigma_B_telling(FileName,LocalFile,_IOPort),!,put(_IOPort,Char).


sigma_B_seeing_console:-sigma_B_seeing(FileName,LocalFile,_IOPort),!,LocalFile=userin.
sigma_B_telling_console:-sigma_B_telling(FileName,LocalFile,_IOPort),!,LocalFile=userin.


sigma_file_open(_LocalFile,Mode,_IOPort):-file_open(_LocalFile,Mode,_IOPort),once((valid_handle(_IOPort);((ua_out(cb_error,['File Not Found',_LocalFile],_X)),!,abort))).

% file_get0(IOPort,Char)  See Platform Specifics

valid_handle('$stream'(_)):-!.
valid_handle(IOPort):- IOPort > 3,!.



