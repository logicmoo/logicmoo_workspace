% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.


:- module(get_ape_results, [
		get_ape_results_timelimit/3,
		get_ape_results_timelimit/4,
		get_ape_results/2,
		get_ape_results/3,
                get_ape_term_results/2,
                ace_to_pkif/2,
                call_ape/1,
                rename_vars/2
	]).

:- reexport('prolog/ape').
:- use_module(library(logicmoo_nlu/parser_sharing)).
:-export(rename_vars/2).
rename_vars(Content):-rename_vars(Content,RContent),ignore(Content=RContent).
rename_vars(Content0,RContent):-
        (ground(Content0)->unnumbervars(Content0,Content);Content=Content0),
        rename_vars(pass1,Content,RContent1),
        rename_vars(pass2,RContent1,RContent2),
        rename_vars(pass3,RContent2,RContent).

rename_vars(_,Var,Var):- \+ compound(Var),!.
rename_vars(_,'$VAR'(V),'$VAR'(V)):-!.
rename_vars(Pass,Content,RContent):- is_list(Content),!,maplist(rename_vars(Pass),Content,RContent).
rename_vars(Pass,Content,RContent):-   
  once(ignore(suggests_name(Pass,Content))),
  Content=..[F|Args],  
  maplist(rename_vars(Pass),Args,RArgs),
  (Args==RArgs->RContent=Content;RContent=..[F|RArgs]),!.
rename_vars(_,Content,Content).

% DRS
name_in_arg(pass1,object/6,2-1,'_OBJ').
name_in_arg(pass1,predicate/3,2-1,'_FRAME').
name_in_arg(pass2,property/3,2-1,'_REL').
name_in_arg(pass3,modifier_pp/3,2-1,'_MOD').

% FOL
name_in_arg(pass1,object/7,3-2,'_OBJ').
name_in_arg(pass1,predicate/4,3-2,'_EVENT').
name_in_arg(pass2,property/4,3-2,'_PROP').
name_in_arg(pass3,modifier_pp/4,3-2,'_MOD').
name_in_arg(pass3,predicate/4,3-1,'_FRAME').

name_in_arg(pass2,predicate/5,3-2,'_EVENT').
name_in_arg(pass3,predicate/5,3-1,'_FRAME').

% TPTP
name_in_arg(pass2,predicate1/3,2-1,'_EVENT').
name_in_arg(pass2,predicate2/_,2-1,'_PRED2').
name_in_arg(pass2,property1/3,2-1,'_PROP').
name_in_arg(pass3,modifier_pp/4,3-2,'_MOD').


suggests_name(_Pass,G):-  (\+ compound(G) ; ground(G)).
suggests_name(_Pass,G):- G=..[Name,Var], maybe_name_var(Var,Name,'_OBJ'),!.
suggests_name(Pass,G):- functor(G,F,A),name_in_arg(Pass,F/A,NameArg-VarArg,SUFFIX),arg(NameArg,G,Name),arg(VarArg,G,Var),maybe_name_var(Var,Name,SUFFIX),!.
suggests_name(_Pass,_).

maybe_name_var('$VAR'(Var),Name,CAT):-atom(Name),ignore(upcase_atom(Name,VAR)),atom_concat(VAR,CAT,Var),!.
maybe_name_var(_,_,_).

:-export(fol_to_pkif/2).
fol_to_pkif(FOL,PKIF):- transitive(fol_to_kif,FOL,KIF),kif_to_pkif(KIF,PKIF),!.

:-export(ace_to_pnf/2).
ace_to_pnf(ACE,FOL):-  must_det_l((get_ape_term_results(ACE,PROPS),transitive(props_to_pnf,PROPS,PNF),v_pp(PNF,FOL))),!.

:-export(ace_to_pkif/2).
ace_to_pkif(ACE,PKIF):-  must_det_l((ace_to_pnf(ACE,FOL),fol_to_pkif(FOL,PKIF0),fully_expand(PKIF0,PKIF))),!.

props_to_pnf(PROPS,PNF):-member(drs=drs([],[]),PROPS),!,member(tokens=Tokens,PROPS),!,PNF=isEng2KifFn(ftAssertable,Tokens),!.
props_to_pnf(PROPS,PNF):-member(pnf=PNF,PROPS),!.
props_to_pnf(PROPS,PNF):-member(fol=PNF,PROPS),!.
props_to_pnf(PROPS,PNF):-member(drs=PNF,PROPS),!.
props_to_pnf(PROPS,PNF):-member(tptp=PNF,PROPS),!.


ace_i_name(A,Plur,AT):-atom(Plur),talkdb:talk_db(noun1,Sing,Plur),if_defined(i_name(A,Sing,AT)),!.
ace_i_name(A,T,AT):-atom(T),if_defined(i_name(A,T,AT)),!.
ace_i_name(_A,T,AT):-AT=T.

%:- talkdb:load_language_file(pldata(talk_db_pdat)).

:-export(fol_to_kif/2).

                           
fol_to_kif(FOL, FOL) :- var(FOL),!.
fol_to_kif($true, is_true) :-!.
fol_to_kif(FOL, FOL) :- (\+ (compound(FOL))) ,!.
fol_to_kif('$VAR'(O), '$VAR'(O)):-!.
fol_to_kif([FH|FT],[KH|KT]) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif([FH|FT],(KH,KT)) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif(t(P,A),C) :- atom(P),!,C=..[P,A],!.
fol_to_kif(t(P,A,B),C) :- atom(P),!,C=..[P,A,B],!.
fol_to_kif(object(_LOVE_FRAME,PERSON_OBJ,Person,Countable,Na,Eq,One),object(PERSON_OBJ,Person,Countable,Na,Eq,One)).
fol_to_kif(predicate(_LOVE_FRAME,LOVE_EVENT,Love,PERSON_OBJ,ANIMAL_OBJ),predicate(LOVE_EVENT,Love,PERSON_OBJ,ANIMAL_OBJ)).
fol_to_kif(property(_LOVE_FRAME, PERSON_OBJ, Person, Pos),property(PERSON_OBJ, Person, Pos)).

fol_to_kif(object(LOVE_FRAME,PERSON_OBJ,Person,countable,na,eq,1),ist(LOVE_FRAME,t(PersonSym,PERSON_OBJ))):-!,ace_i_name(t,Person,PersonSym).
fol_to_kif(object(LOVE_FRAME,PERSON_OBJ,Person,Countable,_Na,Eq,One),(ist(LOVE_FRAME,t(Countable,PERSON_OBJ,Eq,One)),ist(LOVE_FRAME,t(PersonSym,PERSON_OBJ)))):-!,ace_i_name(t,Person,PersonSym).
fol_to_kif(property(LOVE_FRAME, PERSON_OBJ, Person, pos),(ist(LOVE_FRAME,t(PersonSym,PERSON_OBJ)))):-!,ace_i_name(t,Person,PersonSym).


fol_to_kif(object(PERSON_OBJ,Person,countable,na,eq,1),t(PersonSym,PERSON_OBJ)):-!,ace_i_name(t,Person,PersonSym).
fol_to_kif(property(PERSON_OBJ, Person, pos),t(PersonSym,PERSON_OBJ)):-!,ace_i_name(v,Person,PersonSym).
fol_to_kif(property(PERSON_OBJ, Person, neg), '-'(t(PersonSym,PERSON_OBJ))):-!,ace_i_name(v,Person,PersonSym).



fol_to_kif(object(ANIMAL_OBJ, Animal, countable, na, eq, 1),t(AnimalSym,ANIMAL_OBJ)):-!,ace_i_name(t,Animal,AnimalSym).
fol_to_kif(object(PERSON_OBJ,Person,Countable,_Na,Eq,One),(t(Countable,PERSON_OBJ,Eq,One),t(PersonSym,PERSON_OBJ))):-!,ace_i_name(t,Person,PersonSym).
fol_to_kif(predicate(LOVE_FRAME,LOVE_EVENT,Love,PERSON_OBJ,ANIMAL_OBJ),(holdsIn(LOVE_FRAME,LOVE_EVENT),frame(LOVE_FRAME),ist(LOVE_EVENT,t(LoveSym,PERSON_OBJ,ANIMAL_OBJ)))):-!,ace_i_name(mud,Love,LoveSym).
fol_to_kif(predicate(Love,PERSON_OBJ,ANIMAL_OBJ),(t(LoveSym,PERSON_OBJ,ANIMAL_OBJ))):-!,ace_i_name(mud,Love,LoveSym).
fol_to_kif(predicate(_LOVE_FRAME, Love, PERSON_OBJ, ANIMAL_OBJ),t(LoveSym, PERSON_OBJ, ANIMAL_OBJ)):-!,ace_i_name(mud,Love,LoveSym).

fol_to_kif((O - N/M), K):- integer(N),integer(M), !,fol_to_kif(O,K).
fol_to_kif('=>'(FH,FT),implies(KH,KT)) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif('implies'(FH,FT),implies(KH,KT)) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif('&'(FH,FT),(KH,KT)) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif('v'(FH,FT),(KH;KT)) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif('-'(FT),'-'(KT)) :- !, fol_to_kif(FT,KT).
fol_to_kif('~'(FT),'-'(KT)) :- !, fol_to_kif(FT,KT).
fol_to_kif(exists(FH,FT),exists(FH,KT)) :- !,fol_to_kif(FT,KT).
fol_to_kif(all(FH,FT),all(FH,KT)) :- !,fol_to_kif(FT,KT).
fol_to_kif(drs([],FT),CNF) :- !,fol_to_kif(FT,KT),list_to_conjuncts(KT,CNF).
fol_to_kif(drs(FH,FT),exists(FH,CNF)) :- !,fol_to_kif(FT,KT),list_to_conjuncts(KT,CNF).
fol_to_kif(forall(FH,FT),all(FH,KT)) :- !,fol_to_kif(FT,KT).
fol_to_kif(('?'(VARS : FOL)),exists(VARS,KIF)):-!,fol_to_kif(FOL,KIF).
fol_to_kif('!'(VARS:FOL),forall(VARS,KIF)):-!,fol_to_kif(FOL,KIF).
fol_to_kif('!'(VARS):FOL,forall(VARS,KIF)):-!,fol_to_kif(FOL,KIF).

fol_to_kif([FH|FT],[KH|KT]) :- !, fol_to_kif(FH,KH),fol_to_kif(FT,KT).
fol_to_kif(FH,KH) :- FH=..[F|FT], fol_to_kif(FT,KT),!,KH=..[F|KT].


:-export(kif_to_pkif/2).
kif_to_pkif(FOL, FOL) :- \+ compound(FOL),!.
kif_to_pkif('$VAR'(O), '$VAR'(O)):-!.
kif_to_pkif(exists(FH,FT),exists(FH,KT)) :- !,kif_to_pkif(FT,KT).
kif_to_pkif(forall(FH,FT),all(FH,KT)) :- !,kif_to_pkif(FT,KT).
kif_to_pkif([FH|FT],[KH|KT]) :- !, kif_to_pkif(FH,KH),kif_to_pkif(FT,KT).
kif_to_pkif(FH,KH) :- FH=..[F|FT], kif_to_pkif(FT,KT),!,KH=..[F|KT].

%% get_ape_term_results(+Input:list, -Content:atom) is det.
%  get_ape_term_results(+Input:list, -ContentType:atom, -Content:atom) is det.
%
% @param Input is a list of input parameters of the form Key=Value
% @param ContentType is one of {text/plain, text/xml}
% @param Content is the returned XML
%

get_ape_term_results(InputTok, RContent) :- is_list(InputTok), flatten(InputTok,InputTokF),!,    
  delete(InputTokF,(^),InputTokS),concat_atom(InputTokS,' ',InputStr),!,get_ape_term_results(InputStr, RContent),!.
get_ape_term_results(Input, Content) :-  get_ape_results([text=Input,guess=on], _ContentType, Content).


:-export(get_ape_term_results/1).
get_ape_term_results(Text):- get_ape_term_results(Text,L),forall(member(K=V,L),(v_pp(V,P),portray_clause(K:-P))).


v_pp(V,V):-var(V),!.
v_pp([V],P):-nonvar(V),v_pp(V,P).
v_pp(fof(Axiom,V),fof(Axiom,P)):-!,v_pp(V,P).
v_pp(A:V,exists(A,P)):-!,v_pp(V,P).
v_pp(V,V):-!.

call_ape(G):- G.

end_of_file.


:- shared_parser_data(talkdb:talk_db/3).

/** <module> Interface for the ACE tools (ACE parser, DRS verbalizer, ...)

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-05-13

Usage with multiple results returned (i.e. multi-mode):

==
get_ape_results([text='Every man waits.', cparaphrase1=on], ContentType, Content).
get_ape_results([text='Every man waits.', cparaphrase=on, cparaphrase1=on], ContentType, Content).
get_ape_results([text='A man waits.', cinput=on, cdrs=on, cdrspp=on, cparaphrase=on, cparaphrase1=on, cparaphrase2=on, ctokens=on, csyntax=on, csyntaxpp=on, cfol=on], ContentType, Content).
==

Usage with a single result returned (i.e. solo-mode):

==
get_ape_results([text='A man sees a dog.', solo=drs], ContentType, Content).
get_ape_results([text='Every man owns a dog.', solo=owlrdf], ContentType, Content).
get_ape_results([text='Peeter likes Mary.', solo=owlfss], ContentType, Content).
==

@tbd: provide all outputs
	(1) as serialized Prolog terms,
	(2) as pretty-printed terms,
	(3) as XML, JSON, HTML, etc.
*/

% Default encoding used for opening files in text mode.
:- set_prolog_flag(encoding, utf8).

% Note: The following line fixes the testcase: 3.1415926536 approximates Pi.
% Note that using 'f' instead of 'g' does not drop the trailing zeros.
% @bug: sometimes weird digits are attached to the end.
:- set_prolog_flag(float_format, '%.11g').

% Richard A. O'Keefe: to see full precision for IEEE doubles, do
% :- set_prolog_flag(float_format, '%.18g').
% So it seems that values above 18 do not make sense.

:- prolog_load_context(directory,Dir),assert(user:file_search_path(ape,Dir)).


:- use_module(ape('utils/morphgen'), [
		acesentencelist_pp/2
	]).

:- use_module(ape('utils/ace_niceace'), [
		tokens_to_sentences/2
	]).

:- use_module(ape('utils/drs_to_xml')).
:- use_module(ape('utils/drs_to_fol_to_prenex')).
:- use_module(ape('utils/drs_to_ascii')).
:- use_module(ape('utils/drs_to_ace')).
:- use_module(ape('utils/drs_to_coreace')).
:- use_module(ape('utils/drs_to_npace')).
:- use_module(ape('utils/drs_to_html')).
:- use_module(ape('utils/drs_to_ruleml')).
:- use_module(ape('utils/tree_utils')).
:- use_module(ape('utils/trees_to_ascii')).
:- use_module(ape('utils/drs_to_tptp')).
:- use_module(ape('lexicon/clex')).
:- use_module(ape('lexicon/ulex')).
:- use_module(ape('parser/ace_to_drs')).
:- use_module(ape('logger/error_logger')).

:- use_module(ape('utils/xmlterm_to_xmlatom'), [
		xmlterm_to_xmlatom/2,
		xmlterm_to_xmlatom/3
	]).

:- use_module(ape('utils/serialize_term'), [
		serialize_term_into_atom/2
	]).

:- use_module(ape('utils/owlswrl/get_owl_output'), [
		get_owl_output/7
	]).


%% get_ape_results_timelimit(+Input:list, -Content:atom, +TimeLimit) is det.
%% get_ape_results_timelimit(+Input:list, -ContentType:atom, -Content:atom, +TimeLimit) is det.
%
% There is call_with_time_limit(+Time, :Goal) defined in library(time),
% part of clib package. On timeout this throws the exception time_limit_exceeded.
% But we catch other exceptions as well...
%
% @param Input is a list of input parameters of the form Key=Value
% @param ContentType is one of {text/plain, text/xml}
% @param Content is the returned result
% @param TimeLimit the timelimit in seconds
%
get_ape_results_timelimit(Input, Content, TimeLimit) :-
	get_ape_results_timelimit(Input, _ContentType, Content, TimeLimit).

get_ape_results_timelimit(Input, ContentType, Content, TimeLimit) :-
	catch(
		call_with_time_limit(
			TimeLimit,
			get_ape_results(Input, ContentType, Content)
		),
		CatchType,
		catchtype_errormessage(CatchType, ContentType, Content)
	).


%% catchtype_errormessage(+CatchType:atom, -ContentType:atom, -ErrorMessage:atom) is det.
%
% Returns an error message as XML. The error message is set by catch/3,
% this predicate just formats the message.
% This is a very toplevel error message indicating either that the time limit
% was exceeded or that there were resource errors (out of stack, etc.) or programmer
% errors (undefined predicate called, etc.).
%
% @param CatchType is one of {time_limit_exceeded, ...}
% @param ContentType is always 'text/xml'
% @param ErrorMessage is an end-user-level message that corresponds to the CatchType
%
catchtype_errormessage(
	time_limit_exceeded,
	'text/xml',
	'<apeResult><messages><message importance="error" type="ws" sentence="" token="" value="time_limit_exceeded"
repair="Split the text into smaller parts and parse them separately."/></messages></apeResult>'
	) :- !.

catchtype_errormessage(CatchType, 'text/xml', ErrorMessage) :-
	format(atom(CatchTypeAsAtom), "~w", [CatchType]),
	xmlterm_to_xmlatom(element(apeResult, [], [
			element(messages, [], [
				element(message, [
					importance='error',
					type='ws',
					sentence='',
					token='',
					value=CatchTypeAsAtom,
					repair='Fatal error. Please send screenshot to APE developers.'
					], [])
			])
		]), ErrorMessage).


get_ape_term_results(InputStr, RContent) :-
   Input = [text = InputStr , cinput=on, cdrs=on,  cparaphrase=on, 
      cparaphrase1=on, cparaphrase2=on, ctokens=on, guess=on, csyntax=on, cfol=on, cpnf=on, ctptp=on, cowlfss=on, cowlrdf = on],
	clear_messages,
	init_clex(Input),
	load_ulex(Input),
	get_value(Input, text, ACEText),
	get_value(Input, guess, GuessOnOff),
	acetext_to_drs00(ACEText, GuessOnOff, off, TokensSentences, Syntax, Drs, _Messages, [DT, DP, DR]),
	get_value(Input, uri, Uri, 'http://attempto.ifi.uzh.ch/ontologies/owlswrl/test'),
	TempResult = [
			time=[tokenizer=DT, parser=DP, refres=DR],
			acetext=ACEText,
			tokens=TokensSentences,
			drs=Drs,
			syntax=Syntax,
			uri=Uri                        
		],
       % rename_vars(Drs),
	findall(PrologTerm, get_ape_prolog_term_result(Input, TempResult, PrologTerm), OutputElements),
	get_messages(Messages),!,
        % aline PNF and FOL (if possible)
        ignore((
           get_value(OutputElements, fol, FO),
           get_value(OutputElements, pnf, FO)
        )),        
        append(TempResult,[messages= Messages|OutputElements],Content),!,
        rename_vars(Content,RContent).

acetext_to_drs00(Text, GuessOnOff, CatchOnOff, TokensSentences, Syntax, Drs, Messages, [DT, TimeP, TimeR]) :-
	ape_utils:cpu_time(tokenizer:tokenize(Text, TokensInner), T),
	ace_to_drs:tokens_to_paragraphs(TokensInner, Paragraphs),
	ace_to_drs:paragraphs_to_drs(Paragraphs, GuessOnOff, CatchOnOff, 1 /*NewStartID*/ , TokensSentences, Syntax, Drs, Messages, [TimeTPre, TimeP, TimeR]),
	clear_messages,
	add_messages(Messages),
	DT is T + TimeTPre,
	!.

get_ape_prolog_term_result(Input, TempResult, Type=PrologTerm) :-
	output_type(Type),
	atom_concat('c', Type, Key),
	once(memberchk(Key=on, Input);memberchk(Type=on, Input);memberchk(Type, Input)),
	ignore((catch(
		get_output(Type, TempResult, _, PrologTerm, Output),
		Catcher,
		(Output = '', PrologTerm = Catcher)
	))).


get_ape_results(Input, ContentType, Content) :-
	clear_messages,
	init_clex(Input),
	load_ulex(Input),
	get_value(Input, text, ACEText),
	get_value(Input, guess, GuessOnOff),
	acetext_to_drs(ACEText, GuessOnOff, on, Tokens, Syntax, Drs, _Messages, [DT, DP, DR]),
	get_value(Input, uri, Uri, 'http://attempto.ifi.uzh.ch/ontologies/owlswrl/test'),
	TempResult = [
			time=[DT, DP, DR],
			acetext=ACEText,
			tokens=Tokens,
			drs=Drs,
			syntax=Syntax,
			uri=Uri
		],
	get_content(Input, TempResult, ContentType, Content),
	!.

% @bug: This is sometimes called, but we should know why.
% It must be here, just to catch errors.
get_ape_results(_, 'text/plain', '').


%% output_type(?Type:atom)
%
% This predicate defines the supported output types and the order of the outputs for the multi mode.
%
output_type(input).
output_type(tokens).
output_type(sentences).
output_type(drs).
output_type(drsrt).
output_type(syntax).
output_type(syntaxpp).
output_type(syntaxd).
output_type(syntaxdpp).
output_type(drspp).
output_type(drsxml).
output_type(drshtml).
output_type(paraphrase).
output_type(paraphrase1).
output_type(paraphrase2).
output_type(owlrdf).
output_type(owlfss).
output_type(owlfsspp).
output_type(owlxml).
output_type(ruleml).
output_type(fol).
output_type(pnf).
output_type(tptp).


%% get_content(+Input:list, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
%
get_content(Input, TempResult, ContentType, Content) :-
    member(solo=SoloType, Input),
	!,
	get_solo_content(SoloType, TempResult, ContentType, Content).

get_content(Input, TempResult, ContentType, Content) :-
	get_multi_content(Input, TempResult, ContentType, Content).


%% get_solo_content(+SoloType:atom, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
% @param SoloType defines which output should be returned
% @param TempResult is a list of outputs from the parser
% @param ContentType is one of {text/xml, text/plain}
% @param Content is the result (e.g. a syntax tree or a paraphrase) in XML or plain text

% If there are APE error messages
% then we print the messages and not the solo output.
% Note: we do not care about the warning messages.
get_solo_content(_, _TempResult, 'text/xml', Content) :-
	get_error_messages([M | ErrorMessages]),
	!,
	messages_xmlmessages([M | ErrorMessages], XmlErrorMessages),
	xmlterm_to_xmlatom(element(messages, [], XmlErrorMessages), Content).

get_solo_content(SoloType, TempResult, ContentType, Content) :-
	output_type(SoloType),
	get_output(SoloType, TempResult, OutputContentType, OutputContent),
	(
		get_error_messages([M | ErrorMessages])
	->
		messages_xmlmessages([M | ErrorMessages], XmlErrorMessages),
		xmlterm_to_xmlatom(element(messages, [], XmlErrorMessages), Content),
		ContentType = 'text/xml'
	;
		Content = OutputContent,
		ContentType = OutputContentType
	).

get_solo_content(SoloType, _, 'text/plain', 'ERROR: Unexpected error.') :-
	output_type(SoloType),
	!.

get_solo_content(_, _, 'text/plain', 'ERROR: Wrong solo type.').


%% get_multi_content(+Input:list, +TempResult:list, -ContentType:atom, -Content:atom) is det.
%
% @bug: we ignore the parser+refres messages, and get a fresh list
% of messages from the messages repository (it will include the parser+refres
% messages anyway).
%
get_multi_content(Input, TempResult, 'text/xml', Content) :-
    get_value(TempResult, time, [DTokenizer, DParser, DRefres]),
	with_output_to(atom(DT), format("~3f", [DTokenizer])),
	with_output_to(atom(DP), format("~3f", [DParser])),
	with_output_to(atom(DR), format("~3f", [DRefres])),
	findall(OutputElement, get_multi_output_element(Input, TempResult, OutputElement), OutputElements),
	get_messages_in_xml(Messages),
	append([element(duration, [tokenizer=DT, parser=DP, refres=DR], [])|OutputElements], [element(messages, [], Messages)], Elements),
	xmlterm_to_xmlatom(element(apeResult, [], Elements), [header(true)], Content),!.


%% get_multi_output_element(+Input:list, +TempResult:list, -Content:element)
%
% Returns one output element if it is required by the input in multi mode. It succeeds as many times as there
% are such multi typed outputs.
%
% In case an exception is thrown during the generation of the output,
% then an empty atom is returned as the output.
%
get_multi_output_element(Input, TempResult, element(Type, [], [Output])) :-
	output_type(Type),
	atom_concat('c', Type, Key),
	memberchk(Key=on, Input),
	catch(
		get_output(Type, TempResult, _, _PrologTerm,  Output),
		_Catcher,
		Output = ''
	).


%% get_output(+OutputType:atom, +TempResult:list, -ContentType:atom, -Prolog:term, -Content:atom)
%
% Returns the required output as an atom. This is used for both modes, solo and multi.
%
get_output(input, TempResult, 'text/plain', Output, Output) :-
    get_value(TempResult, acetext, Output),
    !.

get_output(tokens, TempResult, 'text/plain', Tokens, Output) :-
    get_value(TempResult, tokens, Tokens),
	serialize_term_into_atom(Tokens, Output),
    !.

get_output(sentences, TempResult, 'text/plain', Sentences, Output) :-
    get_value(TempResult, tokens, Tokens),
	tokens_to_sentences(Tokens, Sentences),
	serialize_term_into_atom(Sentences, Output),
    !.

get_output(syntax, TempResult, 'text/plain', Syntax, Output) :-
    get_value(TempResult, syntax, Syntax1),
	unsplit_pronouns_in_tree(Syntax1, Syntax2),
	remove_gaps_in_tree(Syntax2, Syntax3),
	unify_coords_in_tree(Syntax3, Syntax),
	serialize_term_into_atom(Syntax, Output),
    !.

get_output(syntaxpp, TempResult, 'text/plain', Syntax, Output) :-
    get_value(TempResult, syntax, Syntax1),
	unsplit_pronouns_in_tree(Syntax1, Syntax2),
	remove_gaps_in_tree(Syntax2, Syntax3),
	unify_coords_in_tree(Syntax3, Syntax),
	trees_to_ascii:trees_to_ascii(Syntax, Output),
    !.

get_output(syntaxd, TempResult, 'text/plain',Syntax, Output) :-
    get_value(TempResult, syntax, Syntax),
	serialize_term_into_atom(Syntax, Output),
    !.

get_output(syntaxdpp, TempResult, 'text/plain', Syntax, Output) :-
    get_value(TempResult, syntax, Syntax),
	trees_to_ascii:trees_to_ascii(Syntax, Output),
    !.

get_output(fol, TempResult, 'text/plain', Fol, Output) :-
    get_value(TempResult, drs, Drs),
	drs_fol_pnf:drs_fol(Drs, Fol),
	serialize_term_into_atom(Fol, Output),
    !.

get_output(pnf, TempResult, 'text/plain', Pnf, Output) :-
    get_value(TempResult, drs, Drs),
	drs_fol_pnf:drs_pnf(Drs, Pnf),
	serialize_term_into_atom(Pnf, Output),
    !.

get_output(paraphrase, TempResult, 'text/plain', Sentences, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ace:drs_to_ace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(paraphrase1, TempResult, 'text/plain', Sentences, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_coreace:bigdrs_to_coreace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(paraphrase2, TempResult, 'text/plain', Sentences, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_npace:drs_to_npace(Drs, Sentences),
	acesentencelist_pp(Sentences, Output),
    !.

get_output(ruleml, TempResult, 'text/xml', [Ruleml], Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ruleml:drs_to_ruleml(Drs, Ruleml),
	xmlterm_to_xmlatom([Ruleml], Output),
    !.

get_output(drs, TempResult, 'text/plain', Drs, Output) :-
    get_value(TempResult, drs, Drs),
	serialize_term_into_atom(Drs, Output),
    !.

get_output(drspp, TempResult, 'text/plain', Drs, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_ascii:drs_to_ascii(Drs, Output),
    !.

get_output(tptp, TempResult, 'text/plain', TptpList, Output) :-
	get_value(TempResult, drs, Drs),
	drs_to_tptp:drs_to_tptplist(Drs, TptpList),
	with_output_to(atom(Output), tptplist_pp(TptpList)),
    !.

get_output(drsxml, TempResult, 'text/xml', Drs, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_xmlatom(Drs, Output),
    !.

get_output(drshtml, TempResult, 'text/xml',Drs, Output) :-
    get_value(TempResult, drs, Drs),
	drs_to_html:drs_to_html(Drs, Output),
    !.

get_output(OwlOutputType, TempResult, ContentType, PrologTerm, Output) :-
	get_value(TempResult, acetext, AceText),
	get_value(TempResult, drs, Drs),
	get_value(TempResult, uri, Uri),
	get_owl_output(OwlOutputType, AceText, Drs, Uri, ContentType, PrologTerm, Output),
	!.


%% init_clex(+Input:list) is det.
%
%
init_clex(Input) :-
	get_value(Input, noclex, on),
	!,
	set_clex_switch(off).

init_clex(_) :-
	set_clex_switch(on).


%% load_ulex(+Input:list) is det.
%
%
load_ulex(Input) :-
	get_value(Input, ulextext, Ulex),
	!,
	discard_ulex,
    atom_to_memory_file(Ulex, UlexHandle),
    open_memory_file(UlexHandle, read, UlexStream),
    read_ulex(UlexStream),
    free_memory_file(UlexHandle).

load_ulex(_).


%% get_value(+Map:list, +Key:atom, -Value:atom, +Default:atom) is det.
%
% Returns the value for a key from a map of key/value pairs. The default value is returned if
% the key is not found.
%
% @param Map is a list key/value pairs of the form Key=Value
% @param Key is the key to look for
% @param Value is the value that is returned
% @param Default is the default value for the case that the key is not found
%
get_value(Map, Key, Value, _Default) :-
	memberchk(Key=Value, Map),
	!.

get_value(_, _, Default, Default).


%% get_value(+Map:list, +Key:atom, -Value:atom) is det.
%
% The same as get_value/4 with '' as default value
%
get_value(Map, Key, Value) :-
	get_value(Map, Key, Value, '').
