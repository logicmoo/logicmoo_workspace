:-module(parser_penn_trees, 
[
 is_word_or_span/1,
 sort_words/2,
 tree_to_lexical_segs/2,
 segs_retain_w2/3,
 into_segs/2,
 text_to_best_tree/2,
 penn_tree_to_segs/3,
 lxpr_to_segs/2,
 lxpr_to_list/2]).
/*

S - simple declarative clause, i.e. one that is not introduced by a (possible empty) subordinating conjunction or a wh-word and that does not exhibit subject-verb inversion.
SBAR - Clause introduced by a (possibly empty) subordinating conjunction.
SBARQ - Direct question introduced by a wh-word or a wh-phrase. Indirect questions and relative clauses should be bracketed as SBAR, not SBARQ.
SINV - Inverted declarative sentence, i.e. one in which the subject follows the tensed verb or modal.
SQ - Inverted yes/no question, or main clause of a wh-question, following the wh-phrase in SBARQ.

Phrase Level
ADJP - Adjective Phrase.
ADVP - Adverb Phrase.
CONJP - Conjunction Phrase.
FRAG - Fragment.
INTJ - Interjection. Corresponds approximately to the part-of-speech tag UH.
LST - List marker. Includes surrounding punctuation.
NAC - Not a Constituent; used to show the scope of certain prenominal modifiers within an NP.
NP - Noun Phrase.
NX - Used within certain complex NPs to mark the head of the NP. Corresponds very roughly to N-bar level but used quite differently.
PP - Prepositional Phrase.
PRN - Parenthetical.
PRT - Particle. Category for words that should be tagged RP.
QP - Quantifier Phrase (i.e. complex measure/amount phrase); used within NP.
RRC - Reduced Relative Clause.
UCP - Unlike Coordinated Phrase.
VP - Vereb Phrase.
WHADJP - Wh-adjective Phrase. Adjectival phrase containing a wh-adverb, as in how hot.
WHAVP - Wh-adverb Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing a wh-adverb such as how or why.
WHNP - Wh-noun Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing some wh-word, e.g. who, which book, whose daughter, none of which, or how many leopards.
WHPP - Wh-prepositional Phrase. Prepositional phrase containing a wh-noun phrase (such as of which or by whose authority) that either introduces a PP gap or is contained by a WHNP.
X - Unknown, uncertain, or unbracketable. X is often used for bracketing typos and in bracketing the...the-constructions.

CC	coordinating conjunction	and, but, or, 
 & therefore times v. versus vs. whether yet
CC conjunction, coordinating
CD	cardinal number	1, three numeral,   mid-1890 nine-thirty forty-two one-tenth ten million 0.5 one forty-seven 1987 twenty '79 zero two 78-degrees eighty-four IX '60s .025 fifteen 271,124 dozen quintillion DM2,000 ...
DT	determiner	the  all an another any both del each either every half la many much nary neither no some such that the them these this those
EX	existential there	there is
FW	foreign word	d'œuvre
IN	preposition/subord. conj.	in,of,like,after,whether
IN preposition or conjunction, subordinating     astride among uppon whether out inside pro despite on by throughout below within for towards near behind atop around if like until below next into if beside ... 
IN/that	complementizer	that
JJ	adjective	green or numeral, ordinal     third ill-mannered pre-war regrettable oiled calamitous first separable ectoplasmic battery-powered participatory fourth still-to-be-named multilingual multi-disciplinary ...
JJR	adjective, comparative	greener  bleaker braver breezier briefer brighter brisker broader bumper busier calmer cheaper choosier cleaner clearer closer colder commoner costlier cozier creamier crunchier cuter ...
JJS	adjective, superlative	greenest calmest cheapest choicest classiest cleanest clearest closest commonest corniest costliest crassest creepiest crudest cutest darkest deadliest dearest deepest densest dinkiest ...
LS	list marker	(1),
LS list item marker     A A. B B. C C. D E F First G H I J K One SP-44001 SP-44002 SP-44005
MD	modal	could, will
MD modal auxiliary     can cannot could couldn't dare may might must need ought shall should shouldn't will would
Missing: '', (, ), ,, --, ., FW, NNPS, SYM, WP$, 
NN	noun, singular or mass	table
NN noun, common, singular or mass     common-carrier cabbage knuckle-duster Casino afghan shed thermostat investment slide humour falloff slick wind hyena override subhumanity machinist ...
NNP noun, proper, singular     Motown Venneboerger Czestochwa Ranzer Conchita Trumplane Christos Oceanside Escobar Kreisler Sawyer Cougar Yvette Ervin ODI Darryl CTCA Shannon A.K.C. Meltex Liverpool ...
NNS	noun plural	tables
NNS noun, common, plural     undergraduates scotches bric-a-brac products bodyguards facets coasts divestitures storehouses designs clubs fragrances averages subjectivists apprehensions muses factory-jobs ...
NP	proper noun, singular	John
NPS	proper noun, plural	Vikings
PDT	predeterminer	both the boys
PDT pre-determiner     all both half many quite such sure this
POS	possessive ending	friend's
POS genitive marker     ' 's
POS Tag	Description	Example
PP	personal pronoun	I, he, it
PP$	possessive pronoun	my, his
PRP pronoun, personal     hers herself him himself hisself it itself me myself one oneself ours ourselves ownself self she thee theirs them themselves they thou thy us
PRP$ pronoun, possessive     her his mine my our ours their thy your
RB	adverb	however, usually, here, not occasionally unabatingly maddeningly adventurously professedly stirringly prominently technologically magisterially predominately swiftly fiscally pitilessly ...
RBR	adverb, comparative	better further gloomier grander graver greater grimmer harder harsher healthier heavier higher however larger later leaner lengthier less- perfectly lesser lonelier longer louder lower more ...
RBS	adverb, superlative	best biggest bluntest earliest farthest first furthest hardest heartiest highest largest least less most nearest second tightest worst
RP	particle	give up  aboard about across along apart around aside at away back before behind by crop down ever fast for forth from go high i.e. in into just later low more off on open out over per pie raising start teeth that through under unto up up-pp upon whole with you 
SENT	end punctuation	?, !, .
SP-44007 Second Third Three Two * a b c d first five four one six three two
SYM	symbol	@, +, *, ^, |, =

TO "to" as preposition or infinitive marker     to  to	to go, to him
UH	interjection	uhhuhhuhh
UH interjection     Goodbye Goody Gosh Wow Jeepers Jee-sus Hubba Hey Kee-reist Oops amen huh howdy uh dammit whammo shucks heck anyways whodunnit honey golly man baby diddle hush sonuvabitch ...
VB	verb be, base form	be
VB verb, base form     ask assemble assess assign assume atone attention avoid bake balkanize bank begin behold believe bend benefit bevel beware bless boil bomb boost brace break bring broil brush build ... 
VBD	verb be, past	was|were
VBD verb, past tense     dipped pleaded swiped regummed soaked tidied convened halted registered cushioned exacted snubbed strode aimed adopted belied figgered speculated wore appreciated contemplated ...
VBG	verb be, gerund/participle	being
VBG verb, present participle or gerund     telegraphing stirring focusing angering judging stalling lactating hankerin' alleging veering capping approaching traveling besieging encrypting interrupting erasing wincing ...
VBN	verb be, past participle	been
VBN verb, past participle     multihulled dilapidated aerosolized chaired languished panelized used experimented flourished imitated reunifed factored condensed sheared unsettled primed dubbed desired ...
VBP	verb be, pres non-3rd p.	am|are
VBP verb, present tense, not 3rd person singular     predominate wrap resort sue twist spill cure lengthen brush terminate appear tend stray glisten obtain comprise detest tease attract emphasize mold postpone sever return wag ...
VBZ	verb be, pres, 3rd p. sing	is
VBZ  verb, present tense, 3rd person singular     bases reconstructs marks mixes displeases seals carps weaves snatches slumps stretches authorizes smolders pictures emerges stockpiles seduces fizzes uses bolsters slaps speaks pleads ...
VD	verb do, base form	do
VDD	verb do, past	did
VDG	verb do gerund/participle	doing
VDN	verb do, past participle	done
VDP	verb do, pres, non-3rd per.	do
VDZ	verb do, pres, 3rd per.sing	does
VH	verb have, base form	have
VHD	verb have, past	had
VHG	verb have, gerund/participle	having
VHN	verb have, past participle	had
VHP	verb have, pres non-3rd per.	have
VHZ	verb have, pres 3rd per.sing	has
VV	verb, base form	take
VVD	verb, past tense	took
VVG	verb, gerund/participle	taking
VVN	verb, past participle	taken
VVP	verb, present, non-3rd p.	take
VVZ	verb, present 3d p. sing.	takes
WDT	wh-determiner	 that what whatever which whichever
WP	wh-pronoun	who, what
WP WH-pronoun     that what whatever whatsoever which who whom whosoever
WP$	possessive wh-pronoun	whose
WRB	wh-abverb	where, when how however whence whenever where whereby whereever wherein whereof why
$	currency symbol	$, £
& 'n and both but either et for less minus neither nor or plus so
:	general joiner	;, -, --

*/
:- set_module(class(library)).
:- set_module(base(system)).


check_tree_quality(Srch,X,G1,G2,Total):-findall(Srch,(sub_term(Srch,X),G1,G2),Sols),length(Sols,Total).

tree_quality(X,cmp(vs(G1),ps(G2),ats(G3),as(G4))):- 
  check_tree_quality(Srch,X,atom(Srch),once(atom_concat('VB',_,Srch);atom_concat('E',_,Srch);atom_concat('AUX',_,Srch)),G1),
  check_tree_quality(Srch,X,atom(Srch),once(atom_concat('NP',_,Srch);atom_concat('VP',_,Srch)),G2),
  check_tree_quality(Srch,X,true,once(atom(Srch)),G3),
  check_tree_quality(Srch,X,true,once(atomic(Srch)),G4),!.

pick_tree(XY,XY,XY,(==),white).
pick_tree(XX,YY,XY,Why,Color):- tree_quality(XX,X),tree_quality(YY,Y),pick_tree_why(XX,X,YY,Y,XY,Why,Color).
pick_tree_why( XX,X,_YY,Y,XX,X > Y, cyan):- X @> Y.
pick_tree_why(_XX,X, YY,Y,YY,X < Y, red):- X @< Y.
pick_tree_why( XX,X,_YY,Y,XX,X =@= Y, yellow).

dont_format(_,_).

text_to_best_tree(Text,Tree):- 
 (callable(Tree) -> Format = Tree ; (Format = dont_format)),
  call(Format,'=================================\n',[]),
  call(Format,'Testing: ~w  \n',[Text]),
  call(Format,'===========',[]),
  notrace(parser_charniak:text_to_charniak_tree(Text,Charniak)),!,
  call(Format,'===========',[]),
  notrace(parser_stanford:text_to_corenlp_tree(Text,CoreNLP)),!,
  call(Format,'===========~n',[]),!,
  pick_tree(Charniak,CoreNLP,Result,Why,Color),
  (var(Tree) -> Tree = Result;
  (with_output_to(string(S), color_format(hfg(Color),"~@",[print_tree(result(q(Why),Charniak,CoreNLP))])),!,
   call(Format,"~w",[S]))).

% calls text_to_best_tree/2
into_chat80_merged_segs(Text80,U):- 
 mort((
  parser_stanford:text_to_corenlp_w2(Text80,U2),
  text_to_best_tree(Text80,LExpr),
  tree_to_lexical_segs(LExpr,U1),
  smerge_segs(U1,U2,U))),!.

smerge_segs(U1,U2,U):- smerge_segs(alt,U1,U2,U).

%can_be_partof('Obj',W):-!, member(W,['Situation','Event']).
%can_be_partof(W,W):-!,fail.
%can_be_partof('Situation','Event'):-!,fail.
can_be_partof(_,_).


marked_segs([
 'VP'-'Situation',
 'WHNP'-'WHNP',
 'SBARQ'-'SBARQ',
 'ADVP'-'Adv',
 'SQ'-'SQ',
 'PP'-'PP',
 % 'VP'-'VPhrase',
  'S1'-'ROOT',
  'SBAR'-'Thing','NP'-'Obj',
  'NP-TMP'-'NP-TMP',
  'S'-'Situation','S1'-'Event','NML'-'NML','ADJP'-'tCol','FRAG'-'FRAG']).
%marked_seg_type(Mark,Type):- marked_segs(S),member(Mark-Type,S).
with_reset_segs(Start,G):- marked_segs(Segs), with_reset_segs(Start,Segs,G).
with_reset_segs(_Start,[],G):-!,call(G).
with_reset_segs(Start,[NP-_Type|S],G):- with_reset_flag(NP,Start,with_reset_segs(Start,S,G)).


with_reset_flag(NP,Start,G):-
  setup_call_cleanup(flag(NP,Was,Start),G, flag(NP,_,Was)).
  

fix_c2s(A,D):- with_reset_flag('word',1,fix_c2s0(A,D)).
fix_c2s0(A,D):- fix_c2s1(A,B),fix_c2s1(B,D).
fix_c2s1(A,D):- fix_tree_ses(A,B),fix_tree_ses(B,C),fix_tree_vps(C,D),!.

%fix_tree_ses(['S1',['S'|MORE]],OUT):- fix_tree_ses(['S1'|MORE],OUT).
%fix_tree_ses(['S',['S1'|MORE]],OUT):- fix_tree_ses(['S'|MORE],OUT).
%fix_tree_ses(['S',MORE],OUT):- !, fix_tree_ses(MORE,OUT).
fix_tree_ses_1(['SBAR',MORE],OUT):- fix_tree_ses(MORE,OUT).
fix_tree_ses_1(['RB',there],OUT):- fix_tree_ses(['EX',there],OUT).
/*
( [  'NP',
         ['DT',an],
         ['NNP','Italian'],
         [  'SBAR',
            [  'NP',
               ['WP',who]  ],
            [  ['VBD',became],
               [  'NP',
                  ['DT',the],
                  ['NN',world],
                  ['POS','\'s'],
                  ['JJS',greatest],
                  ['NN',tenor]  ]  ]  ]  ]).
*/


fix_tree_ses_1(['NP',['NP'|MORE]|MORE2],OUT):- append(['NP'|MORE],MORE2,NPMORE),fix_tree_ses(NPMORE,OUT).
fix_tree_ses_1(['VP',['NN',X]| MORE],O):- fix_tree_ses( [ 'VP', ['VB',X]| MORE],O).
%fix_tree_ses_1(['VP'| MORE],O):- fix_tree_ses(MORE,O).
fix_tree_ses_1(['VP',['AUX'|MORE]|MORE2],OUT):- fix_tree_ses([['AUX'|MORE]|MORE2],OUT).
fix_tree_ses_1([S,['VP',['VB',Have]|VPMORE]],O):-  fix_tree_ses([S,['VB',Have]|VPMORE],O).

fix_tree_ses_1(['WHADJP'|MORE],OUT):- !, fix_tree_ses(MORE,OUT).


fix_tree_ses(['ROOT',MORE],OUT):- fix_tree_ses(MORE,OUT).
%fix_tree_ses(LIST,OUT):- fix_tree_ses_1(LIST,M),!,fix_tree_ses(M,OUT).
fix_tree_ses(LIST,OUT):- is_list(LIST), maplist(fix_tree_ses,LIST,OUT),!.
fix_tree_ses(B,A):- replace_seg_name(B,A).
fix_tree_ses(S,S):-!.

replace_seg_name(B,A):- atom(B),marked_segs_replace(B,A).
marked_segs_replace(w(W,[  pos(POS)|_]),w(W,[pos(POS)])).
marked_segs_replace('S1','ROOT').
marked_segs_replace('WHNP','NP').
marked_segs_replace('VBP','VB').
%marked_segs_replace('SBAR','S').
%marked_segs_replace('SBARQ','SBAR').
marked_segs_replace(SQ,S):- atom_concat(S,'Q',SQ).
/* 'VP'-'Situation',
 'WHNP'-'WHNP',
 'SBARQ'-'SBARQ',
 'ADVP'-'Adv',
 'SQ'-'SQ',
 'PP'-'PP',
 % 'VP'-'VPhrase',
  'S1'-'ROOT').
*/

fix_tree_w2s(WORD,POS):- is_pos(WORD,POS),!. 
fix_tree_w2s(LIST,OUT):- is_list(LIST), maplist(fix_tree_w2s,LIST,OUT),!.
fix_tree_w2s(I,I).

fix_tree_vps([S|MORE],[S|OUT]):- atom(S),fix_tree_pp(MORE,OUT),!.
fix_tree_vps(LIST,OUT):- is_list(LIST), maplist(fix_tree_vps,LIST,OUT),!.
fix_tree_vps(I,I).
%fix_tree_pp([[H]|MORE],OUT):- fix_tree_pp([H|MORE],OUT),!.
fix_tree_pp([['PP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp([w(S,['PP'|List])|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).
fix_tree_pp(['PP',w(S,List)|PPMORE],[w(S,['PP'|List])|OUT]):- fix_tree_pp(PPMORE,OUT).
% fix_tree_pp([['ADVP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp([w(S,['ADVP'|List])|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).
fix_tree_pp([S|MORE],[S|OUT]):- atom(S),fix_tree_pp(MORE,OUT),!.
fix_tree_pp([H|MORE],[H|OUT]):- fix_tree_pp(MORE,OUT),!.
fix_tree_pp(I,I).
%fix_tree_pp([['ADVP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp(['ADVP',w(S,List)|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).

%fix_tree_pp(['PP',w(S,List)]|MORE],OUT):- fix_tree_pp([w(S,['PP'|List])|MORE],OUT).
%fix_tree_pp(['ADVP',w(S,List)]|MORE],OUT):- fix_tree_pp([w(S,['ADVP'|List])|MORE],OUT).


lxpr_to_segs([],[]):- !.

lxpr_to_segs(WORD,[POS]):- is_pos(WORD,POS),!. 
lxpr_to_segs([WORD|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([[WORD]|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([P|MORE],Out):- atom(P),maplist(lxpr_to_segs,MORE,MORES),create_coref(P,MORES,Out).
lxpr_to_segs([H|T],POS):- lxpr_to_segs(H,POSH),lxpr_to_segs(T,POST), !, append(POSH,POST,POS).
lxpr_to_segs(I,I).

always_create_coref(A,B,C,D):- \+ tracing, notrace(catch(create_coref(A,B,C,D),_,fail)),!.
always_create_coref(A,B,C,D):- trace,create_coref(A,B,C,D).
/*

?- 
   my_aceparagraph_to_drs("is there a man who becomes the greatest tenor?",X,Y,Z,T,R),
  ape_to_penn_tree(Y,P),!,
  tree_to_lexical_segs(P,L),!.

*/

favored_pos('DT').
favored_pos('JJ').
favored_pos('CC').
favored_pos('RB').
favored_pos(_).


is_word_or_span(S):- compound(S), (S = w(_,_); S=span(_)),!.

:- export(is_word_or_span/1).
:- system:import(is_word_or_span/1).

smerge_segs(F,U1,U2,U):- 
  smerge_segsl(F,U1,U2,U3),!,
  sort_words(U3,U),!.
/*
smerge_segsl(F,[span(List)|U1],U2,[span([Seg|NewList])|U3]):-
   Seg=seg(_,_),select(Seg,List,List0),   
   select(span(EList2),U2,NewU2),
   select(Seg,EList2,List1),!,
   smerge_w_list(F,List0,List1,NewList),
   smerge_segsl(F,U1,NewU2,U3),!.
*/
smerge_segsl(F,U1,U2,Out):-
   select(span(S1),U1,NewU1),
   Seg=seg(_,_),
   select(Seg,S1,List1),
   select(span(S2),U2,NewU2),
   select(Seg,S2,List2),
  find_subterm(List1,phrase(Type1)),
  find_subterm(List2,phrase(Type2)),
  prefered_span_type(Type1,Type2),!,
  smerge_segsl(F,[span(S2)|NewU1],[span(S1)|NewU2],Out).

smerge_segsl(F,U1,U2,[Out|U3]):-
   select(span(S1),U1,NewU1),
   Seg=seg(_,_),
   select(Seg,S1,List1),
   select(span(S2),U2,NewU2),
   select(Seg,S2,List2),
  find_subterm(List1,phrase(Type1)),
  find_subterm(List2,phrase(Type2)),
  not_conflicted_span_types(Type1,Type2),
   smerge_segsl(F,NewU1,NewU2,U3),
   smerge_w_list(F,List1,List2,NewList),!,
   Out = span([Seg|NewList]).

smerge_segsl(F,[w(W1,EList1)|U1],U2,[w(W1,NewList)|U3]):-
  select(w(W1,EList2),U2,NewU2),
  select(pos(Type1),EList1,NewEList1),
  select(pos(Type2),EList2,NewEList2),
 (prefered_span_type(Type1,Type2)
    -> smerge_w_list(F,[pos(Type2)|NewEList1],[pos(Type1)|NewEList2],NewList)
     ; smerge_w_list(F,EList1,EList2,NewList)),
  smerge_segsl(F,U1,NewU2,U3),!.
smerge_segsl(F,[E|U1],U2,[E|U3]):-
   smerge_segsl(F,U1,U2,U3),!.
smerge_segsl(F,U1,[E|U2],[MaybeAlt|U3]):-
   compound(E),maybe_alt(F,E,MaybeAlt),!,
   smerge_segsl(F,U1,U2,U3),!.
smerge_segsl(F,U1,[Drop|U2],U3):- !,
   wdmsg(dropping(Drop)),
   smerge_segsl(F,U1,U2,U3),!.
smerge_segsl(_F,U1,[],U1):- U1==[],!.
%smerge_segsl(F,[],U2,U2):-!.

prefered_span_type('rb',X):- X \== 'rb'.
prefered_span_type('ADVP','NP').

not_conflicted_span_types(Type1,Type2):- Type1==Type2,!.
not_conflicted_span_types(Type1,Type2):-  Type1 @> Type2,!,not_conflicted_span_types(Type2,Type1).
not_conflicted_span_types('VP','NP'):- !, fail.
not_conflicted_span_types(Type1,Type2):- wdmsg(not_conflicted_span_types(Type1,Type2)).
%flag_remove_alts:- true.

maybe_alt(F,span(L),span([FE|L])):- altify(F,'unused_span',FE), !, fail.
maybe_alt(F,w(W,L),span([FE,w(W),seg(N,N)|L])):-member(loc(N),L),!,altify(F,'unused_w2',FE).
%maybe_alt(F,w(W,L),span([alt|L])).
maybe_alt(F,E,span([FE])):- altify(F,E,FE).

altify(F,E,FE):- FE =.. [F,E].



smerge_w_list(_,U1,[],U1):-!.
smerge_w_list(F,U1,[E|U2],[O|U3]):-
 \+ member(E,U1),
   free_mismatcher(E,EE),
  (member(EE,U1) -> altify(F,E,O) ; O= E),!,
   smerge_w_list(F,U1,U2,U3),!.
smerge_w_list(F,[E|U1],U2,[E|U3]):-
   select(E,U2,NewU2),!,
   smerge_w_list(F,U1,NewU2,U3),!.
smerge_w_list(F,[E|U1],U2,[E|U3]):-
   smerge_w_list(F,U1,U2,U3),!.

%free_mismatcher(E,EE):- compound(E),compound_name_arity(E,_,A),A>0,!,duplicate_term(E,EE), nb_setarg(A,EE,_).
free_mismatcher(E,EE):- compound(E),compound_name_arity(E,F,A),compound_name_arity(EE,F,A),!.
free_mismatcher(E,E).


lxpr_to_list(String, LExpr):- any_to_codelist(String,Codes), c2s(LExpr0,Codes,_) ,fix_c2s(LExpr0,LExpr),
 !.

c2s(L)  --> `(`, !, c2s_list(L),!.
c2s(L)  --> one_blank, !, c2s(L),!.
c2s(L)  --> c2w_chars(Chars),!,{any_to_atom(Chars,L)}.

c2w_chars([]) --> dcg_peek(one_blank;`(`;`)`),!.
c2w_chars([C|X]) --> [C],!,c2w_chars(X).

c2s_list(X) --> one_blank,!,c2s_list(X).
c2s_list([]) --> `)`, !.
c2s_list([Car|Cdr]) --> c2s(Car), !, c2s_list(Cdr),!.


incr_ht(HT,Name,V,V1):- 
  nb_rb_get_node(HT,Name,Node),nb_rb_node_value(Node,V),!,
  SV is V1, 
  nb_rb_set_node_value(Node,SV),!.
incr_ht(HT,Name,V,V1):- 
  rb_in('$start',HT,V),
  SV is V1,
  nb_rb_insert(HT,Name,SV),!.


%add_p_to_words(_Var,_,[],[]):-!.
% create_coref('S',MORES,MORES):- !.
create_coref('ROOT',MORES,MORES):- !.
create_coref(NP,MORES,Out):- atom(NP), % marked_segs(Segs),%member(NP-Type,Segs),
  %notrace(incr_ht(HT,NP,N,N+1)),
  %flag(NP,N,N+1),   % atomic_list_concat([NP,'@',N],NPNRef),!,
  SEG = seg(start,end),
  NPNRef = r(NP,SEG),
  NPN=span([SEG,phrase(NP),size(0),lnks(0),'#'(NPNRef),txt([]),childs(0)]),
  %add_var_to_env_now(NP,Var),
  must_add_p_to_words(NP,NPN,MORES,Out0),
  Out=[NPN|Out0].
create_coref(NPN,MORES,Out):- trace,add_p_to_words(NPN,NPN,MORES,Out).

%append_varname_h(_,_).
append_varname_h(X,Y):- append_varname(X,Y).


add_loc_to_span(PosL,P):- find_subterm(PosL,loc(X)), find_subterm(P,seg(SW,_),Seg),add_loc_to_span3(X,SW,Seg),!.

add_loc_to_span3(X,SW,Seg):- SW=start,nb_setarg(1,Seg,X),!,nb_setarg(2,Seg,X).
add_loc_to_span3(X,_,Seg):- nb_setarg(2,Seg,X).



resize_span(P):- ignore((( find_subterm(P,seg(S,E)),number(S),number(E),Z is E - S + 1,find_subterm(P,size(_),Size),nb_setarg(1,Size,Z)))).

must_add_p_to_words(Var,P,Child,OUT):- \+ tracing, notrace(catch(add_p_to_words(Var,P,Child,OUT),_,fail)),!.
must_add_p_to_words(Var,P,Child,OUT):- trace,add_p_to_words(Var,P,Child,OUT),!.
  
%add_p_to_words(_Var,_,[],[]):- !.
%add_p_to_words(Var,P,[[w(H,L)]|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
%add_p_to_words(Var,P,[w(H,L)|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,List,Out):- is_list(List),!,maplist(add_p_to_words(Var+1,P),List,Out).
%add_p_to_words(Var,P,Child,OUT):- atom(Child),!,maplist(add_p_to_words(Var+1,P),List,Out).
%add_p_to_words(Var,P,T,TT):- add_p_to_word(Var,P,T,TT),!.
add_p_to_words(Var,P,Child,OUT):-
 must_or_rtrace(( 
  functor(Child,ChildType,_),
  nop(pprint_ecp_cmt(yellow,add_p_to_words(Var,P,Child))),
  find_subterm(P,phrase(Type)),
  find_subterm(P,'#'(ID)),
  find_subterm(P,txt(_),Txt),
 ignore(add_loc_to_span(Child,P)),!,
  child_loc(Child,LOC),
  resize_span(P),  
  %ignore((find_subterm(Child,txt(S)), append_varname_h(S,Var))),
  ignore(((     
   %ChildType == span,   
   (Var=Atom+1+1,atom(Atom)),
   find_subterm(P,childs(Chldrn),ChldrnHolder), 
   (find_subterm(Child,phrase(ChildPhrase));(fail,find_subterm(Child,pos(ChildPhrase)))),
   (find_subterm(Child,'#'(ChildID));ChildID=LOC),   
   NChldrn is Chldrn + 1, 
   nb_setarg(1,ChldrnHolder,NChldrn),   
   nb_set_add(P,child(NChldrn,ChildPhrase,ChildID)),
   !))),  

  ignore(((
  %\+ find_subterm(Child,link(_,Type,_,_)), 
     find_subterm(Child,lnks(OldN),Holder),    
     LinkNum is OldN + 1,
     nb_setarg(1,Holder,LinkNum),    
     nb_set_add(Child,link(LinkNum,Type,ID))))),

  OUT=Child,!,
  ignore(((ChildType=w,find_subterm(Child,txt(W)), nb_set_add1(Txt,W)))))).
  %[Child,partOf(X,Y)]

/*
?- ape_to_penn_tree(
[ [specification,[s,[np,[pname,'John']],[vp,[vbar,[],[vcompl,[v,likes],[np,[pname,'Mary']]],[]]]],'.'],
  [specification,[s,[np,[pname,'Bill']],[vp,[vbar,[],[vcompl,[v,sees], [np,[pname,'Mary']]],[]]]],'.']],Tree1)
*/




ape_to_penn_tree(A,[]):-A==[],!.
ape_to_penn_tree(A,P):- 
   while_tracing_pipeline(print_tree(?- ape_to_penn_tree(A,_))),
   ape_to_penn_tree_0(A,P),!,
   if_debug_module(print_tree(A -> P)).

ape_to_penn_tree_0([A],P):- is_list(A),!, ape_to_penn_tree_0(A,P).
ape_to_penn_tree_0(A,P):- 
  must_or_rtrace(ape_to_penn_tree([],A,AP)),
  must_or_rtrace(ape_to_penn_tree([],AP,P)).

ape_pos_to_pen(A,_):- \+ atom(A),!,fail.
ape_pos_to_pen(pname,'NNP').
ape_pos_to_pen(prep,'IN').
ape_pos_to_pen(npposs,'$PNS').
ape_pos_to_pen(det,'DT').
ape_pos_to_pen(coord,'CC').
ape_pos_to_pen(nump,'CD').
ape_pos_to_pen(n,'NN').
ape_pos_to_pen(nbar,'NP').
ape_pos_to_pen(adj,'JJ').

ape_to_penn_tree(_,A,P):- (var(A);A==[]),!,P=A.
ape_to_penn_tree(_,A,P):- ape_pos_to_pen(A,P),!.
ape_to_penn_tree(_,A,P):- atom(A),upcase_atom(A,P).
ape_to_penn_tree(_,A,PP):- \+ is_list(A),PP=A.

ape_to_penn_tree(O,A,P):- maplist(is_list,A),!,maplist(ape_to_penn_tree(O),A,P).


%ape_to_penn_tree(O,A,PP):- is_list(A),append(L,[P,A2],A),atom(A2),!,
%  append(L,[P,w('.',A2)],AA), ape_to_penn_tree(O,AA,PP).

ape_to_penn_tree(O,A,P):- select(E,A,AA),E==[],!,ape_to_penn_tree(O,AA,P).
ape_to_penn_tree(_O, [W],[]):- ape_pos(W), !.
ape_to_penn_tree(_O, [W],['CD',W]):- number(W), !.
ape_to_penn_tree(_O, [W],['.',W]):- atom(W), !.
ape_to_penn_tree(O,[A,W],[P,W]):- atom(A),atom(W),!, ape_to_penn_tree(O,A,P).

ape_to_penn_tree(O,A,P):- append(L,[A1,A2],A), \+ atom(A1),atom(A2),
 append(L,[A1,['.',A2]],LL),!, ape_to_penn_tree(O,LL,P).

ape_to_penn_tree(O,A,PP):- append(L,[A1,A2],A), atom(A1),atom(A2),!,
  ape_to_penn_tree(O,L,P), upcase_atom(A1,A1U), append(P,[A1U,A2],PP).

ape_to_penn_tree([],['specification', A],P):-!,ape_to_penn_tree(specification,A,P).
ape_to_penn_tree([],['specification'| A],P):-!,ape_to_penn_tree(specification,A,P).
ape_to_penn_tree(np,[nbar,A],P):- ape_to_penn_tree(np,A,P).
ape_to_penn_tree(np,[nbar|A],P):- ape_to_penn_tree(np,A,P).
ape_to_penn_tree(vp,[vbar,A],P):- ape_to_penn_tree(vp,A,P).
ape_to_penn_tree(vp,[vbar|A],P):- ape_to_penn_tree(vp,A,P).
ape_to_penn_tree(VP,[E,A],P):- eliminate_header(E),ape_to_penn_tree(VP,A,P).
ape_to_penn_tree(VP,[E|A],P):- eliminate_header(E),ape_to_penn_tree(VP,A,P).

ape_to_penn_tree(_,[A|AA],[A|PP]):- \+ is_list(A),maplist(ape_to_penn_list_tree(A),AA,PP),!.
ape_to_penn_tree(O,AA,PP):- maplist(ape_to_penn_tree(O),AA,PP),!.
%ape_to_penn_tree(O,[A,B|AA],[P,BB|PP]):- \+ is_list(A), ape_to_penn_tree(A,B,BB),ape_to_penn_tree(A,AA,PP),!.
%ape_to_penn_tree(O,[A|AA],[P|PP]):- ape_to_penn_tree(O,A,P),ape_to_penn_tree(O,AA,PP),!.
%ape_to_penn_tree(_,A,A).
%ape_to_penn_tree(O,A,P):- is_list(A),maplist(ape_to_penn_tree,A,P).

ape_to_penn_list_tree(O,A,P):- ape_to_penn_tree(O,A,PP),listify(PP,P).

ape_pos(aux).
ape_pos(adv).
ape_pos(vmod).
ape_pos(adj).
ape_pos(v).
ape_pos(n).
ape_pos(X):- 
  member(M,[grammar_contentwords,grammar,system]),
   current_predicate(M:F/N),
   functor(P,F,N),
   \+ predicate_property(M:P,imported_from(_)),
   \+ predicate_property(M:P,(foreign)),
   
   (F=X ; (clause(M:P,_),sub_term(S,P),atom(S),S=X)).

 


eliminate_header('VCOMPL').
%eliminate_header('QUESTION').
%eliminate_header('TOPIC').
child_loc(Child,LOC):- find_subterm(Child,loc(_),LOC),!.
child_loc(Child,LOC):- find_subterm(Child,seg(_,_),LOC),!.

is_pos(p(Pos,Head),Out):-!,is_pos([Pos, Head],Out).
is_pos([Word],Out):-!, is_pos(Word,Out).
is_pos([Pos,[Quote,Head]],Out):- quote==Quote, atom(Head), !,is_pos([Pos, Head],Out).
is_pos([Pos, Head],TwoPOS):- atomic(Head), Head\==[], atom(Pos), downcase_atom(Pos,POSD),is_pos3([],POSD, Head, TwoPOS),!.

is_pos3(Props,Pos, Head, OUT):- 
 atomic_list_concat(Words,' ',Head), 
 Words=[_,_|_],!,
 maplist(is_pos3([mws(Head)|Props],Pos), Words, OUT).

is_pos3(Props,Pos, Head, OUT):- 
  favored_pos(Pos), 
  any_to_string(Head,SHead),
  downcase_atom(Head,WD),
  !,flag('word',X,X+1),
  OUT = w(WD,[pos(Pos),loc(X),lnks(0),txt(SHead)|Props]).

is_pos3(Props,Pos, Head, OUT):- 
  any_to_string(Head,SHead),
  downcase_atom(Head,WD),
  !,flag('word',X,X+1),
  OUT = [Pos,w(WD,[pos(Pos),loc(X),lnks(0),txt(SHead)|Props])].

sort_words(List,Sorted):- notrace((predsort(by_word_loc,List,Sorted))).
:- export(sort_words/2).
:- system:import(sort_words/2).
% my_aceparagraph_to_drs("is there a man who becomes the greatest tenor?",X,Y,Z,T,R),ape_to_penn_tree(Y,P),print_tree(P),!,tree_to_lexical_segs(P,L),          !.

by_word_loc(R,A,B):-into_loc_sort(A,AK),into_loc_sort(B,BK),compare(RK,AK,BK), (RK == (=) -> compare(R,A,B) ; R = RK).
into_loc_sort(w(_,List),Key):- member(loc(S),List), member(lnks(L),List), Key = [a,S,0,S,L],!.
into_loc_sort(span(List),Key):- member(seg(S,E),List),once(member(lnks(L),List);L=10),once(member(size(W),List);W=0),
  RS is 100-W, % E1 is E-1, 
  Key = [span, W, E,S,RS,L|List],!.
into_loc_sort(span(List),Key):- member(seg(S,E),List),
  once(member(lnks(L),List);L=0),once(member(childs(C),List);C=0),once(member(size(W),List);W=0),
  NW is 100-W, NL is 100-L, NC is 100-C, % E1 is E-1,
  Key = [span,NW,C,S,E,NL,NC,List],!.
into_loc_sort(span(L1),Key):- member(List,L1),member(seg(_,_),List),into_loc_sort(span(List),Key).
into_loc_sort(A,Key):- A=..[_|AA], findnsols(4,T, ((sub_term(T,AA),compound(T),arg(1,T,N),number(N));T=AA),Key).

tree_to_lexical_segs(LExpr,Sorted):-
  penn_tree_to_segs(1,LExpr,Sorted),!.

penn_tree_to_segs(_Start,LExpr,[]):- LExpr==[],!.
penn_tree_to_segs(Start,LExpr,Sorted):- 
 if_debug_module(print_tree(penn_tree_to_segs(LExpr,_))),
 with_reset_flag('word',1,
  with_reset_segs(Start,lxpr_to_segs(LExpr,CSegs))),
 into_segs(CSegs,Sorted),!.

into_segs(SSegs,Sorted):- 
 findall(Words, (sub_term(C,SSegs), compound(C), C = sentence(_N,Words,_Props)),CL),
 CL\==[],!, 
 maplist(into_segs,CL,SL),
 flatten(SL,Sorted).

into_segs(CSegs,Sorted):- 
 findall(W2,(sub_term(W2,CSegs),is_word_or_span(W2)),Segs),
 sort_words(Segs,Sorted),!.

segs_retain_w2(SegsF,InfoS,PosW2s):-
 apply:partition(\=(w(_,_)), SegsF, Info, PosW2s),!,
 sort_words(Info,InfoS).







lex_penn(Penn,W):- atom_concat(Penn,'_lex',PennLex),call(PennLex,W).
lex_penn(Penn,W,A):- atom_concat(Penn,'_lex',PennLex),call(PennLex,W,A).
lex_penn(Penn,W,A,B):- atom_concat(Penn,'_lex',PennLex),call(PennLex,W,A,B).
lex_penn(Penn,W,A,B,C):- atom_concat(Penn,'_lex',PennLex),call(PennLex,W,A,B,C).
lex_penn(Penn,W,A,B,C,D):- atom_concat(Penn,'_lex',PennLex),call(PennLex,W,A,B,C,D).






w2_w_l(w(Text,List),Text,List).

penn_lex(Penn,Prolog,W2):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text)).
penn_lex(Penn,Prolog,W2,Z):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text,Z)).
penn_lex(Penn,Prolog,W2,A,Z):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text,A,Z)).
penn_lex(Penn,Prolog,W2,A,B,Z):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text,A,B,Z)).
penn_lex(Penn,Prolog,W2,A,B,C,Z):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text,A,B,C,Z)).
penn_lex(Penn,Prolog,W2,A,B,C,D,Z):- w2_w_l(W2,Text,List),or_rev(Penn,List,call(Prolog,Text,A,B,C,D,Z)).

or_rev(Penn,List,OR):- OR*->true;(fail,member(pos(Penn),List),functor(OR,call,A),fail,(A < 3 -> true ; (arg(3,OR,Root),member(root(Root),List)))).

:- fixup_exports.
