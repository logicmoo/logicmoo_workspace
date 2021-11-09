:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- if(\+ prolog_load_context(reloading, true)).
:- use_module(library(logicmoo_clif)).
:- endif.
%:- xlisting(lock_predicate/1).
%:- autoload_all.


:- '$set_source_module'(parser_chat80).


test_objecteese:-
 test_objecteese(
   "A man who is happy or who is sad waits.",
  "a type1 who is value53 or who is value70 action54s ."),
 test_objecteese(
   "John eats or John drinks.",
   "object19 action44s or object19 action15s ."),
 test_objecteese(
   "Some men wait. They talk.",
  "some type17s action54 . they action16 ."),
 test_objecteese(
   "For each of 2 tables 2 girls lift it.",
  "for each of 2 type240s 2 type238s action17 it ."),
 test_objecteese(
   "A man X waits. X sleeps.",
  "a type1 X action54s. X action10s .").



test_objecteese(X,Z):- locally(set_prolog_flag(debug,false),cvt_to_objecteese(X,Y)), 
  show_failure(always,check_answer(X,Y,Z,true))-> true ; locally(set_prolog_flag(debug,true),cvt_to_objecteese(X,Y)).

do_objecteese_test(X):- cvt_to_objecteese(X,Y),!,nl,any_to_string(Y,Z),
  format('~N test_objecteese(~n   ~q,~n  ~q).~n',[X,Z]),!.



sample_set80([ does, object6, action5, object7, ? ]).
sample_set80([ is, object7, in, object6, ? ]).
sample_set80([ does, mexico, action1, the, united_states, ? ]).
sample_set80([ is, the, attrib1, of, object5, greater, than, value1, ? ]).
sample_set80([ does, the, attrib1, of, object5, exceed, value2, million, ? ]).
sample_set80([ is, the, attrib1, of, object5, value3, ? ]).
sample_set80([ does, the, attrib1, of, object5, exceed, the, attrib1, of, object4, ? ]).
sample_set80([ is, spain, action1ed, by, the, object8, ? ]).
sample_set80([ does, the, object3, action1, spain, ? ]).
sample_set80([ is, the, object9, in, object11swis, ? ]).
sample_set80([ is, the, object10, in, object2, ? ]).
sample_set80([ what, type3s, are, there, ? ]).
sample_set80([ does, object12, action1, object5, ? ]).
sample_set80([ what, is, the, property1, of, upper, volta, ? ]).
sample_set80([ where, is, the, largest, type1, ? ]).
sample_set80([ which, type1s, are, objectian2, ? ]).
sample_set80([ which, type1, '''', s, property1, is, london, ? ]).
sample_set80([ which, is, the, largest, african, type1, ? ]).
sample_set80([ how, large, is, the, smallest, object6n, type1, ? ]).
sample_set80([ what, is, the, type2, that, action1s, african, type1s, and, that, action1s, asian, type1s, ? ]).
sample_set80([ what, are, the, property1s, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ which, type1s, are, action1ed, by, two, type6s, ? ]).
sample_set80([ how, many, type1s, does, the, danube, flow, through, ? ]).
sample_set80([ how, many, type1s, does, the, object1, action3, through, ? ]).
sample_set80([ what, is, the, total, area, of, type1s, south, of, the, equator, and, not, in, australasia, ? ]).
sample_set80([ what, is, the, total, attrib1, of, type1s, south, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, total, attrib1, of, type1s, abverb2, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, total, area, of, type1s, south, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, average, area, of, the, type1s, in, each, type5, ? ]).
sample_set80([ is, there, more, than, one, type1, in, each, type5, ? ]).
sample_set80([ is, there, some, type2, that, does, not, action1, any, type1, ? ]).	       
sample_set80([ what, are, the, type1s, from, which, a, type3, flows, into, the, object1, ? ]).
sample_set80([ what, are, the, type5s, no, type1, in, which, action5s, more, than, two, type4s, whose, attrib1, exceeds, (1), million, ? ]).
sample_set80([ which, type1, action1ing, the, object14, action1s, a, type1, that, is, action1ed, by, a, type1, whose, attrib1, exceeds, the, attrib1, of, object4, ? ]).
sample_set80([ which, type1s, have, an, attrib1, exceeding, (10), million, ? ]).
sample_set80([ which, type1s, with, a, attrib1, exceeding, (10), million, action1, the, object3, ? ]).
sample_set80([ what, percentage, of, type1s, action1, each, type2, ? ]).
sample_set80([ what, type1s, are, there, in, object2, ? ]).
sample_set80([ what, are, the, areas, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, flow, through, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, flow, through, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, through, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, through, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, property1s, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type4s, in, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, type4s, do, the, type1s, action1ing, the, object1, action5, ? ]).
sample_set80("iraq action1s iran?").
sample_set80("iraq does action1 iran?").
sample_set80("iraq did action1 iran?").
%sample_set80("iraq will action1 iran?").
sample_set80("iraq is action1ing iran?").
sample_set80("iraq was action1ing iran?").
sample_set80("iran is action1ed by iraq?").
%sample_set80("iraq has action1 iran?").
%sample_set80("iraq has a action1 object5").
% X flows into Dest from Origin
sample_set80("the object9 flows into object15?").
sample_set80("the object9 flows to object15?").
sample_set80("the object9 flows in object15?").
sample_set80("the object9 flows from object15?").
sample_set80("the object9 flows at object15?").
sample_set80("the object9 flows through object15?").
sample_set80("the object9 flows into object11swis?").
sample_set80("the object9 flows to object11swis?").
sample_set80("the object9 flows in object11swis?").
sample_set80("the object9 flows from object11swis?").
sample_set80("the object9 flows at object11swis?").
sample_set80("the object9 flows through object11swis?").

sample_set80("the object9 action1s into object15?").
sample_set80("the object9 action1s to object15?").
sample_set80("the object9 action1s in object15?").
sample_set80("the object9 action1s from object15?").
sample_set80("the object9 action1s at object15?").
sample_set80("the object9 action1s through object15?").
sample_set80("the object9 action1s into object11swis?").
sample_set80("the object9 action1s to object11swis?").
sample_set80("the object9 action1s in object11swis?").
sample_set80("the object9 action1s from object11swis?").
sample_set80("the object9 action1s at object11swis?").
sample_set80("the object9 action1s through object11swis?").


chat80_test("does america contain new york ?").
chat80_test("is new york in america ?").
chat80_test("does mexico border the united states ?").
chat80_test("the population of china greater than 200 million ?").
chat80_test("the population of china exceed 1000 million ?").
chat80_test("the population of china 840 million ?").
chat80_test("does the population of china exceed the population of india ?").
chat80_test("is spain bordered by the pacific ?").
chat80_test("does the atlantic border spain ?").
chat80_test("is the rhine in switzerland ?").
chat80_test("is the united kingdom in europe ?").
chat80_test("what rivers are there ?").
chat80_test("does afghanistan border china ?").
chat80_test("what is the capital of upper volta ?").
chat80_test("where is the largest country ?").
chat80_test("which countries are european ?").
chat80_test("which country ' s capital is london ?").
chat80_test("which is the largest african country ?").
chat80_test("how large is the smallest american country ?").
chat80_test("what is the ocean that borders african countries and that borders asian countries ?").
chat80_test("what are the capitals of the countries bordering the baltic ?").
chat80_test("which countries are bordered by two seas ?").
chat80_test("how many countries does the danube flow through ?").
chat80_test("what is the total area of countries south of the equator and not in australasia ?").
chat80_test("what is the average area of the countries in each continent ?").
chat80_test("is there more than one country in each continent ?").
chat80_test("is there some ocean that does not border any country ?").
chat80_test("what are the countries from which a river flows into the black sea ?").
chat80_test("are the continents no country in which contains more than two cities whose population exceeds 1 million ?").
chat80_test("which country bordering the mediterranean borders a country that is bordered by a country whose population exceeds the population of india ?").
chat80_test("countries have a population exceeding 10 million ?").
chat80_test("countries with a population exceeding 10 million border the atlantic ?").
chat80_test("what percentage of countries border each ocean ?").
chat80_test("what countries are there in europe ?").
chat80_test("what are the areas of the countries bordering the baltic ?").
chat80_test("iraq borders iran?").
chat80_test("iraq does border iran?").
chat80_test("iraq did border iran?").
chat80_test("iraq will border iran?").
chat80_test("iraq is bordering iran?").
chat80_test("iraq was bordering iran?").
chat80_test("iran is bordered by iraq?").



%map_ees_tag('NN',type,'').
map_ees_word('country',type,'').
/*
map_ees_word('asian',type,ian).
map_ees_word('american',object,'ian').
map_ees_word('flow',action,'').
map_ees_word('flows',action,'s').
map_ees_word('kingdom',type,'').
map_ees_word('african',type,'ish').
map_ees_word('million','','').
map_ees_word('million',' type','').
map_ees_word('volta',object,'').
map_ees_word('equator',object,'').
*/
/*
map_ees_word('address',attrib,'').
map_ees_word('area',attrib,'').
map_ees_word('areas',attrib,'s').
map_ees_word('atlantic',object,'').
map_ees_word('baltic',object,'').
map_ees_word('bordered',action,'ed').
map_ees_word('borders',action,'s').
map_ees_word('border',action,'').
map_ees_word('capital',property,'').
map_ees_word('china',object,'').
map_ees_word('continent','large type','').
map_ees_word('likes',action,'s').
map_ees_word('largest',adjective,'est').
map_ees_word('man','type','').
map_ees_word('new','a adjective','').
map_ees_word('ocean',type,'').
map_ees_word('population',attrib,'').
map_ees_word('rhine',object,'').
map_ees_word('river',type,'').
map_ees_word('smallest',type,'liest').
map_ees_word('south',adverb,'').
map_ees_word('united','action','ed').
*/
/*
red = value
happy = 
*/

map_ees_tag('JJR',value,'er').
map_ees_tag('JJS',value,'est').
map_ees_tag('JJ',value,'').
map_ees_tag('RBR',value,'lier').
map_ees_tag('RBS',value,'liest').
map_ees_tag('RB',value,'').
map_ees_tag('VB_NN',action,'').
map_ees_tag('NN_JJ','adjective','').
map_ees_tag('NNP',object,'').
map_ees_tag('NNPS',object,'s').
map_ees_tag('NNS',type,'s').
map_ees_tag('NN',type,'').
map_ees_tag('VB',action,'').
map_ees_tag('VBN',action,'ed').
map_ees_tag('VBD',action,'ed').
map_ees_tag('VBG',action,'ing').
map_ees_tag('VBZ',action,'s').

noun_var(neutr,type).
noun_var(human,agent).

map_ees_lex(Wives,Var,'s'):- clex:noun_pl(Wives, _, Type),noun_var(Type,Var).
map_ees_lex(Wife,Var,''):-   clex:noun_pl(_, Wife, Type),noun_var(Type,Var).
map_ees_lex(Clerk,Var,''):-  clex:noun_sg(Clerk, _, Type),noun_var(Type,Var).
%map_ees_tag(Y,'adjective','est'):-  talkdb:talk_db(superl,_,Y).
%map_ees_lex(Y,'adjective','er'):-  talkdb:talk_db(comp,_,Y).
%map_ees_tag(Y,'adjective',''):- talkdb:talk_db(adj,Y).
%map_ees_tag('VBZ',attrib,' is ').


map_ees(_,LPOS,'object',S):- member(ner('COUNTRY'),LPOS),!,(member(form(pl),LPOS)->S='s';S='').
map_ees(_,LPOS,'attrib',S):- member(dep_child(prep,n(of,_)),LPOS),!,(member(form(pl),LPOS)->S='s';S='').
map_ees(W,LPOS,N,S):- ((member(pos(Pos),LPOS),upcase_atom(Pos,POS))->true;POS='unk'), 
 (map_ees_word(W,N,S)*-> true ; map_ees_tag(POS,N,S) *-> true ; map_ees_lex(W,N,S)).

% may_debug(G):- !, call(G).
may_debug(G):- ignore((current_prolog_flag(debug,true),!,on_x_fail(in_cmt(call(G))))).
may_debug(_).

:- dynamic(tmp:dont_change/1). 
never_change(X):- det_lex(X,_,_,_).
never_change(X):- loc_pred_lex(_,X,_).
never_change(X):- modal_verb_form_aux(X,_,_,_).
never_change(X):- number_lex(X,_,_).
never_change(X):- pers_pron_lex(X,_,_,_,_).
never_change(X):- poss_pron_lex(X,_,_,_).
never_change(X):- prep_lex(X).
never_change(X):- determiners:semlex_det(X,_,_,Z),Z\=lam(B, lam(C, merge(merge(D:drs([D:X:E], []), app(B, E)), app(C, E)))).
never_change(not).  
never_change(no).
never_change(X):- flexicon(numcompare,_,Z), sub_var(X,Z).
never_change(X):- flexicon(det,_,Z), sub_var(X,Z).
never_change(X):- quantifier_pron_lex(X,_,_).
never_change(X):- terminator_lex(X,_).
never_change(X):- verb_form_aux(X,_,_,_).
never_change(X):- wh_art_lex(X,_,_,_).
never_change(X):- wh_pron_lex(X,_).
never_change(X):- wh_rel_pron_lex(X,_).
never_change(X):- tmp:dont_change(X).
never_change(X):- string2digit:s2d(X,_).
tmp:dont_change(exceeds).
:- forall(retract((tmp:dont_change(_):-true)),true).

:- dynamic(tmp:replacement_4_wrd/3).
use_replacement_4_wrd(I,'SYM',O):- upcase_atom(I,O).
use_replacement_4_wrd(I,_,O):- upcase_atom(I,I),O=I.
use_replacement_4_wrd(I,_,O):- number_lex(I,O,_).
use_replacement_4_wrd(I,P,O):- tmp:replacement_4_wrd(I,P,O).
:- forall(retract((tmp:replacement_4_wrd(_,_,_):-true)),true).

word2jecteese(X,X):- \+ compound(X).
word2jecteese(X,''):- X \= w(_,_),!.
word2jecteese(w(W,L),S):- member(pos(P),L),upcase_atom(P,POS),lex2jecteese(W,POS,L,S).

lex2jecteese(_,_,_,_):- flag('$sentence_word',X,X+1),fail.
lex2jecteese(X,_,_,X):- \+ atom(X).
lex2jecteese(W,_,_,W):- never_change(W),!.
lex2jecteese(W,P,_,S):- use_replacement_4_wrd(W,P,S),!.
lex2jecteese(W,P,LPOS,Y):-
  findall(N-S,map_ees(W,LPOS,N,S),L),L\==[],!,random_member(N-S,L), 
  (N==''->Y='';( flag('$objecteese_word',X,X+1),atomic_list_concat([N,X,S/*,@,W*/],Y))),!,
  assert(tmp:replacement_4_wrd(W,P,Y)).
lex2jecteese(W,_,_,W):- assert(tmp:dont_change(W)).


combined_w2s([w(W1,L1),w(W2,L2),w(W3,L3)|More],[w(W12,L12),w(W3,L3)|More]):- 
         \+ atom_contains(W1,'_'),\+ atom_contains(W2,'_'), 
 POS='nnp', member(pos(POS),L1),member(pos(POS),L2), \+ member(pos(POS),L3),
 duplicate_term(L2,L12),nb_set_add(L12,L1),atomic_list_concat([W1,'_',W2],W12).

:-  flag('$objecteese_word',_,1).

cvt_to_objecteese(X,Y):- flag('$sentence_word',_,1), cvt_to_w2(X,W2), may_debug(dmsg(W2)), flatten(W2,W2F),sent_to_jecteese(W2F,Y).
sent_to_jecteese([],[]).
sent_to_jecteese(WLs,YY):- combined_w2s(WLs,W2s),!, sent_to_jecteese(W2s,YY).
sent_to_jecteese([W|WL],[Y|YY]):-!, word2jecteese(W,Y), sent_to_jecteese(WL,YY).

:- dynamic(tmp:cached_cvt_to_w2/2).
cvt_to_w2(X,W2):- \+ (is_list(X) ; (X=[H|_],is_w2(H))),!, words_of(X,W), !, cvt_to_w2(W,W2).
%cvt_to_w2(X,W2):- tmp:cached_cvt_to_w2(X,W2),!.
cvt_to_w2(X,W2):- with_output_to(string(_), into_lexical_segs(X,W2Segs),include(is_w2,W2Segs,W2).
/*
cvt_to_w2(X,W2):- spacy_lexical_segs(X,W2),!.
cvt_to_w2(X,W2):- text_to_spacy_pos(X,W2),!.
cvt_to_w2(X,W2):- text_to_best_tree_real_old(X,T),may_debug(dmsg(T)),
 tree_s_w2(T,WTwo),
 text_to_spacy_pos(X,W22),
 merge_w2s(WTwo,W22,W2),
 nop(assert(tmp:cached_cvt_to_w2(X,W2))),dmsg(X).
*/

merge_w2s(W2,[],W2):-!.
merge_w2s(W21,[W2|L2],W222):- 
  must_or_rtrace(merge_w2(W21,W2,W22)),
  merge_w2s(W22,L2,W222).

merge_w2(W21,seg(_),W21):- !.
merge_w2(W21,w(W,WL),W21):- 
 downcase_word_4_match(W,WB1),
 ignore((member(W20, W21),
  W20=w(Word,WASL),
  downcase_word_4_match(Word,WB2),
  WB1==WB2,
  \+ member(node(_),WASL),
  nb_set_add(WASL,WL))),!.


s82:- make, s811(p1(do_objecteese_test)).

:- add_history1((cls,s82)).

