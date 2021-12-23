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


chat80_test("does america contain New York ?").
chat80_test("is New York in america ?").
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




%map_ees_tag2('NN',type,'').
map_ees_word1('country',type,'').
/*
map_ees_word1('asian',type,ian).
map_ees_word1('american',object,'ian').
map_ees_word1('flow',action,'').
map_ees_word1('flows',action,'s').
map_ees_word1('kingdom',type,'').
map_ees_word1('african',type,'ish').
map_ees_word1('million','','').
map_ees_word1('million',' type','').
map_ees_word1('volta',object,'').
map_ees_word1('equator',object,'').
*/
/*
map_ees_word1('address',attrib,'').
map_ees_word1('area',attrib,'').
map_ees_word1('areas',attrib,'s').
map_ees_word1('atlantic',object,'').
map_ees_word1('baltic',object,'').
map_ees_word1('bordered',action,'ed').
map_ees_word1('borders',action,'s').
map_ees_word1('border',action,'').
map_ees_word1('capital',property,'').
map_ees_word1('china',object,'').
map_ees_word1('continent','large type','').
map_ees_word1('likes',action,'s').
map_ees_word1('largest',adjective,'est').
map_ees_word1('man','type','').
map_ees_word1('new','a adjective','').
map_ees_word1('ocean',type,'').
map_ees_word1('population',attrib,'').
map_ees_word1('rhine',object,'').
map_ees_word1('river',type,'').
map_ees_word1('smallest',type,'liest').
map_ees_word1('south',adverb,'').
map_ees_word1('united','action','ed').
*/
/*
red = value
happy = 
*/

map_ees_tag2('JJR',value,'ier').
map_ees_tag2('JJS',value,'est').
map_ees_tag2('JJ',value,'ish').
map_ees_tag2('RBR',value,'lier').
map_ees_tag2('RBS',value,'liest').
map_ees_tag2('RB',value,'ly').
map_ees_tag2('VB_NN',action,'').
map_ees_tag2('NN_JJ','value','ish').
map_ees_tag2('NNP','Object','').
map_ees_tag2('NNPS','Object','s').
map_ees_tag2('NNS',type,'s').
map_ees_tag2('NN',type,'').

map_ees_tag2('VB',action,'').
map_ees_tag2('VBN',action,'ed').
map_ees_tag2('VBD',action,'ed').
map_ees_tag2('VBP',action,''). % verb prep
map_ees_tag2('VBG',action,'ing').
map_ees_tag2('VBZ',action,'s').



map_ees_tag2a(VBG,Action,S):- 
 map_ees_tag2(VBG,Type,ING),
 map_ees_tag2b(VBG,Type,ING,Action,S).

map_ees_tag2b(VBG,action,ING,Action,S):-!,
  flag(action_num,X,X+1),
  Y is X mod 6,
  nth0(Y,[like,hate,own,ride,admire,eat],Action),
  map_ees_tag2c(VBG,ING,S).  
map_ees_tag2b(_VBG,A,B,A,B).

map_ees_tag2c(_,'','').
map_ees_tag2c(_,'s','s').
map_ees_tag2c(_,'ed','s').
map_ees_tag2c(_,'ing','s').

noun_var(neutr,type).
noun_var(human,agent).

map_ees_lex3(Wives,Var,'s'):- clex:noun_pl(Wives, _, Type),noun_var(Type,Var).
map_ees_lex3(Wife,Var,''):-   clex:noun_pl(_, Wife, Type),noun_var(Type,Var).
map_ees_lex3(Clerk,Var,''):-  clex:noun_sg(Clerk, _, Type),noun_var(Type,Var).
%map_ees_tag2(Y,'adjective','est'):-  talkdb:talk_db(superl,_,Y).
%map_ees_lex3(Y,'adjective','er'):-  talkdb:talk_db(comp,_,Y).
%map_ees_tag2(Y,'adjective',''):- talkdb:talk_db(adj,Y).
%map_ees_tag2('VBZ',attrib,' is ').


map_ees(_,LPOS,'object',S):- member(ner('COUNTRY'),LPOS),!,(member(form(pl),LPOS)->S='s';S='').
map_ees(_,LPOS,'attrib',S):- member(dep_child(prep,n(of,_)),LPOS),!,(member(form(pl),LPOS)->S='s';S='').
map_ees(W,LPOS,N,S):- ((member(pos(Pos),LPOS),upcase_atom(Pos,POS))->true;POS='unk'), 
 (map_ees_word1(W,N,S)*-> true ; map_ees_tag2a(POS,N,S) *-> true ; map_ees_lex3(W,N,S)).

% may_debug(G):- !, call(G).
%may_debug(G):- ignore((current_prolog_flag(debug,true),!,on_x_fail(in_cmt(call(G))))).
may_debug(_).

:- dynamic(tmp:dont_change/1). 
never_change(X):- det_lex(X,_,_,_).
never_change(X):- loc_pred_lex(_,X,_).
never_change(X):- modal_verb_form_aux(X,_,_,_).
never_change(X):- number_lex(X,_,_).
never_change(X):- pers_pron_lex(X,_,_,_,_).
never_change(X):- poss_pron_lex(X,_,_,_).
never_change(X):- prep_lex(X).
never_change(X):- determiners:semlex_det(X,_,_,Z)->Z\=lam(B, lam(C, merge(merge(D:drs([D:X:E], []), app(B, E)), app(C, E)))).
never_change(not).  
never_change(no).
never_change(X):- sub_var(X,v(quantity,
      'n\'t', never, non, half, total, false, true, firstly, different, certain, average, years, days,
      but, and, or, nor, almost, another, second, seconds, secondly, percent, percentage, multitude,
      also, except, excluding, including, many,most,few,least,
      always, currently, exactly, instead, now,
      only, plenty, probably, so, then, too)).
never_change(X):- flexicon(numcompare,_,Z), sub_var(X,Z).
never_change(X):- flexicon(det,_,Z), sub_var(X,Z).
never_change(X):- quantifier_pron_lex(X,_,_).
never_change(X):- terminator_lex(X,_).
never_change(X):- verb_form_aux(X,_,_,_).
never_change(X):- wh_art_lex(_,X,_,_,_).
never_change(X):- wh_pron_lex(X,_).
never_change(X):- wh_rel_pron_lex(X,_).
never_change(X):- tmp:dont_change(X). 
never_change(X):- string2digit:s2d(X,_). % million
tmp:dont_change(exceeds).
:- forall(retract((tmp:dont_change(_):-true)),true).

compound_wrds([A,B]):- relations : nn(A,B,_Of), \+ is_adjective(A). % only non-composable compounds

is_adjective(X):- framenet_pos(X,adjective).

:- dynamic(tmp:replacement_4_wrd/4).
use_replacement_4_wrd(I,'SYM',O):- upcase_atom(I,O).
use_replacement_4_wrd(I,_,O):- upcase_atom(I,I),O=I.
use_replacement_4_wrd(I,_,O):- number_lex(I,O,_).
use_replacement_4_wrd(I,P,O):- tmp:replacement_4_wrd(I,P,_,O).
:- forall(retract((tmp:replacement_4_wrd(_,_,_,_):-true)),true).

word2jecteese(_,X,X):- \+ compound(X).
word2jecteese(_,X,''):- X \= w(_,_),!.
word2jecteese(Type,w(W,L),Out):- !, word2jecteese4(Type,W,L,Out).

word2jecteese4(w2,W,L,w(S,L)):- !, word2jecteese3(W,L,S).
word2jecteese4(_,W,L,S):-word2jecteese3(W,L,S).

word2jecteese3(W,L,S):- member(pos(P),L),upcase_atom(P,POS),lex2jecteese(W,POS,L,S).

cvt_to_ace_pos(Var,O):- var(Var),!,freeze(Var,cvt_to_ace_pos(Var,O)).
cvt_to_ace_pos(I,O):- is_list(I), maplist(cvt_to_ace_pos,I,O).
cvt_to_ace_pos(w(I,L),O):-  member(pos(P),L),!,cvt_to_ace_pos(wp(I,P,L),O).
cvt_to_ace_pos(w(I,_),O):-  !, cvt_to_ace_pos(I,O).
cvt_to_ace_pos(wp(I,P,L),O):- ensure_ace_knows_l(L,I,P,O),!.
cvt_to_ace_pos(wp(I,_,_),O):- !, cvt_to_ace_pos(I,O).
cvt_to_ace_pos(I,I):- functionwords:functionword(I),!.
cvt_to_ace_pos(I,O):- I=O.



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
  (show_failure(always,check_answer(X,Y,Z,true))-> true ; 
    (current_prolog_flag(debug,true)-> false ; locally(set_prolog_flag(debug,true),cvt_to_objecteese(X,_)))).

%do_objecteese_test(X):- try_ace_drs(X),!.
do_objecteese_test(X):- 
  try_ace_drs(X),
  cvt_to_objecteese_ace(X,Y),!,nl,any_to_string(Y,Z),
  format('~N test_objecteese(~n   ~q,~n  ~q).~n',[X,Z]),!,  
  %ignore((try_ace_drs(X,XX),exec_fol(ace=XX))),
  ignore((try_ace_drs(Z,A),(replace_back_words(A,B)->exec_fol(replace=(B));exec_fol(A)))).



replace_back_words(Var,O):- var(Var),!,O=Var. %freeze(Var,replace_back_words(Var,O)).
replace_back_words(I,O):- tmp:replacement_4_wrd(_,_,L,I),member(root(O),L).
replace_back_words(I,O):- tmp:replacement_4_wrd(O,_,_,I).
replace_back_words(I,O):- is_list(I),!,maplist(replace_back_words,I,O).
replace_back_words(I,O):- compound(I), compound_name_arguments(I,N,A),maplist(replace_back_words,A,AO),compound_name_arguments(O,N,AO),!.
replace_back_words(I,O):- I=O.


ensure_ace_knows_l(L,_,_,O):- member(truecase('UPPER'),L),member(txt(Text),L),!,upcase_atom(Text,O).
ensure_ace_knows_l(_,I,P,O):- ensure_ace_knows(I,P,O),!.

ensure_ace_knows(I,'.',I):-!.
ensure_ace_knows(I,I,I):-!.
ensure_ace_knows(I,'sym',O):-!,upcase_atom(I,O).
ensure_ace_knows(I,P,O):- atom_length(P,1),!,atomic_list_concat([P,':',I],'',O).
ensure_ace_knows(I,_,I):- functionwords:functionword(I),!.
ensure_ace_knows(I,_,I):- !.
ensure_ace_knows(I,P,O):- atom_concat('n',_,P),!,ensure_ace_knows(I,'n',O).
ensure_ace_knows(I,P,O):- atom_concat('v',_,P),!,ensure_ace_knows(I,'v',O).
ensure_ace_knows(I,P,O):- atom_concat('j',_,P),!,ensure_ace_knows(I,'a',O).
ensure_ace_knows(I,P,O):- atom_concat('rbr',_,P),!,ensure_ace_knows(I,'a',O).
ensure_ace_knows(I,P,O):- atom_concat('rbs',_,P),!,ensure_ace_knows(I,'a',O).
%ensure_ace_knows(I,P,O):- atom_concat('r',_,P),!,ensure_ace_knows(I,'a',O).
% C:\opt\logicmoo_workspace\packs_sys\logicmoo_utils\prolog\ ;C:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\prolog\;C:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\ape\;C:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\chat80\original\;<Buffers>

lex2jecteese(_,_,_,_):- flag('$sentence_word',X,X+1),fail.
lex2jecteese(X,_,_,X):- \+ atom(X).
lex2jecteese(W,_,_,W):- never_change(W),!.
lex2jecteese(W,P,_,S):- use_replacement_4_wrd(W,P,S),!.
lex2jecteese(W,P,LPOS,Y):-
  findall(N-S,map_ees(W,LPOS,N,S),L),L\==[],!,random_member(N-S,L), 
  (N==''->Y='';( flag('$objecteese_word',X,X+1),atomic_list_concat([N,X,S/*,@,W*/],Y))),!,
  assert(tmp:replacement_4_wrd(W,P,LPOS,Y)).
lex2jecteese(W,_,_,W):- assert(tmp:dont_change(W)).


combined_w2s([w(W1,L1),w(W2,L2),w(W3,L3)|More],[w(W12,L12),w(W3,L3)|More]):- 
         \+ atom_contains(W1,'_'),\+ atom_contains(W2,'_'), 
 POS='nnp', member(pos(POS),L1),member(pos(POS),L2), \+ member(pos(POS),L3),
 duplicate_term(L2,L12),nb_set_add(L12,L1),atomic_list_concat([W1,'_',W2],W12).

:-  flag('$objecteese_word',_,1).

cvt_to_objecteese(X,Y):- cvt_to_objecteese(w2,X,W2),maplist(arg(1),W2,Y).
cvt_to_objecteese_ace(X,Y):- cvt_to_objecteese(w2,X,W2),cvt_to_ace_pos(W2,Y).
cvt_to_objecteese(Type,X,Y):- flag('$sentence_word',_,1), cvt_to_w2(X,W2), may_debug(dmsg(W2)), flatten(W2,W2F),sent_to_jecteese(Type,W2F,Y).
sent_to_jecteese(_,[],[]).
sent_to_jecteese(Type,WLs,YY):- combined_w2s(WLs,W2s),!, sent_to_jecteese(Type,W2s,YY).
sent_to_jecteese(Type,[W|WL],[Y|YY]):-!, word2jecteese(Type,W,Y), sent_to_jecteese(Type,WL,YY).

:- dynamic(tmp:cached_cvt_to_w2/2).
cvt_to_w2(X,W2):- \+ (is_list(X) ; (X=[H|_],is_w2(H))),!, words_of(X,W), !, cvt_to_w2(W,W2).
%cvt_to_w2(X,W2):- tmp:cached_cvt_to_w2(X,W2),!.
cvt_to_w2(X,W2):- into_lexical_segs(X,W2Segs),include_is_w2(W2Segs,W2).
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


o85:- make, locally(set_prolog_flag(debug,true),o811(p1(do_objecteese_test))).

o84:- make,o81(o84).
o84(X):- any_to_ace_str(X,S),string_codes(S,Codes), Out = current_output,
	catch(
		run_acerules:(
      no_debug,
			generate_output(Codes, court, [], _, _, Trace, _),
			member(final(InferenceSteps, _), Trace),
			verbalize_trace(Trace, VerbTrace),
			print_ace_trace(Out, 0, InferenceSteps, VerbTrace)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)).

o85(X):- any_to_ace_str(X,S),string_codes(S,Codes), Out = current_output,
	ignore((catch(
		run_acerules:( % court, stable, or stable_strong.
                  debug,
      Semantics = court,
			generate_output(Codes, Semantics, [maxanswers=10], Rules, Answersets, Trace, AnswerTexts),
			simple_rules(Rules, SimpleRules),
      verbalize_trace(Trace, VerbTrace),
      member(final(InferenceSteps, _), Trace),
      print_ace_trace(Out, 0, InferenceSteps, VerbTrace),
			print_normal(Out, Codes, Rules, SimpleRules, Answersets, AnswerTexts)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)
	))),!.

:- add_history((cls,debug,o82)).
:- fixup_exports.
