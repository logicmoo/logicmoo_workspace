:-module(parser_allen_srl, 
 [
  test_allen_srl/0,
  test_allen_srl/1,
  test_allen_srl/2,
  test_allen_srl/1,
  test_allen_srl_parse1_broken/0,
  test_allen_srl_parse2/0,  
  allen_srl_stream/2,
  text_to_allen_srl_pos/2,
  text_to_allen_srl_sents/2,
  text_to_allen_srl_segs/2,
  allen_srl_parse/2]).

:- use_module(library(http/json)).
:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

:- dynamic(tmp:existing_allen_srl_stream/4).
:- volatile(tmp:existing_allen_srl_stream/4).
allen_srl_stream(In,Out):- thread_self(Self),tmp:existing_allen_srl_stream(Self,_,In,Out),!,clear_allen_srl_pending(Out).
allen_srl_stream(In,Out):-
  lmconfig:space_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python3 parser_allen_srl.py -nc -cmdloop ',[]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(In)),stdout(pipe(Out)), stderr(null), process(FFid)]),!,
  assert(tmp:existing_allen_srl_stream(Self,FFid,In,Out)).

clear_allen_srl_pending(Out):- nop((read_pending_codes(Out,Codes,[]),dmsg(clear_allen_srl_pending=Codes))).

:- prolog_load_context(directory,Dir), assert(lmconfig:space_py_dir(Dir)).

tokenize_allen_srl_string(Text,StrO):- any_to_string(Text,StrO).
/*
tokenize_allen_srl_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/
allen_srl_lexical_segs(I,O):-
  spacy_lexical_segs(I,M),
  allen_srl_parse(I,S),
  merge_allen_srl(S,M,O).

merge_allen_srl([],O,O):-!.
merge_allen_srl([H|T],I,O):- !, merge_allen_srl(H,I,M), merge_allen_srl(T,M,O).
merge_allen_srl(w(W,L),O,O):- member(w(W,OL),O), \+ member(allen_srl,OL),!,    
  select(pos(Pos),L,ML), 
  downcase_atom(Pos,DPos),
  ignore((member(pos(OLD),OL), OLD\==DPos, remove_el_via_setarg(OL,pos(OLD)), nb_set_add(OL,old_pos(OLD)))),
  nb_set_add(OL,[allen_srl,pos(DPos)|ML]), !.
merge_allen_srl(span(List),I,O):- member(dep_tree(_,_,_),List),!,
  merge_allen_srl(List,I,O),!.
merge_allen_srl(span(List),O,O):- 
  member(seg(S,E),List), member(span(Other),O), member(seg(S,E),Other),
  nb_set_add(Other,[allen_srl|List]).
merge_allen_srl(dep_tree(Type,R,Arg),O,O):- 
  member(w(_,Other),O),member(node(R),Other),
  nb_set_add(Other,dep_tree(Type,R,Arg)).
merge_allen_srl(S,I,O):- append(I,[S],O).



allen_srl_parse(Text, Lines) :- 
  tokenize_allen_srl_string(Text,String),
  allen_srl_parse2(String, Lines).

allen_srl_parse2(_String, _Lines) :-
  allen_srl_stream(In,Out),
  once(catch(flush_output(In),_,retract(tmp:existing_allen_srl_stream(_,_,In,Out)))),fail.
allen_srl_parse2(_String, _Lines) :-
  allen_srl_stream(In,Out),
  once(catch(flush_output(In),_,retract(tmp:existing_allen_srl_stream(_,_,In,Out)))),fail.
allen_srl_parse2(String, Lines) :-
  allen_srl_stream(In,Out),
  format(In,'~w\n',[String]),flush_output(In),
  read_term(Out,Term,[]),!,
  read_allen_srl_lines(Term, Lines).
allen_srl_parse2(String, Lines) :- 
  lmconfig:space_py_dir(Dir),
  sformat(S,'python3 parser_allen_srl.py -nc ~q ',[String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(Out))]),!,
  read_allen_srl_lines(Out, Lines).

test_allen_srl_parse1_broken :-
 Text = "Can the can do the Can Can?",
  allen_srl_stream(In,Out),
  format(In,'~w\n',[Text]),
  flush_output(In),
  read_allen_srl_lines(Out, Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse1=Lines).

test_allen_srl_parse2 :-
  Text = "Can the can do the Can Can?",
  allen_srl_parse(Text,Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse2=Lines).

test_allen_srl_parse3 :-
  Text = "Can the can do the Can Can?",
  text_to_allen_srl_pos(Text,Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse2=Lines).


   
allen_srl_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_allen_srl_sents(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.

text_to_allen_srl_pos(Text,PosW2s):- allen_srl_parse(Text,PosW2s),!.
text_to_allen_srl_pos(Text,PosW2s):- allen_srl_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.
  
text_to_allen_srl_segs(Text,Segs):-
  text_to_allen_srl_tree(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_allen_srl_sents(Text,Sent):-
  text_to_allen_srl_segs(Text,Segs),!,
  allen_srl_segs_to_sentences(Segs,Sent),!.

allen_srl_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).

read_allen_srl_lines(Term, Result):- show_failure(always,allen_srl_to_w2(Term, Result)).

text_to_allen_srl_tree(Text,LExpr):-
  allen_srl_parse(Text, String),
  nop(dmsg(allen_srl_parse=String)),  
  allen_srl_to_w2(String,LExpr),
  nop(print_tree_nl(allen_srl=LExpr)).

%allen_srl_to_w2((Word,POS),[POS,Word]).
allen_srl_to_w2(w2allen_srl(I),O):- !, allen_srl_to_w2(I,O).
allen_srl_to_w2(Out, Result):- is_stream(Out),!,read_term(Out,Term,[]),allen_srl_to_w2(Term, Result).
%allen_srl_to_w2(Out, Result):- is_stream(Out),!,json_read(Out,Term,[]),allen_srl_to_w2(Term, Result).
%allen_srl_to_w2(List,ListO):- is_list(List),!,include(compound,List,ListO).
allen_srl_to_w2(I,O):- (string(I); atom(I)), on_x_fail(read_term_from_atom(I,Term,[])), !, allen_srl_to_w2(Term,O).
allen_srl_to_w2(I,O):- (string(I); atom(I)), on_x_fail((text_to_string(I,Str), atom_string(Atom,Str), atom_json_term(Atom,Term,[]))),!, 
 allen_srl_to_w2(Term,O).
allen_srl_to_w2(I,O):- compound(I),show_failure(always,allen_srl_to_data(I,M)),!,show_failure(always,allen_srl_to_tree(M,O)),!.
allen_srl_to_w2(I,O):- I=O,!.
%allen_srl_to_w2(I,ListO):- \+ compound(I), on_x_fail(atom_to_term(I,Term,_)),!,allen_srl_to_w2(Term,ListO).
%allen_srl_to_w2(I,_ListO):- \+ compound(I), nl,writeq(I),nl,!,fail.

allen_srl_to_tree(List,O) :- is_list(List),member( words : Words, List),member( verbs : Verbs, List),!,
  show_failure(always,allen_srl_get_roles(Verbs,Words,O)).
allen_srl_to_tree(Text,ListO):- Text=ListO,!.

allen_srl_get_roles([],_,[]):-!.
allen_srl_get_roles(List,Words,[M|O]):- is_list(List), select(tags:Tags,List,NewList), M=v([o('O',[])]),!,allen_srl_get_1role('O',Tags,Words,M),
  allen_srl_get_roles(NewList,Words,O).
allen_srl_get_roles([V|Verbs],Words,O):- allen_srl_get_roles(V,Words,I), allen_srl_get_roles(Verbs,Words,M), flatten([I,M],O).
allen_srl_get_roles(_,_,[]).

append_store_current(Store,Type,W):- nb_set_unify(Store,o(Type,Values),OTV),append(Values,[W],NewValues),nb_setarg(2,OTV,NewValues),!.
append_store_current(v(Store),Type,W):- last(Store,E),nb_set_unify(E,o(Type,_),OTV),nb_set_add(OTV,W),!.
append_store_current(v(Store),Type,W):- last(Store,E),nb_set_unify(E,o(Type,Values),OTV),append(Values,[W],NewValues),nb_setarg(2,OTV,NewValues),!.
append_store_current(Store,Type,W):- nb_set_add(Store,o(Type,[W])),!.

append_store(Store,Type,W):- nb_set_add(Store,o(Type,[W])),!.

allen_srl_get_1role(_,[],[],_):-!.
allen_srl_get_1role('O',['O'|Tags],[W|Words],Store):- append_store_current(Store,'O',W),!, allen_srl_get_1role('O',Tags,Words,Store).
allen_srl_get_1role(_,['O'|Tags],[W|Words],Store):-  append_store(Store,'O',W),!, allen_srl_get_1role('O',Tags,Words,Store).
allen_srl_get_1role(_,[T|Tags],[W|Words],Store):- atom_concat('B-',Kind,T),append_store(Store,Kind,W),!,
  atom_concat('I-',Kind,NewType), allen_srl_get_1role(NewType,Tags,Words,Store).
allen_srl_get_1role(Type,[Type|Tags],[W|Words],Store):- 
  atom_concat('I-',Kind,Type),append_store_current(Store,Kind,W),
  allen_srl_get_1role(Type,Tags,Words,Store).


allen_srl_to_data(I,ListO):- \+ compound(I),!,I=ListO,!.
allen_srl_to_data(List,O):- is_list(List),!, maplist(allen_srl_to_data,List,O).
allen_srl_to_data({I},O):-!,allen_srl_to_data(I,O).
allen_srl_to_data((A,B),O):-!,allen_srl_to_data(A,AA),allen_srl_to_data(B,BB),flatten([AA,BB],AB),allen_srl_to_data(AB,O),!.
allen_srl_to_data((A:B),A:O):- !,allen_srl_to_data(B,BB),flatten([BB],AB),allen_srl_to_data(AB,O),!.
allen_srl_to_data(Text,ListO):- Text=ListO,!.

is_upper_allen_srl_letters_atom(S):- atom(S),upcase_atom(S,S), \+ downcase_atom(S,S).

/*


dep_tree_to_tree([W2|W2Segs],Tree):-
  arg(2,W2,List),member(node(N1),List),
  partition(is_w2,W2Segs,W2Only,Excluded),
  findall(dep_tree(P,S,O),(member(seg(L),Excluded),member(dep_tree(P,S,O))),List),
  make_tree(W2Only,[],List,Tree).

make_tree(N1,L,Rest,Tree):- 
  select(dep_tree(P,PN,N1),Rest,Todo),!,
  append(L,[[PN,N1]],LL),
  make_tree(N1,LL,Todo,Tree).

make_tree(N1,L,Rest,Tree):- 
  make_tree(N1,[],Rest,Tree),
  append(L,[[PN,N1]],LL),
  make_tree(N1,LL,Todo,Tree).
*/


unused_allen_srl('{\'}').
unused_allen_srl('{!}').
unused_allen_srl('}').
unused_allen_srl('{').

correct_allen_srl_atom(S,A,O):- \+ atom(A),!,correct_allen_srl_tree(S,A,O).
correct_allen_srl_atom(S,A,O):- correct_allen_srl_atom0(S,A,M),correct_allen_srl_atom1(S,M,O).
%correct_allen_srl_atom(S,A,[UP,WordO]):- atomic_list_concat([Word,POS],'.',A),atomic_list_concat([S,POS],'-',U),upcase_atom(U,UP),correct_allen_srl_sub_atom(POS,Word,WordO).
correct_allen_srl_atom0(_,A,[UP,WordO]):- atomic_list_concat([Word,POS],'.',A),Word\=='',POS\=='',upcase_atom(POS,UP0),atomic_list_concat([UP0,'w'],'-',UP),correct_allen_srl_sub_atom(POS,Word,WordO).
correct_allen_srl_atom0(PT,A,[UP,WordO]):- atomic_list_concat([PT,'w'],'-',UP),correct_allen_srl_sub_atom(PT,A,WordO).

correct_allen_srl_atom1(_,['S-w','.'],['.','.']):-!.
correct_allen_srl_atom1(_,O,O).

correct_allen_srl_sub_atom(POS,Word,WordO):- unused_allen_srl(X),atomic_list_concat([W1,W2|Ws],X,Word),atomic_list_concat([W1,W2|Ws],'',WordM),WordM\=='',correct_allen_srl_sub_atom(POS,WordM,WordO).
correct_allen_srl_sub_atom(_POS,Word,Word).

:- if( \+ getenv('keep_going','-k')).
:- use_module(library(editline)).
:- add_history((call(make),call(test_allen_srl1))).
:- endif.

baseKB:regression_test:- test_allen_srl(1,X),!,test_allen_srl(X).
baseKB:sanity_test:- make, forall(test_allen_srl(1,X),test_allen_srl(X)).
baseKB:feature_test:- test_allen_srl.

test_allen_srl0:- 
  Txt = "PERSON1 asks : Hey , what 's going on XVAR. < p >. PERSON2 said : Not a whole lot . . < p >. PERSON2 said : I 'm looking forward to the weekend , though . . < p >. PERSON1 asks : Do you have any big plans XVAR. < p >. PERSON2 said : Yes . . < p >. PERSON2 said : I 'm going to Wrigley Field on Saturday . . < p >. PERSON1 asks : Aren 't the Cubs out of town XVAR. < p >. PERSON2 said : Yes , but there 's a big concert at Wrigley this weekend . . < p >. PERSON1 said : Oh nice . . < p >. PERSON1 asks : Who 's playing XVAR. < p >. PERSON2 said : Pearl Jam is headlining the Saturday night show . . < p >. PERSON1 said : Wow , Pearl Jam . . < p >. PERSON1 said : I remeber when I got their first CD , Ten , at the record store at Harlem and Irving Plaza . . < p >. PERSON2 said : Oh right . . < p >. PERSON2 said : I remember that record store . . < p >. PERSON1 said : It was called Rolling Stone , and they went out of business many years ago . . < p >. PERSON2 said : Oh that 's too bad . . < p >. PERSON2 said : I really loved taking the bus to Harlem and Irving and visiting that store . . < p >. PERSON1 said : Me too . . < p >. PERSON1 said : We did n't have the internet back then and had to discover new music the hard way . . < p >. PERSON2 said : Haha yes . . < p >. PERSON2 said : I remember discovering ' ' Nirvana before they got famous . . < p >. PERSON1 said : Those were the good old days . . < p >. PERSON2 said : Yes they were . . < p >. PERSON2 said : I need to dig up my old Sony disc player and pop in an old CD . . < p >. PERSON1 asks : Where did the time go XVAR. < p >. PERSON1 said : Pearl Jam is 25 years old already . . < p >. PERSON2 said : It seems like only yesterday that the grunge music movement took over . . < p >. PERSON1 said : Right . . < p >. PERSON1 said : I bet everyone at the concert will be in their forty 's . . < p >. PERSON2 said : No doubt . . < p >. PERSON2 said : Well , I hope you have a great time at the concert . . < p > .",
  test_allen_srl(Txt),
  ttyflush,writeln(('\n test_allen_srl0.')),!.

test_allen_srl1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_allen_srl(Txt),
  ttyflush,writeln(('\n test_allen_srl1.')),!.
test_allen_srl2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_allen_srl(Txt),
  ttyflush,writeln(('\n test_allen_srl2.')),!.

test_allen_srl:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_allen_srl(Txt),
  ttyflush,writeln(('\n test_allen_srl.')),
  fail.
test_allen_srl:- forall(test_allen_srl(X),test_allen_srl(X)).

test_1allen_srl(Text):- 
  format('~N?- ~p.~n',[test_allen_srl(Text)]),
  text_to_allen_srl_tree(Text,W),
  print_tree_nl(W),
  !.
test_1allen_srl(Text):- wdmsg(failed(test_1allen_srl(Text))).

test_allen_srl(N):- number(N),!, forall(test_allen_srl(N,X),test_1allen_srl(X)). 
test_allen_srl(X):- test_allen_srl(_,X),nop(lex_info(X)).

test_allen_srl(_,X):- nonvar(X), !, once(test_1allen_srl(X)).

test_allen_srl(1,".\nThe Norwegian lives in the first house.\n.").

test_allen_srl(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_allen_srl(2,Each):- test_allen_srl(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_allen_srl(3,
'There are 5 houses with five different owners.
 These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.
 No owners have the same pet, smoke the same brand of cigar or drink the same beverage.
 The man who smokes Blends has a neighbor who drinks water.
 A red cat fastly jumped onto the table which is in the kitchen of the house.
 After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.
 Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.
 Concrete beams overhead had been hand-painted to vaguely resemble blond oak.
 The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.
 Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.
 A book called, "A little tribute to Gibson".
 "You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').


test_allen_srl(4,".
The Brit lives in the red house.
The Swede keeps dogs as pets.
The Dane drinks tea.
The green house is on the immediate left of the white house.
The green house's owner drinks coffee.
The owner who smokes Pall Mall rears birds.
The owner of the yellow house smokes Dunhill.
The owner living in the center house drinks milk.
The Norwegian lives in the first house.
The owner who smokes Blends lives next to the one who keeps cats.
The owner who keeps the horse lives next to the one who smokes Dunhills.
The owner who smokes Bluemasters drinks beer.
The German smokes Prince.
The Norwegian lives next to the blue house.
The owner who smokes Blends lives next to the one who drinks water.").

:- add_history(test_allen_srl).
:- fixup_exports.

