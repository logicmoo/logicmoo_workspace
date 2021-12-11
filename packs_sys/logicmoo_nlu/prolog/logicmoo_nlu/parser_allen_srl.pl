:-module(parser_allen_srl, 
 [
  test_allen_srl/0,
  test_allen_srl/1,
  test_allen_srl/2,
  test_allen_srl/1,
  test_allen_srl_parse1/0,
  test_allen_srl_parse2/0,  
  foc_allen_srl_stream/2,
  text_to_allen_srl_pos/2,
  text_to_allen_srl_sents/2,
  text_to_allen_srl_segs/2,
  allen_srl_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

read_allen_srl_lines(Term, Result):- show_failure(always,allen_srl_to_w2(Term, Result)).
/*
allen_srl_lexical_segs not working on Deb 10 VMs (works on Deb 11 VMs)

allen_srl_lexical_segs(I,O):-
  spacy_lexical_segs(I,M),
  allen_srl_parse(I, S),
  merge_allen_srl(S,M,O).
*/
allen_srl_lexical_segs(I,O):-
  spacy_lexical_segs(I,O).

merge_allen_srl([],O,O):-!.
merge_allen_srl([H|T],I,O):- !, merge_allen_srl(H,I,M), merge_allen_srl(T,M,O).
merge_allen_srl(v(List),I,O):- select(o('V',Verb),List,_), !, merge_allen_srl(srl(Verb,List),I,O).
merge_allen_srl(v(_),I,I):-!. 
merge_allen_srl(srl([Verb],List),I,O):- !,merge_allen_srl(srl(Verb,List),I,O).
merge_allen_srl(srl(Verb,List),O,O):- member(w(Verb,OL),O), \+ member(allen_srl,OL),!,    
  include('\\='(o('O',_)),List,NList),
  set_pos(2,'vb',OL),
  nb_set_add(OL,[allen_srl,srl(Verb,NList)]), !.
%merge_allen_srl(S,I,O):- append(I,[S],O),!.
merge_allen_srl(_,O,O).


allen_srl_parse0(Text, LinesO) :- 
  allen_srl_parse1(Text, Lines),
  ((maybe_redo_allen_srl_parse(Text, Lines, LinesM) , LinesM\==[] ) 
    -> LinesM=LinesO ;  Lines=LinesO).


maybe_redo_allen_srl_parse(_, LinesO, LinesO):- (length(LinesO,L),alt_l:min_expected(N)) -> L>=N,!. 
maybe_redo_allen_srl_parse(Text, _, LinesO):-
  II=[_,_|_],
  words_of(Text,[CAN,_|II]), CAN==cAn,!,
  locally(alt_l:min_expected(1),allen_srl_parse0(II,LinesO)).
maybe_redo_allen_srl_parse(Text, _, LinesO):- 
  II=[_,_|_],
  words_of(Text,[_|II]),
  locally(alt_l:min_expected(1),allen_srl_parse0(II,LinesO)).
maybe_redo_allen_srl_parse(Text, _, LinesO):- 
  words_of(Text,II),
  II=[_,_|_],
  allen_srl_parseC([cAn|II],LinesO).

allen_srl_parse1(Text, LinesO):- 
  tokenize_allen_srl_string(Text,String),
  allen_srl_parse2(String, LinesO).

allen_srl_parseC(Text,Lines):-
 allen_srl_parse1(Text,LinesI),
 remove_cans(LinesI,Lines).

remove_cans([],[]).
remove_cans([srl([cAn],_)|Rest],O):- !, remove_cans(Rest,O).
remove_cans([srl(Vs,Stuff)|Rest],[srl(Vs,StuffO)|O]):-
  remove_cans(Stuff,StuffO),
  remove_cans(Rest,O).
remove_cans([o('ARGM-MOD', [cAn])|Rest],O):-!, remove_cans(Rest,O).
remove_cans([o(Vs,Stuff)|Rest],[o(Vs,StuffO)|O]):-
  remove_cans(Stuff,StuffO),
  remove_cans(Rest,O).
remove_cans([S|Rest],[S|O]):- remove_cans(Rest,O).


text_to_allen_srl_tree(Text,LExpr):-
  allen_srl_parse(Text, String),
  nop(dmsg(allen_srl_parse=String)),  
  allen_srl_to_w2(String,LExpr),
  nop(print_tree_nl(allen_srl=LExpr)).

%allen_srl_to_w2((Word,POS),[POS,Word]).
allen_srl_to_w2(w2allen_srl(I),O):- !, allen_srl_to_w2(I,O).
allen_srl_to_w2(Out, _Result):- Out==end_of_file,!,throw(allen_srl_to_w2(Out)).
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
allen_srl_get_roles(List,Words,[M|O]):- is_list(List),select(verb:Verb,List,NewList1), select(tags:Tags,NewList1,NewList), M=srl(Verb,[o('O',[])]),!,allen_srl_get_1role('O',Tags,Words,M),
  allen_srl_get_roles(NewList,Words,O).
allen_srl_get_roles(List,Words,[M|O]):- is_list(List),select(tags:Tags,List,NewList), M=v([o('O',[])]),!,allen_srl_get_1role('O',Tags,Words,M),
  allen_srl_get_roles(NewList,Words,O).
allen_srl_get_roles([V|Verbs],Words,O):- allen_srl_get_roles(V,Words,I), allen_srl_get_roles(Verbs,Words,M), flatten([I,M],O).
allen_srl_get_roles(_,_,[]).

append_store_current(Store,Type,W):- nb_set_unify(Store,o(Type,Values),OTV),append(Values,[W],NewValues),nb_setarg(2,OTV,NewValues),!.
%append_store_current(v(Store),Type,W):- last(Store,E),nb_set_unify(E,o(Type,_),OTV),nb_set_add(OTV,W),!.
%append_store_current(v(Store),Type,W):- last(Store,E),nb_set_unify(E,o(Type,Values),OTV),append(Values,[W],NewValues),nb_setarg(2,OTV,NewValues),!.
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
:- dynamic(tmp:existing_allen_srl_stream/4).
:- volatile(tmp:existing_allen_srl_stream/4).
foc_allen_srl_stream(Out,In):- thread_self(Self),tmp:existing_allen_srl_stream(Self,_,Out,In),!,clear_allen_srl_pending(In).
foc_allen_srl_stream(Out,In):- tmp:existing_allen_srl_stream(OldThread,FFid,Out,In), \+ thread_property(OldThread,running),!,
  retract(tmp:existing_allen_srl_stream(OldThread,FFid,Out,In)),
  thread_self(Self),
  assert(tmp:existing_allen_srl_stream(Self,FFid,Out,In)),!.

foc_allen_srl_stream(Out,In):-
  thread_self(Self),
  tcp_socket(Socket),
  catch((tcp_connect(Socket, 'logicmoo.org':4097),
  tcp_open_socket(Socket, StreamPair)),_,fail),
  StreamPair = In, StreamPair = Out,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  assert(tmp:existing_allen_srl_stream(Self,_,Out,In)),!.

foc_allen_srl_stream(Out,In):-
  lmconfig:space_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python3 parser_allen_srl.py -nc -cmdloop ',[]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(Out)),stdout(pipe(In)), stderr(null), process(FFid)]),!,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  sleep(1.0),
  read_until_allen_srl_notice(In,"cmdloop_Ready."),!,
  assert(tmp:existing_allen_srl_stream(Self,FFid,Out,In)).

read_until_allen_srl_notice(In,Txt):- repeat,read_line_to_string(In,Str),(Str==end_of_file;atom_contains(Str,Txt)),!.

clear_allen_srl_pending(In):- nop((read_pending_codes(In,Codes,[]),dmsg(clear_allen_srl_pending=Codes))).

:- prolog_load_context(directory,Dir), assert(lmconfig:space_py_dir(Dir)).
tokenize_allen_srl_string(Text,StrO):- any_to_string(Text,Str),  replace_in_string('\n',' ',Str,StrO).
/*
tokenize_allen_srl_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/


allen_srl_parse(Text, LinesO):- 
  locally(alt_l:min_expected(2),allen_srl_parse0(Text, LinesO)).

allen_srl_parse2(String, Lines) :- 
  once(allen_srl_parse3(String, Lines)
      ;allen_srl_parse4(String, Lines)).

try_allen_srl_stream(Out,Write):- once(catch((flush_output(Out),format(Out,'~w',[Write])),_,
  (retract(tmp:existing_allen_srl_stream(_,_,Out,_)),fail))).

% Clears if there is a dead one
allen_srl_parse3(_String, _Lines) :-
  foc_allen_srl_stream(Out,_In),
  try_allen_srl_stream(Out,''),fail.
% Reuses or Creates
allen_srl_parse3(String, Lines) :-
  foc_allen_srl_stream(Out,In),
  try_allen_srl_stream(Out,String),
  try_allen_srl_stream(Out,'\n'),
  try_allen_srl_stream(Out,''),!,
  read_allen_srl_lines(In, Lines).

% Very slow version
allen_srl_parse4(String, Lines) :- 
  lmconfig:space_py_dir(Dir),
  sformat(S,'python3 parser_allen_srl.py -nc ~q ',[String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(In))]),!,
  read_until_allen_srl_notice(In,"cmdloop_Ready."),!,
  read_allen_srl_lines(In, Lines).

test_allen_srl_parse1 :-
  String = "Can the can do the Can Can?",
  allen_srl_parse3(String, Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse1=Lines).

test_allen_srl_parse2 :-
  Text = "Can the can do the Can Can?",
  allen_srl_parse4(Text,Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse2=Lines).

test_allen_srl_parse3 :-
  Text = "Can the can do the Can Can?",
  allen_srl_parse2(Text,Lines),
  pprint_ecp_cmt(yellow,test_allen_srl_parse3=Lines).


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
test_allen_srl(1,"").
test_allen_srl(1,".").
test_allen_srl(1,"\n").

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

