:-module(parser_flair, 
 [
  test_flair/0,
  test_flair/1,
  test_flair/2,
  test_flair/1,
  test_flair_parse1/0,
  test_flair_parse2/0,  
  foc_flair_stream/2,
  text_to_flair_pos/2,
  text_to_flair_sents/2,
  text_to_flair_segs/2,
  flair_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

read_flair_lines(In, Result):- flair_to_w2(In, Result),!.

text_to_flair_tree(Text,LExpr):-
  flair_parse(Text, String),
  nop(dmsg(flair_parse=String)),  
  flair_to_w2(String,LExpr),
  nop(print_tree_nl(flair=LExpr)).

%flair_to_w2((Word,POS),[POS,Word]).
flair_to_w2(In, Result):- var(In),!,throw(var_flair_to_w2(In, Result)).
flair_to_w2('', Result):-!, Result=[],!.
flair_to_w2(w2flair(List),ListO):- !, flair_to_w2(List,ListO).
flair_to_w2(List,ListO):- is_list(List),!,include(compound,List,ListO).
flair_to_w2(In, Result):- is_stream(In),!,flair_stream_to_w2(In,'', Result).
flair_to_w2(Text,ListO):- \+ compound(Text), on_x_fail(atom_to_term(Text,Term,_)),!,flair_to_w2(Term,ListO).
flair_to_w2(Text,_ListO):- \+ compound(Text), nl,writeq(Text),nl,!,fail.

flair_stream_to_w2(_, S, Result):- atom_contains(S,"w2flair([])."),!,Result=[].
flair_stream_to_w2(In,_, Result):- peek_string(In,10,S),atom_contains(S,"w2flair("),!,read_term(In,Term,[]),flair_to_w2(Term, Result).
flair_stream_to_w2(In,S, Result):- atom_contains(S,"w2flair("),!,read_term_from_atom_rest(In,S,Term),flair_to_w2(Term, Result).
flair_stream_to_w2(In,S, Result):- at_end_of_stream(In),!,flair_to_w2(S, Result).
flair_stream_to_w2(In,_, Result):- repeat, read_pending_codes(In,Codes,[]),
 (Codes==[]->(sleep(0.1),fail);true),sformat(S,'~s',[Codes]),
 flair_stream_to_w2(In,S, Result).

read_term_from_atom_rest(_,S, Result):- atom_contains(S,"w2flair([])."),!,Result=[].
read_term_from_atom_rest(In,S,Term):- atom_concat(L,'\n',S),!,read_term_from_atom_rest(In,L,Term).
read_term_from_atom_rest(In,S,Term):- atom_concat(L,' ',S),!,read_term_from_atom_rest(In,L,Term).
read_term_from_atom_rest(_,S,Term):-  atom_concat(_,'.',S),!,read_term_from_atom(S,Term,[]).
read_term_from_atom_rest(In,S,Term):- read_pending_codes(In,Codes,[]),
  (Codes==[]->(sleep(0.1),fail);true),sformat(S2,'~s~s',[S,Codes]),
  read_term_from_atom_rest(In,S2,Term).


flair_lexical_segs(I,O):-
  allen_srl_lexical_segs(I,M),
  flair_parse_or_skip(I,S),
  merge_flair(S,M,O),!.

%flair_parse_or_skip(I,O):- catch(flair_parse(I,O),_,fail),nonvar(O),!.
flair_parse_or_skip(_,[]).


merge_flair([],O,O):-!.
merge_flair([H|T],I,O):- !, merge_flair(H,I,M), merge_flair(T,M,O).
merge_flair(w(W,L),O,O):- member(w(W,OL),O), \+ member(flair,OL),!,    
  ignore((member(upos(Pos),L),downcase_atom(Pos,DPos),upos_to_penn_pos(DPos,Penn),set_pos(2,Penn,OL))),
  nb_set_add(OL,[flair|L]), !.
merge_flair(_,I,I):-!.

upos_to_penn_pos(verb,vb).
%upos_to_penn_pos(adv,rb). other systems produce better specilizations
upos_to_penn_pos(adj,jj).
upos_to_penn_pos(propn,nnp).
%upos_to_penn_pos(noun,nn). other systems might detect a verb this one doesnt (so we dont overwrite)
upos_to_penn_pos(cconj,cc).



:- dynamic(tmp:existing_flair_stream/4).
:- volatile(tmp:existing_flair_stream/4).
foc_flair_stream(Out,In):- thread_self(Self),tmp:existing_flair_stream(Self,_,Out,In),!,clear_flair_pending(In).
foc_flair_stream(Out,In):- tmp:existing_flair_stream(OldThread,FFid,Out,In), \+ thread_property(OldThread,status(running)),!,
  retract(tmp:existing_flair_stream(OldThread,FFid,Out,In)),
  thread_self(Self),
  assert(tmp:existing_flair_stream(Self,FFid,Out,In)),!.

foc_flair_stream(Out,In):-
  thread_self(Self),
  tcp_socket(Socket),
  catch((tcp_connect(Socket, 'logicmoo.org':4095),
  tcp_open_socket(Socket, StreamPair)),_,fail),
  StreamPair = In, StreamPair = Out,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  assert(tmp:existing_flair_stream(Self,_,Out,In)),!.

foc_flair_stream(Out,In):- current_prolog_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python3 parser_flair.py -nc -cmdloop ',[]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(Out)),stdout(pipe(In)), stderr(null), process(FFid)]),!,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  sleep(1.0),
  read_until_flair_notice(In,"cmdloop_Ready."),!,
  assert(tmp:existing_flair_stream(Self,FFid,Out,In)).

read_until_flair_notice(In,Txt):- repeat,read_line_to_string(In,Str),(Str==end_of_file;atom_contains(Str,Txt)),!.

clear_flair_pending(In):- nop((read_pending_codes(In,Codes,[]),dmsg(clear_flair_pending=Codes))).

:- prolog_load_context(directory,Dir), assert(lmconfig:space_py_dir(Dir)).

tokenize_flair_string(Text,StrO):- any_to_string(Text,Str),  replace_in_string('\n',' ',Str,StrO).
/*
tokenize_flair_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/


flair_parse(Text, Lines) :- 
  tokenize_flair_string(Text,String),
  flair_parse2(String, Lines).

flair_parse2(String, Lines) :- 
  once(flair_parse3(String, Lines)
      ;flair_parse4(String, Lines)).

try_flair_stream(Out,Write):- 
 once(catch((format(Out,'~w',[Write])),_, (retract(tmp:existing_flair_stream(_,_,Out,_)),fail))).

% Clears if there is a dead one
flair_parse3(_String, _Lines) :- fail,
  foc_flair_stream(Out,_In),
  try_flair_stream(Out,''),fail.
% Reuses or Creates
flair_parse3(String, Lines) :-
  foc_flair_stream(Out,In),
  try_flair_stream(Out,String),
  try_flair_stream(Out,'\n'),
  try_flair_stream(Out,''),!,
  flush_output(Out),
  read_flair_lines(In, Lines).

% Very slow version
flair_parse4(String, Lines) :- current_prolog_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  sformat(S,'python3 parser_flair.py -nc ~q ',[String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(In))]),!,
  read_until_flair_notice(In,"cmdloop_Ready."),!,
  read_flair_lines(In, Lines).

test_flair_parse1 :-
  String = "Can the can do the Can Can?",
  flair_parse3(String, Lines),
  pprint_ecp_cmt(yellow,test_flair_parse1=Lines).

test_flair_parse2 :-
  Text = "Can the can do the Can Can?",
  flair_parse4(Text,Lines),
  pprint_ecp_cmt(yellow,test_flair_parse2=Lines).

test_flair_parse3 :-
  Text = "Can the can do the Can Can?",
  flair_parse2(Text,Lines),
  pprint_ecp_cmt(yellow,test_flair_parse3=Lines).

   
flair_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_flair_sents(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.

text_to_flair_pos(Text,PosW2s):- flair_parse(Text,PosW2s),!.
text_to_flair_pos(Text,PosW2s):- flair_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.
  
text_to_flair_segs(Text,Segs):-
  text_to_flair_tree(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_flair_sents(Text,Sent):-
  text_to_flair_segs(Text,Segs),!,
  flair_segs_to_sentences(Segs,Sent),!.

flair_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).


:- if( \+ getenv('keep_going','-k')).
:- use_module(library(editline)).
:- add_history((call(make),call(test_flair1))).
:- endif.

baseKB:regression_test:- test_flair(1,X),!,test_flair(X).
baseKB:sanity_test:- make, forall(test_flair(1,X),test_flair(X)).
baseKB:feature_test:- test_flair.

test_flair0:- 
  Txt = "PERSON1 asks : Hey , what 's going on XVAR. < p >. PERSON2 said : Not a whole lot . . < p >. PERSON2 said : I 'm looking forward to the weekend , though . . < p >. PERSON1 asks : Do you have any big plans XVAR. < p >. PERSON2 said : Yes . . < p >. PERSON2 said : I 'm going to Wrigley Field on Saturday . . < p >. PERSON1 asks : Aren 't the Cubs out of town XVAR. < p >. PERSON2 said : Yes , but there 's a big concert at Wrigley this weekend . . < p >. PERSON1 said : Oh nice . . < p >. PERSON1 asks : Who 's playing XVAR. < p >. PERSON2 said : Pearl Jam is headlining the Saturday night show . . < p >. PERSON1 said : Wow , Pearl Jam . . < p >. PERSON1 said : I remeber when I got their first CD , Ten , at the record store at Harlem and Irving Plaza . . < p >. PERSON2 said : Oh right . . < p >. PERSON2 said : I remember that record store . . < p >. PERSON1 said : It was called Rolling Stone , and they went out of business many years ago . . < p >. PERSON2 said : Oh that 's too bad . . < p >. PERSON2 said : I really loved taking the bus to Harlem and Irving and visiting that store . . < p >. PERSON1 said : Me too . . < p >. PERSON1 said : We did n't have the internet back then and had to discover new music the hard way . . < p >. PERSON2 said : Haha yes . . < p >. PERSON2 said : I remember discovering ' ' Nirvana before they got famous . . < p >. PERSON1 said : Those were the good old days . . < p >. PERSON2 said : Yes they were . . < p >. PERSON2 said : I need to dig up my old Sony disc player and pop in an old CD . . < p >. PERSON1 asks : Where did the time go XVAR. < p >. PERSON1 said : Pearl Jam is 25 years old already . . < p >. PERSON2 said : It seems like only yesterday that the grunge music movement took over . . < p >. PERSON1 said : Right . . < p >. PERSON1 said : I bet everyone at the concert will be in their forty 's . . < p >. PERSON2 said : No doubt . . < p >. PERSON2 said : Well , I hope you have a great time at the concert . . < p > .",
  test_flair(Txt),
  ttyflush,writeln(('\n test_flair0.')),!.

test_flair1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_flair(Txt),
  ttyflush,writeln(('\n test_flair1.')),!.
test_flair2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_flair(Txt),
  ttyflush,writeln(('\n test_flair2.')),!.

test_flair:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_flair(Txt),
  ttyflush,writeln(('\n test_flair.')),
  fail.
test_flair:- forall(test_flair(X),test_flair(X)).

test_1flair(Text):- 
  format('~N?- ~p.~n',[test_flair(Text)]),
  text_to_flair_tree(Text,W),
  print_tree_nl(W),
  !.
test_1flair(Text):- wdmsg(failed(test_1flair(Text))).

test_flair(N):- number(N),!, forall(test_flair(N,X),test_1flair(X)). 
test_flair(X):- test_flair(_,X),nop(lex_info(X)).

test_flair(_,X):- nonvar(X), !, once(test_1flair(X)).

test_flair(1,".\nThe Norwegian lives in the first house.\n.").
test_flair(1,"").
test_flair(1,".").
test_flair(1,"\n").

test_flair(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_flair(2,Each):- test_flair(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_flair(3,
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


test_flair(4,".
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

:- add_history(test_flair).
:- fixup_exports.

