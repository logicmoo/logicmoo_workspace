:-module(parser_nlp_shared, 
 [
  test_nlp_shared/0,
  test_nlp_shared/1,
  test_nlp_shared/2,
  test_nlp_shared/1,
  test_nlp_shared_parse1/0,
  test_nlp_shared_parse2/0,  
  foc_nlp_shared_stream/2,
  text_to_nlp_shared_pos/2,
  text_to_nlp_shared_sents/2,
  text_to_nlp_shared_segs/2,
  nlp_shared_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_nlu/parser_penn_trees)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

read_nlp_shared_lines(Py,Term, Result):- 
  atom_concat(Py,'_to_w2',Proc),
  show_failure(always,call(Proc, Term, Result)).

:- dynamic(tmp:existing_nlp_shared_stream/4).
:- volatile(tmp:existing_nlp_shared_stream/4).
foc_nlp_shared_stream(Py,Out,In):- thread_self(Self),tmp:existing_nlp_shared_stream(Py,Self,_,Out,In),!,clear_nlp_shared_pending(In).
foc_nlp_shared_stream(Py,Out,In):- tmp:existing_nlp_shared_stream(Py,OldThread,FFid,Out,In), \+ thread_property(OldThread,running),!,
  retract(tmp:existing_nlp_shared_stream(Py,OldThread,FFid,Out,In)),
  thread_self(Self),
  assert(tmp:existing_nlp_shared_stream(Py,Self,FFid,Out,In)),!.

foc_nlp_shared_stream(Py,Out,In):-
  thread_self(Self),
    tcp_socket(Socket),
    tcp_connect(Socket, 'logicmoo.org':4097),
    tcp_open_socket(Socket, StreamPair),
    StreamPair = In, StreamPair = Out,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  assert(tmp:existing_nlp_shared_stream(Py,Self,_,Out,In)),!.

foc_nlp_shared_stream(Py,Out,In):- prolog_current_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  thread_self(Self),
  sformat(S,'python3 parser_~w.py -nc -cmdloop ',[Py]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir),  stdin(pipe(Out)),stdout(pipe(In)), stderr(null), process(FFid)]),!,
  set_stream(In,close_on_exec(false)),
  set_stream(Out,close_on_exec(false)),
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  set_stream(In,eof_action(eof_code)),
  set_stream(Out,eof_action(eof_code)),
  sleep(1.0),
  read_until_nlp_shared_notice(In,"cmdloop_Ready."),!,
  assert(tmp:existing_nlp_shared_stream(Py,Self,FFid,Out,In)).

read_until_nlp_shared_notice(In,Txt):- repeat,read_line_to_string(In,Str),(Str==end_of_file;atom_contains(Str,Txt)),!.

clear_nlp_shared_pending(In):- nop((read_pending_codes(In,Codes,[]),dmsg(clear_nlp_shared_pending=Codes))).

:- prolog_load_context(directory,Dir), assert(lmconfig:space_py_dir(Dir)).
tokenize_nlp_shared_string(Text,StrO):- any_to_string(Text,Str),  replace_in_string('\n',' ',Str,StrO).
/*
tokenize_nlp_shared_string(Text,StrO):- any_to_string(Text,Str), replace_in_string(['\\'='\\\\','\''='\\\''],Str,StrM),
  atomics_to_string(["'",StrM,"'"],StrO).
*/


nlp_shared_parse2(Py,String, Lines) :- 
  once(nlp_shared_parse3(Py,String, Lines)
      ;nlp_shared_parse4(Py,String, Lines)).

try_nlp_shared_stream(Out,Write):- once(catch((flush_output(Out),format(Out,'~w',[Write])),_,
  (retract(tmp:existing_nlp_shared_stream(_,_,_,Out,_)),fail))).

% Clears if there is a dead one
nlp_shared_parse3(Py,_String, _Lines) :-
  foc_nlp_shared_stream(Py,Out,_In),
  try_nlp_shared_stream(Out,''),fail.
% Reuses or Creates
nlp_shared_parse3(Py,String, Lines) :-
  foc_nlp_shared_stream(Py,Out,In),
  try_nlp_shared_stream(Out,String),
  try_nlp_shared_stream(Out,'\n'),
  try_nlp_shared_stream(Out,''),!,
  read_nlp_shared_lines(Py,In, Lines).

% Very slow version
nlp_shared_parse4(Py,String, Lines) :- prolog_current_flag(python_local,true),
  lmconfig:space_py_dir(Dir),
  sformat(S,'python3 parser_~w.py -nc ~q ',[Py,String]),
  nop(writeln(S)),
    process_create(path(bash), ['-c', S], [ cwd(Dir), stdout(pipe(In))]),!,
  read_until_nlp_shared_notice(In,"cmdloop_Ready."),!,
  read_nlp_shared_lines(Py,In, Lines).

test_nlp_shared_parse1 :-
  String = "Can the can do the Can Can?",
  nlp_shared_parse3(Py,String, Lines),
  pprint_ecp_cmt(yellow,test_nlp_shared_parse1=Lines).

test_nlp_shared_parse2 :-
  Text = "Can the can do the Can Can?",
  nlp_shared_parse4(Py,Text,Lines),
  pprint_ecp_cmt(yellow,test_nlp_shared_parse2=Lines).

test_nlp_shared_parse3 :-
  Text = "Can the can do the Can Can?",
  nlp_shared_parse2(Py,Text,Lines),
  pprint_ecp_cmt(yellow,test_nlp_shared_parse3=Lines).


nlp_shared_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_nlp_shared_sents(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.

text_to_nlp_shared_pos(Text,PosW2s):- nlp_shared_parse(Text,PosW2s),!.
text_to_nlp_shared_pos(Text,PosW2s):- nlp_shared_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.
  
text_to_nlp_shared_segs(Text,Segs):-
  text_to_nlp_shared_tree(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_nlp_shared_sents(Text,Sent):-
  text_to_nlp_shared_segs(Text,Segs),!,
  nlp_shared_segs_to_sentences(Segs,Sent),!.

nlp_shared_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).



:- if( \+ getenv('keep_going','-k')).
:- use_module(library(editline)).
:- add_history((call(make),call(test_nlp_shared1))).
:- endif.

baseKB:regression_test:- test_nlp_shared(1,X),!,test_nlp_shared(X).
baseKB:sanity_test:- make, forall(test_nlp_shared(1,X),test_nlp_shared(X)).
baseKB:feature_test:- test_nlp_shared.

test_nlp_shared0:- 
  Txt = "PERSON1 asks : Hey , what 's going on XVAR. < p >. PERSON2 said : Not a whole lot . . < p >. PERSON2 said : I 'm looking forward to the weekend , though . . < p >. PERSON1 asks : Do you have any big plans XVAR. < p >. PERSON2 said : Yes . . < p >. PERSON2 said : I 'm going to Wrigley Field on Saturday . . < p >. PERSON1 asks : Aren 't the Cubs out of town XVAR. < p >. PERSON2 said : Yes , but there 's a big concert at Wrigley this weekend . . < p >. PERSON1 said : Oh nice . . < p >. PERSON1 asks : Who 's playing XVAR. < p >. PERSON2 said : Pearl Jam is headlining the Saturday night show . . < p >. PERSON1 said : Wow , Pearl Jam . . < p >. PERSON1 said : I remeber when I got their first CD , Ten , at the record store at Harlem and Irving Plaza . . < p >. PERSON2 said : Oh right . . < p >. PERSON2 said : I remember that record store . . < p >. PERSON1 said : It was called Rolling Stone , and they went out of business many years ago . . < p >. PERSON2 said : Oh that 's too bad . . < p >. PERSON2 said : I really loved taking the bus to Harlem and Irving and visiting that store . . < p >. PERSON1 said : Me too . . < p >. PERSON1 said : We did n't have the internet back then and had to discover new music the hard way . . < p >. PERSON2 said : Haha yes . . < p >. PERSON2 said : I remember discovering ' ' Nirvana before they got famous . . < p >. PERSON1 said : Those were the good old days . . < p >. PERSON2 said : Yes they were . . < p >. PERSON2 said : I need to dig up my old Sony disc player and pop in an old CD . . < p >. PERSON1 asks : Where did the time go XVAR. < p >. PERSON1 said : Pearl Jam is 25 years old already . . < p >. PERSON2 said : It seems like only yesterday that the grunge music movement took over . . < p >. PERSON1 said : Right . . < p >. PERSON1 said : I bet everyone at the concert will be in their forty 's . . < p >. PERSON2 said : No doubt . . < p >. PERSON2 said : Well , I hope you have a great time at the concert . . < p > .",
  test_nlp_shared(Txt),
  ttyflush,writeln(('\n test_nlp_shared0.')),!.

test_nlp_shared1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_nlp_shared(Txt),
  ttyflush,writeln(('\n test_nlp_shared1.')),!.
test_nlp_shared2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_nlp_shared(Txt),
  ttyflush,writeln(('\n test_nlp_shared2.')),!.

test_nlp_shared:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_nlp_shared(Txt),
  ttyflush,writeln(('\n test_nlp_shared.')),
  fail.
test_nlp_shared:- forall(test_nlp_shared(X),test_nlp_shared(X)).

test_1nlp_shared(Text):- 
  format('~N?- ~p.~n',[test_nlp_shared(Text)]),
  text_to_nlp_shared_tree(Text,W),
  print_tree_nl(W),
  !.
test_1nlp_shared(Text):- wdmsg(failed(test_1nlp_shared(Text))).

test_nlp_shared(N):- number(N),!, forall(test_nlp_shared(N,X),test_1nlp_shared(X)). 
test_nlp_shared(X):- test_nlp_shared(_,X),nop(lex_info(X)).

test_nlp_shared(_,X):- nonvar(X), !, once(test_1nlp_shared(X)).

test_nlp_shared(1,".\nThe Norwegian lives in the first house.\n.").
test_nlp_shared(1,"").
test_nlp_shared(1,".").
test_nlp_shared(1,"\n").

test_nlp_shared(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_nlp_shared(2,Each):- test_nlp_shared(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_nlp_shared(3,
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


test_nlp_shared(4,".
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

:- add_history(test_nlp_shared).
:- fixup_exports.

