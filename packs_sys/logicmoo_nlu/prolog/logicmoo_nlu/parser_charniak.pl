:-module(parser_charniak, 
 [
  test_charniak/0,
  test_charniak/1,
  test_charniak/2,
  test_charniak/1,
  test_charniak_parse1/0,
  test_charniak_parse2/0,  
  charniak_stream/2,
  charniak_pos/2,
  text_to_charniak/2,
  text_to_charniak_segs/2,
  charniak_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- reexport(library(logicmoo_nlu/parser_penn_trees)).

charniak_stream(Text,Out):-
  process_create(path(bash), [('/opt/logicmoo_workspace/packs_xtra/logicmoo_pldata/bllip-parser/CharniakParse.sh'), Text ],
    [ stdout(pipe(Out))]).

charniak_parse(Text, Lines) :-
  into_acetext(Text,String),
  setup_call_cleanup(
  charniak_stream(String,Out),
  read_lines(Out, Lines),
  close(Out)).

test_charniak_parse1 :-
 Text = "Can the can do the Can Can?",
 setup_call_cleanup(
  process_create(path(bash), [('/opt/logicmoo_workspace/packs_xtra/logicmoo_pldata/bllip-parser/CharniakParse.sh'), Text ],
    [ stdout(pipe(Out)) ]),
  read_line_to_string(Out, Lines),
  close(Out)),
  pprint_ecp_cmt(yellow,test_charniak_parse1=Lines).

test_charniak_parse2 :-
  Text = "Can the can do the Can Can?",
  charniak_parse(Text,Lines),
  pprint_ecp_cmt(yellow,test_charniak_parse2=Lines).

test_charniak_parse3 :-
  Text = "Can the can do the Can Can?",
  charniak_pos(Text,Lines),
  pprint_ecp_cmt(yellow,test_charniak_parse2=Lines).

read_lines(Out, Result) :-
  read_line_to_string(Out, StringIn),
  read_lines(StringIn, Out, Lines),
  into_result(Lines,Result).

into_result(Lines,Result):- string(Lines)=Result,!.

read_lines(end_of_file, _, "") :- !.
read_lines(StringIn, Out, AllCodes) :-  
  read_line_to_string(Out, Line2),
  read_lines(Line2, Out, Lines),
  atomics_to_string([StringIn,'\n',Lines],AllCodes).

   
charniak_pos_info(Text,PosW2s,Info,LExpr):-
  text_to_charniak(Text,LExpr),
  tree_to_lexical_segs(LExpr,SegsF),
  segs_retain_w2(SegsF,Info,PosW2s),!.
  
charniak_pos(Text,PosW2s):- charniak_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.

text_to_charniak_segs(Text,Segs):-
  text_to_charniak_tree(Text,LExpr),
  tree_to_lexical_segs(LExpr,Segs).

text_to_charniak(Text,Sent):-
  text_to_charniak_segs(Text,Segs),!,
  charniak_segs_to_sentences(Segs,Sent),!.

charniak_segs_to_sentences(Segs,sentence(0,W2,Info)):-
  segs_retain_w2(Segs,Info,W2).

text_to_charniak_tree(Text,LExpr):-
  charniak_parse(Text, String),
  lxpr_to_list(String, LExpr),
  nop(print_tree(charniak=LExpr)).


:- use_module(library(editline)).
:- add_history((call(make),call(test_corenlp1))).

baseKB:regression_test:- test_charniak(1,X),!,test_charniak(X).
baseKB:sanity_test:- make, forall(test_charniak(1,X),test_charniak(X)).
baseKB:feature_test:- test_charniak.

test_charniak1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak1.')),!.
test_charniak2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak2.')),!.

 test_charniak:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak.')),!.
test_charniak:- forall(test_charniak(X),test_charniak(X)).

test_1charniak(Text):- 
  format('~N?- ~p.~n',[test_charniak(Text)]),
  charniak_pos(Text,W),
  print_tree(W),
  !.

test_charniak(N):- number(N),!, forall(test_charniak(N,X),test_1charniak(X)). 
test_charniak(X):- test_charniak(_,X),nop(lex_info(X)).

test_charniak(_,X):- nonvar(X), !, once(test_1charniak(X)).

test_charniak(1,".\nThe Norwegian lives in the first house.\n.").

test_charniak(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_charniak(2,Each):- test_charniak(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_charniak(3,
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


test_charniak(3,".
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

:- fixup_exports.

