:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

% update name match list produced by orthomatcher

% receives a list like
% [match(e1,[123,23,45]),....]
% groups the entities that share the same match list
% and produces terms like name_match(e1,[e4,e7]), name_match(e4,[e1,e7]), name_% match(e7,[e1,e4])...

update_match(Input,Output):-
  extract_match(Input,Match,Name_match),
  update_name_match(Match,Names),
  instantiate(Name_match,Names),
  update_match_bis(Input,Names,Output).

update_match_bis(Input,[],Input).

update_match_bis(Input,[[BEG,END,name_match(E,L)]|R],Output):-
  update_match_element([BEG,END,name_match(E,L)],Input,Input1),
  update_match_bis(Input1,R,Output).

update_match_element([BEG,END,name_match(E,L)],[BEG,END,QLF|QLFS],
[BEG,END,[name_match(E,L)|QLF]|QLFS]):-!.

update_match_element([BEG,END,name_match(E,L)],[BEG1,END1,QLF|QLFS],[BEG1,END1,QLF|QLFS1]):-
  update_match_element([BEG,END,name_match(E,L)],QLFS,QLFS1).

update_match_element(_,[],[]).



extract_match([BEG,END,QLF|QLFS],O1,O2):-
  findall([BEG,END,match(E,list([H|R]))],member(match(E,list([H|R])),QLF),L1),
  generate_uninstantiated(L1,L2),
  extract_match(QLFS,X1,X2),
  append(L1,X1,O1),
  append(L2,X2,O2).

extract_match([],[],[]).

generate_uninstantiated([],[]).
generate_uninstantiated([[BEG,END,match(E,_)]|R],[[BEG,END,name_match(E,L)]|R1]):-
  generate_uninstantiated(R,R1).

instantiate([],_).
instantiate([E|L],R):-
  member(E,R),
  instantiate(L,R).

update_name_match([],[]).

update_name_match([[BEG,END,match(ENTITY,list(LIST))]|Input],Output):-
  findall([BEG1,END1,match(ENTITY1,list(LIST))],member([BEG1,END1,match(ENTITY1,list(LIST))],Input),NEW),
  delete(NEW,Input,Input1),
  generate_name_match([[BEG,END,match(ENTITY,list(LIST))]|NEW],NAME_MATCH_LIST),
  update_name_match(Input1,Output1),
  append(NAME_MATCH_LIST,Output1,Output).

delete([],L,L):-!.
delete([E|R],[E|L],L1):-!,
  delete(R,L,L1).
  
delete(R,[E1|L],[E1|L1]):-!,
  delete(R,L,L1).

generate_name_match(I,O):-
  findall([BEG,END,E],L^member([BEG,END,match(E,list(L))],I),ENTITIES),
  findall(E,BEG^END^L^member([BEG,END,match(E,list(L))],I),LIST),
  findall([BEG,END,name_match(E,ENTITIES1)],(member([BEG,END,E],ENTITIES),
delete([E],LIST,ENTITIES1)),O).
