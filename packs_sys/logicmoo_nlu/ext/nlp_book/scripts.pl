% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% scripts.pl [Chapter 10] Simple script applier
%
?- reconsult('library.pl').
%
% example scripts
%
script(restaurant(Customer,Restaurant,Food),
	[
	 enters(Customer,Restaurant),
	 calls(Customer,Server),
	 brings(Server,Food),
	 eats(Customer,Food),
	 pays(Customer,Teller),
	 leaves(Customer,Restaurant)
	]) :-
	 human(Customer),
	 human(Server),
	 human(Teller),
	 restaurant(Restaurant),
	 food(Food).
%
script(store(Customer,Store,Goods),
	[
	 enters(Customer,Store),
	 calls(Customer,Assistant),
	 brings(Assistant,Goods),
	 pays(Customer,Assistant),
	 picks_up(Customer,Goods),
	 leaves(Customer,Store)
	]) :-
	 goods(Goods),
	 human(Customer),
	 human(Assistant),
	 store(Store).
%
script(supermarket(Customer,Supermarket,Goods),
	[
	 enters(Customer,Supermarket),
	 gets_basket(Customer),
	 fills_basket(Customer,Goods),
	 waits_in_line(Customer),
	 pays(Customer,Teller),
	 leaves(Customer,Supermarket)
	]) :-
	 human(Customer),
	 human(Teller),
	 goods(Goods),
	 supermarket(Supermarket).

script(cat_burglary(Cat,Location,Food),
	[
	 enters(Cat,Location),
	 picks_up(Cat,Food),
	 leaves(Cat,Location),
	 eats(Cat,Food),
	 sleeps(Cat)
	]) :-
	 cat(Cat),
	 food(Food).
%
cat(a_cat).
cat(felix).
cat(garfield).
food(some_food).
food(burger).
food(pizza).
food(lasagne).
food(bigmac).
food(candyfloss).
goods(goods).
goods(bicycle).
goods(jacket).
human(a_person).
human(a_teller).
human(an_assistant).
human(a_customer).
human(a_server).
human(lee).
human(kim).
human(sandy).
restaurant(a_restaurant).
restaurant(pizzahut).
restaurant(mcdonalds).
restaurant(burgerking).
store(a_store).
store(macys).
store(computerland).
supermarket(a_supermarket).
supermarket(safeway).
supermarket(coop).
%
understand(Story,Summary,Events) :-
	script(Summary,Events),
	match(Story,Events).
%
match([],Events).
match([Event1|Events1],[Event1|Events2]) :-
	match(Events1,Events2).
match([Event1|Events1],[Event2|Events2]) :-
	match([Event1|Events1],Events2).
%
test(Story) :- understand(Story,Summary,Events),
	write('The story:  '), nl,
	prpr(Story,3),
	write('Summary:    '),
	write(Summary), nl,
	write('Chronology:'), nl,
	prpr(Events,3).
%
% prpr - pretty-print from C&M 2nd ed. p.97 [name changed to avoid                    
% clash with abbreviation for prepositional phrase]
%
prpr([H|T],I) :-
	!,
	J is I+3,
	prpr(H,J),
	prpx(T,J),
	nl.
prpr(X,I) :-
	tab(I),
	write(X),
	nl.
prpx([],_).
prpx([H|T],I) :-
	prpr(H,I),
	prpx(T,I).
%
test1 :-
	test([enters(lee,mcdonalds),brings(kim,burger), pays(lee,_)]).
test2 :-
	test([enters(kim,macys),brings(sandy,bicycle), pays(kim,sandy)]).
test3 :-
	test([gets_basket(sandy),waits_in_line(_)]).
test4 :-
	test([eats(lee,burger)]).
test5 :-
	test([enters(garfield,pizzahut),eats(He,lasagne)]).
