/* @(#)world0.pl	24.1 2/23/88 */

/*
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Data for the World Database.
% ---------------------------


% Interface.
% ---------

% Interface.
% ---------

database(aggregate(X,Y,Z)) :- aggregate(X,Y,Z).
database(one_of(X,Y)) :- one_of(X,Y).
database(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database(card(X,Y)) :- card(X,Y).
database(african(X)) :- african(X).
database(american(X)) :- american(X).
database(area(X)) :- area(X).
database(area(X,Y)) :- area(X,Y).
database(asian(X)) :- asian(X).
database(borders(X,Y)) :- borders(X,Y).
database(capital(X)) :- capital(X).
database(capital(X,Y)) :- capital(X,Y).
database(circle_of_latitude(X)) :- circle_of_latitude(X).
database(city(X)) :- city(X).
database(continent(X)) :- continent(X).
database(country(X)) :- country(X).
database(drains(X,Y)) :- drains(X,Y).
database(eastof(X,Y)) :- eastof(X,Y).
database(european(X)) :- european(X).
database(exceeds(X,Y)) :- exceeds(X,Y).
database(flows(X,Y)) :- flows(X,Y).
database(flows(X,Y,Z)) :- flows(X,Y,Z).
database(in(X,Y)) :- in(X,Y).
database(latitude(X)) :- latitude(X).
database(latitude(X,Y)) :- latitude(X,Y).
database(longitude(X)) :- longitude(X).
database(longitude(X,Y)) :- longitude(X,Y).
database(northof(X,Y)) :- northof(X,Y).
database(ocean(X)) :- ocean(X).
database(place(X)) :- place(X).
%database(person(X)) :- person(X).
database(population(X)) :- population(X).
database(population(X,Y)) :- population(X,Y).
database(region(X)) :- region(X).
database(rises(X,Y)) :- rises(X,Y).
database(river(X)) :- river(X).
database(sea(X)) :- sea(X).
database(seamass(X)) :- seamass(X).
database(southof(X,Y)) :- southof(X,Y).
database(westof(X,Y)) :- westof(X,Y).

:-op(500,xfy,--).

exceeds(X--U,Y--U) :- !, X > Y.
exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

area(X--ksqmiles).
capital(C) :- capital(X,C).
city(C) :- city(C,_,_).
country(C) :- country(C,_,_,_,_,_,_,_,_,_).
latitude(X--degrees).
longitude(X--degrees).
place(X) :- continent(X); region(X); seamass(X); country(X).
population(X--million).
population(X--thousand).
region(R) :- in_continent(R,_).

african(X) :- in(X,africa).
american(X) :- in(X,america).
asian(X) :- in(X,asia).
european(X) :- in(X,europe).

in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
in(X,Y) :- in0(X,W), ( W=Y ; in(W,Y) ).

in0(X,Y) :- in_continent(X,Y).
in0(X,Y) :- city(X,Y,_).
in0(X,Y) :- country(X,Y,_,_,_,_,_,_,_,_).
in0(X,Y) :- flows(X,Y).

eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

latitude(equator,0--degrees).
latitude(tropic_of_cancer,23--degrees).
latitude(tropic_of_capricorn,-23--degrees).
latitude(arctic_circle,67--degrees).
latitude(antarctic_circle,-67--degrees).

latitude(C,L--degrees) :- country(C,_,L,_,_,_,_,_,_,_).
longitude(C,L--degrees) :- country(C,_,_,L,_,_,_,_,_,_).
area(C,A--ksqmiles) :- country(C,_,_,_,A,_,_,_,_,_).
population(C,P--thousand) :- city(C,_,P).
population(C,P--million) :- country(C,_,_,_,_,_,P,_,_,_).
capital(C,Cap) :- country(C,_,_,_,_,_,_,_,Cap,_).

continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

in_continent(scandinavia, europe).
in_continent(western_europe, europe).
in_continent(eastern_europe, europe).
in_continent(southern_europe, europe).
in_continent(north_america, america).
in_continent(central_america, america).
in_continent(caribbean, america).
in_continent(south_america, america).
in_continent(north_africa, africa).
in_continent(west_africa, africa).
in_continent(central_africa, africa).
in_continent(east_africa, africa).
in_continent(southern_africa, africa).
in_continent(middle_east,  asia).
in_continent(indian_subcontinent, asia).
in_continent(southeast_east, asia).
in_continent(far_east, asia).
in_continent(northern_asia, asia).

seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ocean(arctic_ocean).
ocean(atlantic).
ocean(indian_ocean).
ocean(pacific).
ocean(southern_ocean).

sea(baltic).
sea(black_sea).
sea(caspian).
sea(mediterranean).
sea(persian_gulf).
sea(red_sea).

river(R) :- river(R,L).

rises(R,C) :- river(R,L), last(L,C).

drains(R,S) :- river(R,L), first(L,S).

flows(R,C) :- flows(R,C,_).

flows(R,C1,C2) :- river(R,L), links(L,C2,C1).

first([X|_],X).

% last([X],X).					% (SWI-system predicate)
% last([_|L],X) :- last(L,X).

links([X1,X2|_],X1,X2).
links([_|L],X1,X2) :- links(L,X1,X2).

