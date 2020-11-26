%
% @inproceedings{Mueller:2004b,
%   author = "Erik T. Mueller",
%   year = "2004",
%   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
%   editor = "Valerie Barr and Zdravko Markov",
%   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
%   pages = "147--152",
%   address = "Menlo Park, CA",
%   publisher = "AAAI Press",
% }
%

object(A) :- agent(A).
object(P) :- physobj(P).
fluent(at(O,L)) :- object(O),location(L).
fluent(holding(A,P)) :- agent(A),physobj(P).
event(pick_up(A,P)) :- agent(A),physobj(P).
event(set_down(A,P)) :- agent(A),physobj(P).
event(move(A,L1,L2)) :- agent(A),location(L1),location(L2).

holds_at(at(P,L),T) :- timepoint(T),
                       physobj(P),
                       location(L),
                       agent(A),
                       holds_at(at(A,L),T),
                       holds_at(holding(A,P),T).

:- object(O),
   location(L1),
   location(L2),
   timepoint(T),
   holds_at(at(O,L1),T),
   holds_at(at(O,L2),T),
   L1 != L2.

initiates(move(A,L1,L2),at(A,L2),T) :- timepoint(T),
                                       agent(A),
                                       location(L1),
                                       location(L2).

terminates(move(A,L1,L2),at(A,L1),T) :- timepoint(T),
                                        agent(A),
                                        location(L1),
                                        location(L2).

initiates(pick_up(A,P),holding(A,P),T) :- timepoint(T),
                                          agent(A),
                                          physobj(P).

terminates(set_down(A,P),holding(A,P),T) :- timepoint(T),
                                            agent(A),
                                            physobj(P).

:- timepoint(T),
   agent(A),
   location(L1),
   location(L2),
   happens(move(A,L1,L2),T),
   -holds_at(at(A,L1),T).

at_same_location(A,P,T) :- timepoint(T),
                           agent(A),
                           physobj(P),
                           location(L),
                           holds_at(at(A,L),T),
                           holds_at(at(P,L),T).

:- timepoint(T),
   agent(A),
   physobj(P),
   happens(pick_up(A,P),T),
   not at_same_location(A,P,T).

releases(pick_up(A,P),at(P,L),T) :- timepoint(T),
                                    agent(A),
                                    physobj(P),
                                    location(L).

initiates(set_down(A,P),at(P,L),T) :- timepoint(T),
                                      agent(A),
                                      physobj(P),
                                      location(L),
                                      holds_at(at(A,L),T).

timepoint(0..9).
agent(james).
physobj(coin).
location(l1).
location(l2).
location(l3).
location(l4).

-holds_at(holding(james,coin),0).
holds_at(at(coin,l4),0).
holds_at(at(james,l1),0).

-released_at(F,0) :- fluent(F).

happens(move(james,l1,l2),0).
happens(move(james,l2,l3),1).
happens(move(james,l3,l4),2).
happens(pick_up(james,coin),3).
happens(move(james,l4,l3),4).
happens(move(james,l3,l2),5).
happens(set_down(james,coin),6).
happens(move(james,l2,l3),7).
happens(move(james,l3,l4),8).

% End of file.
