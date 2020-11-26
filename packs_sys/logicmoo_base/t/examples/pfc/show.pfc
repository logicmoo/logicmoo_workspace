% -*-Prolog-*-

:- ensure_loaded('/sun/nlp/nlp/pde/experimental/send.pl').

:- load_gnu_file(show,'/sun/mn2/finin/pfc/show.el').

show_mpred_fact(P) :- send_editor(['(show-assertion "',P,'")']).

hide_mpred_fact(P) :- send_editor(['(hide-assertion "',P,'")']).

demons(P, WhenAdded, WhenRemoved) ==>
  (P ==> {WhenAdded}),
  fcUndoMethod(WhenAdded,WhenRemoved).

show(P) ==> demons(P,show_mpred_fact(P),hide_mpred_fact(P)).

%% try something like:
%%
%%     show(_)            % to show all facts
%%     show(faulty(_))    % show just assertions mathing this.
%%




















  

