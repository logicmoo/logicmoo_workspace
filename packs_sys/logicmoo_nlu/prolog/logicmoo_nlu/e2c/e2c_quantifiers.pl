
:- discontiguous(determiner/4).

/*
  no/not more than N
  no/not less than N
  more than N
  less than N
  at least N
  at most N
  N percent of
  N
*/  
comparative_number([more,than],'>').
comparative_number([(not;no),less,than],'>=').
comparative_number([at,least],'>=').
comparative_number([as,many,as], '>=').
comparative_number([wordEndingWithFn("ly")],'=').
comparative_number([as,few,as], '=<').
comparative_number([at,most], '=<').
comparative_number([(not;no),more,than],'=<').
comparative_number([less,than],'<').
comparative_number([almost], '<').


/*
[debug]  ?- noun_phrase("less than two owners").
success( asserted(  
  exists( Owners,
    &(
        quantity(Owners,<,2),
        H4,
        iza(Owners,'Owner'),
        ~(numberOf(Owners,1))))))      <-  we need to tell the NP to not care that we used it plurally
                                           Otherwise:  Count \= 1, Count < 2.
*/
:- add_e2c(noun_phrase("less than 2 owners"), [sanity, bug]).


% NOTE this is not comparative: "at most" means  "maybe with luck, these owners equalling exactly 5, ... will ... "
:- add_e2c(noun_phrase("at most the 5 owners")).

:- add_e2c(noun_phrase("at most 5 owners")).
:- add_e2c(noun_phrase("less than 3 owners"), sanity).
determiner( V, LF) --> theText1(W),{comparative_number([W|Words],Sign)},theText1(Words),!,number_of(N),determiner( V, LF0), 
    add_traits(V, quantity(V, Sign , N),LF0, LF).
:- add_e2c(noun_phrase("at most 50 percent of owners"), sanity).
determiner( V, LF) --> dcg_peek(number_of(N)),theText1(percent), optionalText1([of]), {fail},
   dcg_push([out,of,100,(','),N]),!, determiner( V, LF).

:- add_e2c(noun_phrase("no three owners"), [sanity,no_working]).
determiner( V, LF) --> theText1(no), dcg_peek(number_of(_)), dcg_push([less,than]),!, determiner( V, LF).

determiner( V, LF) --> determiner0(V, LFV), theText1(of), determiner0(G,LFG), add_traits(V, LFV,of(V,G),LF1),add_traits(G,LFG,LF1,LF).
determiner( V, LF) --> determiner0(V, LF).


%opt_each_all--> optionalText1(each).
%opt_each_all--> optionalText1(all).


determiner1( Var, LF) --> determiner1a(Var,LF),optionalText1(of).
determiner1(_Var, true) --> [].

determiner1a(_Var, quant(no)) --> theText1(no).
determiner1a(_Var, quant(exists)) --> (theText1(a);theText1(an)).
determiner1a(_Var, quant(every)) --> theText1(every);theText1(all);theText1(each).
determiner1a(_Var, quant(no)) --> theText1(none);dcg_peek(theText1(zero)).
determiner1a(_Var, quant(exists)) --> theText1(some);theText1(any).
determiner1a(_Var, quant(most)) --> theText1(most).
determiner1a(_Var, quant(few)) --> theText1(few).
determiner1a( Var, LF) --> number_of( Var, LF).

determiner2( Var, Out) --> determiner2a(Var,Out).
determiner2(_Var, true) --> [].

determiner2a( Var, quant(exists) & the(Var)) --> theText1(the).
determiner2a( Var, quant(exists) & the(Var) & pl) --> theText1(these).
determiner2a( Var, quant(exists) & the(Var) & pl) --> theText1(those).

number_of(_Var, numberOf(N))--> number_of(N).
number_of(_Var, true) --> [].

number_of(0) --> theText1(zero).
number_of(1) --> theText1(one).
number_of(2) --> theText1(two).
number_of(3) --> theText1(three).
number_of(N) --> theText1(Five),{tr_number(Five,N)}.
number_of(N) --> numberic_value(N).

determiner0( Var, LF & quant(exists)) --> (theText1([there, exists]);theText1(exists)),!,determiner( Var, LF).
determiner0( V, LF) --> determiner1(V,LF0), determiner2(V,LF1), number_of(V,LF2),add_traits(V,LF0 & LF1 & LF2,true,LF),!.


