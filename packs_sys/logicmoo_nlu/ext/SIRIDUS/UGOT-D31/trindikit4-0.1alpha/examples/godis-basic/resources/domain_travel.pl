 
/*************************************************************************

         name: domain_travel.pl 
  description: An example domain file
 
*************************************************************************/

:- module( domain_travel, [ plan/2,
			    issue/2,
			    sysaction/1,
			    sort_restr/1,
			    default_question/1,
			    resource_of_type/1
			  ] ).

resource_of_type(domain).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_travel ) ).

/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?Name, ?Plan )
----------------------------------------------------------------------*/

postcond(hej,hej).
depends(jo,jo).
isa(du,du).

plan(top,[findout(X^issue(X))]).

% plan( ?Question, ?Plan )

plan( X^price(X), 
      [ %raise(X1^(how(X1))),
	findout(X1^(how(X1))),
	findout(X2^(dest_city(X2))),
	findout(X3^(dept_city(X3))),
	findout(X4^(month(X4))),
	findout(X9^(dept_day(X9))),
	findout(X5^(class(X5))),
	consultDB(X7^(price(X7)))
      ] ).


plan( need_visa,
      [ findout(X^dest_city(X)),
	findout(X1^citizenship(X1)),
	consultDB( need_visa )
      ] ).

%default_question( X^dest_city(X) ).
default_question( dummy ).

% dummy sysaction
sysaction( dummy ).


/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sort_restr( dest_city(X) ):- sem_sort( X, city ).
sort_restr( dept_city(X) ):- sem_sort( X, city ).
sort_restr( how(X) ):- sem_sort( X, means_of_transport ).
sort_restr( month(X) ):- sem_sort( X, month ).
sort_restr( price(X) ):- sem_sort( X, price ).
sort_restr( class(X) ):- sem_sort( X, class ).
sort_restr( dept_day(X) ):- sem_sort( X, day ).
sort_restr( citizenship(X) ):- sem_sort( X, country ).

% no arguments -> no sortal restrictions (generalise!)
sort_restr( need_visa ).

% negation

sort_restr( not P ) :- sort_restr( P ).
%sort_restr( notexist( X, PX ) ).
%sort_restr( unknown( P ) ).

% issue

sort_restr( issue(Q) ):- plan( Q, _ ).
sort_restr( issue(Q) ):- plan( _, Plan ),
	member( findout(Q), Plan ).

% metaissue

sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)
sort_restr( not und(_DP*set(_))).

sort_restr( db_entry(_,_) ).
sort_restr( db_entry(_,_,_) ).

