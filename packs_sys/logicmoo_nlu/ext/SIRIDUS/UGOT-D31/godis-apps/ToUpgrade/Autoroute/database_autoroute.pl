
 
/*************************************************************************

         name: database_autoroute.pl 
      version: 
  description: An example autoroute domain database
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module( database_autoroute, [consultDB/4] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).


/*----------------------------------------------------------------------
     consultDB( +Domain, +Beliefs, +Query, -Answer )
     -- Returns (if it succeeds) an Answer to a Query given
        background Beliefs and a Domain
----------------------------------------------------------------------*/

consultDB( _^Query, Known, Query ):-
        db( Facts ), 
        select( Query, Facts, KnownFacts ), 
        % check that known facts are consistent with db post
        \+ (
             member( Fact, KnownFacts ),
             \+ implies( Known, Fact )
           ).

implies( Bel, Fact ) :-
        member( Fact, Bel ).





%%% db( -Facts )

db( [from(From),to(To),time(Time), quickshort(QuickShort),dist(Dist)] ) :-
	route( QuickShort, From, To, Time, Dist, _ ).

db( [from(From),to(To),time(Time), quickshort(QuickShort),dur(Dur)] ) :-
	route( QuickShort, From, To, Time, _, Dur ).

%%% db facts for the travel dialogue

route( quickest, malvern, edwinstowe, 6, 113, 2:08).
route( quickest, edwinstowe, malvern, 6, 113, 2:08).
route( shortest, malvern, edwinstowe, 6, 101, 3:08).
route( shortest, edwinstowe, malvern, 6, 101, 3:08).