:- module( database_home, [consultDB/3] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- use_module( library(telia_house) ).

/*----------------------------------------------------------------------
     consultDB( +Beliefs, +Query, -Answer )
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

db( [ device_type(DeviceType), location(Location), device(Location/DeviceID) ] ) :-
	house( House ),
	member( Location=Devices, House ),
	member( DeviceID=(DeviceType,_,_), Devices ).

db( [ task(leaving), location(Location), device(Location/DeviceID) ] ) :-
	house( House ),
	member( Location=Devices, House ),
	member( DeviceID=(DeviceType,_,_), Devices ),
	( DeviceType == lamp ; DeviceType == dimmer ).
