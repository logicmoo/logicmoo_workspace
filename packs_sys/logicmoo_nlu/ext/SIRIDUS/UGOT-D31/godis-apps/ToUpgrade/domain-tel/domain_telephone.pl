/*************************************************************************



         name: domain_telephone.pl 

  description: telephone domain file

 

*************************************************************************/

:- module( domain_telephone, [ plan/2,

			       issue/2,

			       sort_restr/1,

			       isa/2,

			       postcond/2,

			       depends/2

			     ] ).



:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).



:- ensure_loaded( library( semsort_telephone ) ).



default_question(dummy).

depends(dummy,dummy).

%postcond(dummy,dummy).





%actions

plan( top,

      [ forget_all,

	raise( X^action(X) ),

	findout( set( [ action( tp_phoneCall ),

			action( tp_divertCall ),

			action( tp_cancelDivert ),

			action( tp_conferenceCall ) ] ) ) ] ).



postcond( top, none ).



plan( tp_phoneCall, [ findout( X^destination(X) ), % askParameter

		      dev_do(telephone, 'MakePhoneCall') ] ).



postcond( tp_phoneCall, done('MakePhoneCall')).



plan( tp_divertCall,[ findout( X^divert_to(X)),

		      dev_do(telephone, 'DivertCall') ] ).



postcond( tp_divertCall, done('DivertCall')).



plan( tp_cancelDivert,

      [ dev_do(telephone, 'CancelDivert')]).



postcond( tp_cancelDivert, done('CancelDivert')).





plan( tp_conferenceCall,[ findout( X^first_person(X)),

			  findout( Y^second_person(Y)),

			  dev_do(telephone, 'ConferenceCall') ]).



postcond( tp_conferenceCall, done('ConferenceCall') ).





% SL021125

plan( change_domain,

      [ raise(X^domain(X)),

	findout(set([domain(vcr), domain(telephone)])),

	change_domain ] ).

postcond( change_domain, done(change_domain) ).



sort_restr( domain( X ) ) :- sem_sort( X, domain ).% SL021125



%issues not implemented

%plan(X^officeNumber(X),

%     [ findout( X^person_in_office(X) ),

%	if_then( person_in_office(Name),

%		 [ look_up_office_number(Name, OfficeNumber),

%		   informExecution( office_number(Name, OfficeNumber)) ] ),

%	forget,

%	exec(loop)

%      ] ).)







% sortal restrictions; determine relevance and resolvement

sort_restr( destination(X) ) :-sem_sort(X, name ).

sort_restr( divert_to(X) ) :- sem_sort(X, name ).

sort_restr( first_person(X) ):- sem_sort(X, name ).

sort_restr( second_person(X) ):- sem_sort(X, name ).

sort_restr( name(X)) :- sem_sort( X, name ).



%general IBiS sort_restr

% negation



sort_restr( not P ) :- sort_restr( P ).



% action



sort_restr( action( X ) ) :- sem_sort( X, action ).

sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).





% issue



sort_restr( issue(Q) ):- plan( Q, _ ),

	\+ sort_restr( action( Q ) ).



sort_restr( issue(Q) ):-

	plan( _, Plan ),

	member( findout(Q) , Plan ),

	\+ sort_restr( action( Q ) ).



% metaissue



sort_restr( und(_DP*P) ):- sort_restr(P).



% sloppy, but allows "no" as answer to clarification alt-q

% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,

% menaing the same as (not P1 and not p2 and...)



sort_restr( not und(_DP*set(_))).



% parameter validity; determines acceptance

valid_parameter( domain( X ) ) :- sem_sort( X, domain ). %SL021125

valid_parameter( destination(X) ):- sem_sort(X, name).

valid_parameter( divert_to(X) ):- sem_sort(X, name).

valid_parameter( first_person(X) ) :- sem_sort(X, name).

valid_parameter( second_person(X) ) :- sem_sort(X, name).











