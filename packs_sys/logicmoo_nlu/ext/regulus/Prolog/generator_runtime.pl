
:- module(generator_runtime,
	  [consume_item/3
	   ]
      ).

%---------------------------------------------------------------

% This is probably only relevant when we are running an interlingua
% checking in debug mode. We want to avoid trying to generate from
% completely unconstrained [clause ...] items, since this creates
% an explosion.
consume_item(InList, _FeatValItem, _OutList) :-
	var(InList),
	!,
	fail.
consume_item([FeatValItem | R], FeatValItem, R).
consume_item([F | R], FeatValItem, [F | R1]) :-
	consume_item(R, FeatValItem, R1).

%consume_item([FeatValItem | R], FeatValItem, R):-
%	nonvar(R),
%	constrain_uninstantiated_feat_val_item(FeatValItem).
%consume_item([F | R], FeatValItem, [F | R1]) :-
%	nonvar(F),
%	consume_item(R, FeatValItem, R1),
%	constrain_uninstantiated_feat_val_item(FeatValItem).
%
%constrain_uninstantiated_feat_val_item(FeatValItem) :-
%	var(FeatValItem),
%	!,
%	FeatValItem = [Key | _Rest],
%	dif(Key, clause).
%constrain_uninstantiated_feat_val_item(_FeatValItem).
	
