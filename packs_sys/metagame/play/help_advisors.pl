%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

help_advisors :- print_advisors.


print_advisors :- 
	advisor_herald(H),
	format("~s~n",[H]),
	whenever(print_advisor(_A),
	  format("~n~n",[])).
	

advisor_herald("Explanation of ADVISORS
-----------------------
Following is the list of all advisors with a brief explanation.
Also shown (in parens) is the current value for each advisor.
For more information, consult the papers or source-code.
").

print_advisor(A) :- 
	advisor_weight(A,W),
	advhelp(A,Help), 
	format("<~p>  (~p) ~n~s",[A,W,Help]).


advhelp(gen_material,"Gives 1 point for each white piece, -1 for black.").

advhelp(material,"Uses user-defined material function if available (help evalfile).").

advhelp(square,"Uses user-defined piece-square table if available (help evalfile).").

advhelp(lthreat,"Gives points for each enemy piece a piece can capture.").

advhelp(potent,"Like lthreat, but reduces each threat value if defended.").

advhelp(gthreat,"Value only for best of all lthreats.").

advhelp(pthreat,"Value only for best of all potent threats.").

advhelp(vital,"Doesn't like leaving pieces attacked when enemy achieves goal 
by removing them.  Only sensitive when there are VITAL_NUMBER pieces left.").

advhelp(dynamic_mobility,"1 point for each move piece makes in current position.").

advhelp(static_mobility,"1 point each move piece makes from square on empty board.").

advhelp(eventual_mobility,"Points for each square piece can ever reach from square on empty board,
discounted by how many moves it takes piece to get there from square.  The discount
function is controlled by DISCOUNT parameter, either INVERSE or EXPONENT.").

advhelp(gmovmob,"Sum of dynamic_mobility for all player's pieces.").

advhelp(gcapmob,"1 point for each capturing-move in current position.").

advhelp(arrive_distance,"Favors pieces on squares close to achieving arrival goals.").

advhelp(promote_distance,"Favors getting pieces close to promotion. 
Value based on cost to get to promotion square, and value of best piece the piece
can eventually promote into.").

advhelp(possess,"Points for each piece player has in hand.  Generally the value
is the average or max of the values it would have when placed on the board.").
 
advhelp(initprom,"Anticipates points which will follow when player init-promotes a piece
next turn.  Value will be value of best choice that player can make.").

advhelp(random,"Adds a random noise to position value, in range [RANDOM_MIN,RANDOM_MAX].
Note that this value will be multiplied by the weight 
attached to this advisor [as is the case for all advisors.]").


advhelp(static,"The following advisors are used to build the static tables for each piece. 
This advisor determines how much to weigh the static values vs. other advisors.").

advhelp(victims,"Point for each piece this piece could someday capture.").

advhelp(immunity,"Points for each enemy piece which cannot someday capture this piece.").

advhelp(giveaway,"Points for each piece we own which can someday capture this piece.").

advhelp(eradicate,"Points for pieces which enemy would like to eradicate.").

advhelp(stalemate,"Degree to which piece might contribute to stalemate goals.").

advhelp(arrive,"Degree to which piece might contribute to arrive goals.").

advhelp(max_static_mob,"Maximum static mobility piece has.").

advhelp(max_eventual_mob,"Maximum eventual mobility piece has.").

advhelp(avg_static_mob,"Average static mobility piece has.").

advhelp(avg_eventual_mob,"Average eventual mobility piece has.").

advhelp(dominate,"Not used.").

