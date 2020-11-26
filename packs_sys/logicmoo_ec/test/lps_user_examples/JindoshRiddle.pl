:- expects_dialect(lps).

%%  JindoshRiddle (-Solution)
%   @param  Solution is a list of houses that satisfy all constraints.
%   @author Generated puzzle from video game
%   @see https://dishonored.gamepedia.com/The_Jindosh_Riddle


/* At the dinner party were Lady Winslow, Doctor Marcolla, Countess Contee, Madam Natsiou, and Baroness Finch.

The women sat in a row. 
They all wore different colors and <woman 1> wore a jaunty <color 1> hat. 
<woman 2> was at the far left, next to the guest wearing a <color 2> jacket. 
The lady in <color 3> sat left of someone in <color 4>. 
I remember that <color 3> outfit because the woman spilled her <alcohol 1> all over it. 
The traveler from <location 1> was dressed entirely in <color 5>. 
When one of the dinner guests bragged about her <heirloom 1>, 
the woman next to her said they were finer in <location 1>, where she lived.

So <woman 3> showed off a prized <heirloom 5>, 
at which the lady from <location 2> scoffed, saying it was no match for her <heirloom 3>. 
Someone else carried a valuable <heirloom 4> and when she saw it, 
the visitor from <location 3> next to her almost spilled her neighbor’s <alcohol 2>. 
<woman 4> raised her <alcohol 3> in toast. 
The lady from <location 4>, full of <alcohol 4>, jumped up onto the table, 
falling onto the guest in the center seat, spilling the poor woman’s <alcohol 5>. 
Then <woman 5> captivated them all with a story about her wild youth in <location 5>.

In the morning, there were four heirlooms under the table: 
the <heirloom 1>, <heirloom 2>, the <heirloom 3>, and the <heirloom 4>.

But who owned each?"
*/

% Render the seats term as a nice table.
:- use_rendering(table,
		 [header(s('Owner', 'Heirloom','Town', 'Drink', 'Color'))]).



seating(Seats) :-
	% each Owner in the list Seats is represented as:
	%      s(Name, Heirloom, Town, Drink, Color)
	length(Seats, 5),
	member(s(winslow,_,_,_,purple), Seats),
    left_of(s(_,_,_,whiskey,red),s(_,_,_,_,green),Seats),
    member(s(_,_,dabokva,_,white),Seats),
    next(s(_,snuff,_,_,_),s(_,_,dabokva,_,_),Seats),
	Seats = [s(marcola,_,_,_,_),s(_,_,_,_,blue),s(_,_,_,beer,_),_,_],
    member(s(contee,medal,_,_,_),Seats),
    member(s(_,ring,fraeport,_,_),Seats),
    next(s(_,_,    dunwall,_,_),s(_,_,_,      absinthe,_),Seats),
    next(s(_,snuff,_,      _,_),s(_,_,dabokva,_,       _),Seats),
    member(s(finch,_,_,rum,_),Seats),
    member(s(_,_,karnaca,wine,_),Seats),
    next(s(_,pendant,_,_,_),s(_,_,dunwall,_,_),Seats),
    member(s(natsiou,_,baleton,_,_),Seats),
    member(s(_,diamond,_,_,_),Seats).		

left_of(A, B, Ls) :- append(_, [A,B|_], Ls).
right_of(A, B, Ls) :- append(_, [B,A|_], Ls).
next(A, B, Ls) :- left_of(A, B, Ls).
next(A, B, Ls) :- right_of(A, B, Ls).
    

/*

?- seating(Seats).

*/
