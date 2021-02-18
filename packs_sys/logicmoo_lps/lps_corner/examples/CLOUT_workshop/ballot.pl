
:- expects_dialect(lps).

% single ballot per program instance, added method names as in Solidity original
% http://solidity.readthedocs.io/en/v0.4.24/solidity-by-example.html
% The representation could be extended to allow multiple ballots at the same time
% by adding a ballot identifier as an extra parameter of all the events, fluents and actions.

maxTime(15).

events 	ballot(_Chairman, _Proposals), giveRightToVote(_Chairman, _Voter), 
	delegate(_FromVoter, _ToVoter), vote(_Voter, _Candidate).

fluents	chairman(_Chairman), voter(_Voter, _Weight), voted(_Voter, _Candidate), 
	voteCount(_Candidate, _Votes), representedBy(_Voter, _Delegate).

actions announceWinner(_Candidate, _Votes).

observe ballot(chair, [trump, clinton]) from 1 to 2.
observe giveRightToVote(chair, miguel), 
	giveRightToVote(chair, fariba),
	giveRightToVote(chair, bob), 
	giveRightToVote(chair, jacinto)  from 3 to 4.
observe delegate(bob, miguel)  from 4 to 5.
observe vote(miguel, clinton)  from 5 to 6.
observe delegate(jacinto,bob) from 6 to 7.
observe delegate(fariba, miguel)  from 6 to 7.
% observe vote(bob, clinton)  from 5 to 6.

endOfBallot(9).

if endOfBallot(T), winningProposal(Candidate,Votes) at T 
then announceWinner(Candidate, Votes) from T to Tnext, 
	lps_terminate from Tnext.

ballot(_Chairman, Proposals) initiates voteCount(Candidate, 0) if 
	member(Candidate, Proposals).
ballot(Chairman, _Proposals) initiates voter(Chairman,1).
ballot(Chairman, _Proposals) initiates chairman(Chairman).

% A new ballot may not happen if there is already an ongoing ballot 
% (as witnessed by the existence of a candidate on the ongoing ballot).
% 
false ballot(_,_), voteCount(_,_).

false giveRightToVote(Chairman, Voter), not chairman(Chairman).
false giveRightToVote(Chairman, Voter), voter(Voter,_).

giveRightToVote(Chairman, Voter) initiates voter(Voter, 1).
giveRightToVote(Chairman, Voter) initiates representedBy(Voter, Voter).

% I don't think these are necessary. 
% There are other constraints 
% to prevent a person from voting or delegating more than once.
% 
% vote(Voter, _Candidate) updates Old to 0 in voter(Voter, Old).
% delegate(Voter1, _Voter2) updates Old to 0 in voter(Voter1, Old).

vote(Voter, Candidate) initiates voted(Voter, Candidate).

false vote(Voter,_), voted(Voter,_).
false vote(Voter,_), representedBy(Voter, Delegate), Voter \= Delegate.
false vote(Voter, Candidate1), vote(Voter, Candidate2), Candidate1 \= Candidate2.

% A Voter may not delegate its votes
% if it has already voted or if it is already represented by another Voter.
%
false delegate(Voter,_), voted(Voter,_).
false delegate(Voter1,Voter2), representedBy(Voter1,Voter3), Voter1 \= Voter3.

false delegate(Voter,Voter). % Probably harmless, but useless.
false delegate(Voter, Voter1), delegate(Voter, Voter2), Voter1 \= Voter2.

% If a Voter votes for a Candidate, 
% then the vote count for the candidate 
% is increased by the number of votes held by Voter.
% Note that several Voters can vote for the same candidate simultaneously.
% The votes then accumulate.
% 
vote(Voter, Candidate) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes)	if
		voter(Voter, N), 
		NewVotes is OldVotes + N.

% If Voter1 delegates its votes to Voter2, 
% and  the representative of Voter2 has not yet voted,
% then the number of votes held by the representative of Voter2
% is increased by the number of votes held by Voter1.
% Note that several Voters can delegate their votes to the same candidate simultaneously.
% The votes then accumulate.
% 
delegate(Voter1, Voter2) updates Old to New in voter(Voter3, Old) if 
		representedBy(Voter2,Voter3), not voted(Voter3, _),
		voter(Voter1, N1), New is N1 + Old. 

% If Voter1 delegates its votes to Voter2, 
% then the vote count for any candidate that has already been voted for by the representative of Voter2
% is increased by the number of votes held by Voter1.
%
delegate(Voter1, Voter2) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes) if
		representedBy(Voter2,Voter3), voted(Voter3, Candidate),  
		voter(Voter1, N), NewVotes is OldVotes + N.

% If Voter1 delegates its votes to Voter2, 
% then any Voter that is represented by Voter1 
% is now represented instead by the Voter that represents Voter2.
%
delegate(Voter1, Voter2) updates Voter1 to Voter3 in representedBy(Voter, Voter1) if 
		representedBy(Voter2, Voter3).

% A Voter may not delegate its votes to any Voter 
% who is represented by the Voter.
%
false delegate(Voter1,Voter2), representedBy(Voter2,Voter1).

winningProposal(Candidate,N) at T if 
	findall(N-Candidate, voteCount(Candidate,N), L) at T,
	sort(L,SL), append(_,[N-Candidate],SL).

/** <examples>
?- go(Timeline).
?- go.
*/