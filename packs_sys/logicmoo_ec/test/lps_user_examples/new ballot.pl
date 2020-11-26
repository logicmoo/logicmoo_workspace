:- expects_dialect(lps).

% Simplified ballot by RAK and MC: 
% http://solidity.readthedocs.io/en/v0.4.24/solidity-by-example.html
maxTime(15).

events 	
	% ballot(_Chairman, _Proposals), 
	% giveRightToVote(_Chairman, _Voter), 
	delegate(_FromVoter, _ToVoter), vote(_Voter, _Candidate).

fluents	
% chairman(_Chairman), 
voter(_Voter, _Weight), voted(_Voter, _Candidate), 
delegateOf(_Voter,_D), voteCount(_Candidate, _Votes).

initially voter(bob, 1), voter(fariba, 1), voter(jacinto, 1), voter(miguel, 1).
initially voteCount(trump, 0), voteCount(clinton, 0).                                                                    
                                                                   
observe delegate(bob, miguel)  from 4 to 5.
observe vote(miguel, clinton)  from 5 to 6.
observe vote(jacinto, clinton)  from 5 to 6.
% observe delegate(jacinto,bob) from 6 to 7.
observe delegate(fariba, miguel)  from 4 to 5.
% observe delegate(fariba, miguel)  from 7 to 8.

/*
observe initiates(voter(sam, 2)) from 8 to 9.
initiates(Fluent) initiates Fluent.
*/

% Alternatively, delegate and vote terminates voter?
delegate(Voter1, _Voter2) updates Old to 0 in voter(Voter1, Old).
delegate(Voter1, Voter2) updates Old to New in voter(Voter3, Old) if
		delegateOf(Voter2,Voter3), voter(Voter1, N1), New is N1 + Old. 

% The next clause deals with the case a delegate has already voted when a delegation is made:
delegate(Voter1, Voter2) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes) if
		delegateOf(Voter2,Voter3), voted(Voter3, Candidate),  
		voter(Voter1, N), NewVotes is OldVotes + N.
 
delegate(Voter1,Voter2) initiates voted(Voter1,delegated(Voter2)). % Confusing.
% better a different predicate?

% deal with delegate chains
delegateOf(Voter,D) if voted(Voter,delegated(DD)), delegateOf(DD,D).
delegateOf(Voter,Voter) if not voted(Voter,delegated(_)).

false delegate(Voter,_), voted(Voter,_).
false delegate(Voter1,Voter2), Voter1==Voter2.
false delegate(Voter, Voter1), delegate(Voter, Voter2), Voter1 \= Voter2.
% prevent delegation loops:
false delegate(Voter1,Voter2), delegateOf(Voter2,Voter1).

vote(Voter, Candidate) initiates voted(Voter, Candidate).

vote(Voter, Candidate) updates OldVotes to NewVotes in voteCount(Candidate, OldVotes)	if
		voter(Voter, N), 
		NewVotes is OldVotes + N.

false vote(Voter,_), voted(Voter,_).
false vote(Voter, Candidate1), vote(Voter, Candidate2), Candidate1 \= Candidate2.

/*
winningProposal(Candidate,N) at T if 
	findall(N-Candidate, voteCount(Candidate,N), L) at T,
	sort(L,SL), append(_,[N-Candidate],SL).

if winningProposal(_Candidate,4) then lps_terminate from _.
*/

/** <examples>
?- go(Timeline).
*/