%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       STATISTICS TOOLS FOR DEBUGGING 
%       Tested with ECLIPSE 5.3 and SWI Prolog over Linux RH 7.0-7.2
%
%	c) Sebastian Sardina      Many rights reserved		(Dec 2001)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This library provides predicates to "inspect" where a Prolog program is 
% "spending" the execution time      
%
% This file provides:
%
%  -- callt(G,T): T is the time of executing T          */
%  -- callt(G)  : like callt/2 but the time is printed  */
%  -- dtime(L,G): ***main tool for debuging a program***
%                 Asserts a prediacte time(L,G,T) where T is the execution 
%                 time of goal G (sound only if no backtracking on G)  
%  -- collect_time: collects all time(L,G,T) with the same label L and asserts 
%                    predicate ttime(L,TT,N) where TT is the sum of
%                    all T's and N is the number of time/2 collected 
%                    (i.e, calls to G)  
%  -- showstat: print out statistics after the use of dtime/3     
%  -- showmax(L,G,T):- goal G of label L took the maximum time T
%  -- cleanstat: clean up all statistic information in the database
%
% The following predicates are required:
%  --- stime(T): returns system time T              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic time/3,   % stores each goal call using a label
          ttime/3.  % stores the total time of a goal
                    % labeled and the no of times called

% T is the time of calling G
callt(G,T):- stime(I), call(G), stime(E), T is E-I.

% prints out the time of calling G
callt(G)  :- callt(G,T), nl, write('Time: '), write(T).

% MAIN PREDICATE FOR DEBUGING
% Use in programs with a label to inspect how much time
% is spent in a section of a Prolog program
dtime(Label,Goal):- callt(Goal,T), assert(time(Label,Goal,T)).

% Collect all time/2 into ttime/3 to state how much time
% per label and the number of calls 
collect_dtime:- setof(L,O^P^time(L,P,O),LL), member(ML,LL),
	        findall(T,time(ML,_,T),LTime), 
                length(LTime,NoCalls), sumup(LTime,TT), 
                assert(ttime(ML,TT,NoCalls)), fail.
collect_dtime.

% After using dtime/2 in a program, collect total time
% per label using collect_dtime and prints out such data
cleanstat:- retractall(ttime(_,_,_)),  retractall(time(_,_,_)).

showstat:- retractall(ttime(_,_,_)), once(collect_dtime), 
	   ttime(L,T,N), nl,
	   write('Label: '), write(L), 
           write(' Seconds: '), write(T),
	   write(' Times Called: '), write(N),
	   fail.
showstat.

% Goal G of label L took the maximum time
showmax(L,G,T):- time(L,G,T), \+ (time(_,_,T2), T2>T).
	   
% Sum all numbers in a list of numbers
sumup([N],N).
sumup([N|L],T):- sumup(L,T2), T is T2+N.