/* Part of dcgutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(dcg_progress, [
		seqmap_with_progress//3
	,	seqmap_with_progress//4
	,	stats/0
	,	stats/1
   ]).

:- meta_predicate
		seqmap_with_progress(+,3,+,?,?),
		seqmap_with_progress(+,4,+,?,?,?).

%% seqmap_with_progress( +Period:natural, +Pred:pred(A,S,S), +X:list(A))// is nondet.
%% seqmap_with_progress( +Period:natural, +Pred:pred(A,B,S,S), +X:list(A), ?Y:list(B))// is nondet.
%
%  Just like seqmap//2 and seqmap//3 but prints progress and memory usage statistics while running.
%  Information is printed every Period iterations. The first input list must be
%  valid list skeleton with a definite length, so that a percentage progress indicator
%  can be printed.
seqmap_with_progress(E,P,X) --> {progress_init(E,X,Pr0)}, smp(X,P,Pr0).
seqmap_with_progress(E,P,X,Y) --> {progress_init(E,X,Pr0)}, smp(X,Y,P,Pr0).

smp([],_,Pr) --> !, {progress_finish(Pr)}.
smp([X|XX],P,Pr1) --> {progress_next(Pr1,Pr2)}, call(P,X), !, smp(XX,P,Pr2).

smp([],_,_,Pr) --> !, {progress_finish(Pr)}.
smp([X|XX],[Y|YY],P,Pr1) --> {progress_next(Pr1,Pr2)}, call(P,X,Y), !, smp(XX,YY,P,Pr2).


progress_init(E,X,pr(T0,T,E,0,0)) :- length(X,T), get_time(T0).
progress_finish(Pr) :-
	progress_next(Pr,_),
	get_time(T1), Pr=pr(T0,N,_,_,_), 
	format('\nFinished ~w items in ~3g minutes.\n',[N,(T1-T0)/60]).

progress_next(pr(T0,Total,E,N,E),pr(T0,Total,E,M,1)) :- !, 
	succ(N,M),
	stats(Codes),
	get_time(T1), 
	format('~s | done ~0f% in ~3g s    \r', [Codes,100*N/Total,T1-T0]),
	flush_output.

progress_next(pr(T0,T,E,N,C),pr(T0,T,E,M,D)) :- succ(C,D), succ(N,M).


%% stats is det.
%
%  Print memory usage statistics.
stats :- !, 
	stats(Codes),
	format('~s\r',[Codes]),
	flush_output.

%% stats( -Codes:list(code)) is det.
%
%  Return memory usage statistics as a list of codes.
stats(Codes) :- !, 
	statistics(heapused,Heap),
	statistics(localused,Local),
	statistics(globalused,Global),
	statistics(trailused,Trail),
	format(codes(Codes), 'heap: ~t~D ~18| local: ~t~D ~36| global: ~t~D ~57| trail: ~t~D ~77|',
		[Heap,Local,Global,Trail]).

