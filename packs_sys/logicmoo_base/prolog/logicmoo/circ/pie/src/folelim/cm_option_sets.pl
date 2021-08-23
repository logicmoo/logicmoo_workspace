%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cm_option_sets, [cm_option_set/5,
			   pr_option_set/2]).

%% size estitmate: 1-10

% [mr2,r4,r5,hd1,sg(0),r4_limit(100000),l,p]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cm_option_set(std, nonhorn, _, 1,
	      [r1,mr2,r4,r5,hd1,sg(0),r4_limit(100000),r8(al)]).
cm_option_set(std, nonhorn, _, 5,
	      [r1,mr2,hd1,r4a,r5,sq(0),r4a_max(10)]).
cm_option_set(std, nonhorn, _, 8,
	      [hd1,mr2,sq(0),r8(al)]).
cm_option_set(std, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),r8(al)]).
cm_option_set(std, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0),r4a_max(10)]).

%% incomplete, leancop style cut after reduction and after lemma solution:
cm_option_set(lean, nonhorn, _, 1,
	      [xlc,mrc,l,r1,r5,hd1,sg(0),r8(al)]).
cm_option_set(lean, nonhorn, _, 5,
	      [xlc,mrc,l,r1,hd1,r5,sq(0)]).
cm_option_set(lean, nonhorn, _, 7,
	      [xlc,l,mrn,hd1,r5,sq(0)]).
cm_option_set(lean, horn, _, 1,
	      [xlc,l,r5,hd1,mrn,no_fan,sg(0),r8(al)]).
cm_option_set(lean, horn, _, 5,
	      [xlc,l,r5,hd1,mrn,no_fan,sq(0)]).

cm_option_set(lean1, nonhorn, _, 1,
	      [mrc,l,r1,r5,hd1,sg(0),r8(al)]).
cm_option_set(lean1, nonhorn, _, 5,
	      [mrc,l,r1,hd1,r5,sq(0)]).
cm_option_set(lean1, nonhorn, _, 7,
	      [l,mrn,hd1,r5,sq(0)]).
cm_option_set(lean1, horn, _, 1,
	      [l,r5,hd1,mrn,no_fan,sg(0),r8(al)]).
cm_option_set(lean1, horn, _, 5,
	      [l,r5,hd1,mrn,no_fan,sq(0)]).

cm_option_set(low, nonhorn, _, 1,
	      [mr2,hd1,sg(0)]).
cm_option_set(low, nonhorn, _, 5,
	      [mr2,hd1,sq(0)]).
cm_option_set(low, nonhorn, _, 7,
	      [hd1,mrn,sq(0)]).
cm_option_set(low, horn, _, 1,
	      [hd1,mrn,no_fan,sg(0)]).
cm_option_set(low, horn, _, 5,
	      [hd1,mrn,no_fan,sq(0)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% EXPERIMENTAL
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cm_option_set(std, nonhorn, eq, 1,
% 	      [steq,r1,mr2,r4,r5,hd1,sg(0),r4_limit(100000),r8(al)]).
% cm_option_set(std, nonhorn, eq, 5,
% 	      [steq,r1,mr2,hd1,r4a,r5,sq(0),r4a_max(10)]).
% cm_option_set(std, nonhorn, eq, 8,
% 	      [steq, hd1,mr2,sq(0),r8(al)]).
% cm_option_set(std, horn, eq, 1,
% 	      [steq,r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),r8(al)]).
% cm_option_set(std, horn, eq, 5,
% 	      [steq,r4a,r5,hd1,mrn,no_fan,sq(0),r4a_max(10)]).
% 
% cm_option_set(hd, nonhorn, eq, 1,
% 	      [steq,r1,mr2,r4,r5,hd,sg(0),r4_limit(100000),r8(al)]).
% cm_option_set(hd, nonhorn, eq, 5,
% 	      [steq,r1,mr2,hd,r4a,r5,sq(0),r4a_max(10)]).
% cm_option_set(hd, nonhorn, eq, 8,
% 	      [steq, hd,mr2,sq(0),r8(al)]).
% cm_option_set(hd, horn, eq, 1,
% 	      [steq,r4,r5,hd,mrn,no_fan,sg(0),r4_limit(100000),r8(al)]).
% cm_option_set(hd, horn, eq, 5,
% 	      [steq,r4a,r5,hd,mrn,no_fan,sq(0),r4a_max(10)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% OLD VERSIONS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cm_option_set(std_02, nonhorn, _, 1,
	      [r1,mr2,r4,r5,hd1,sg(0),r4_limit(100000),r8(al)]).
cm_option_set(std_02, nonhorn, _, 5,
	      [r1,mr2,hd1,r4a,r5,sq(0),r4a_max(10),r8(al)]).
cm_option_set(std_02, nonhorn, _, 8,
	      [hd1,mr2,sq(0),r8(al)]).
cm_option_set(std_02, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),r8(al)]).
cm_option_set(std_02, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0),r4a_max(10),r8(al)]).

cm_option_set(lean_02, nonhorn, _, 1,
	      [xlc,mrc,l,r1,r5,hd1,sg(0),r8(al)]).
cm_option_set(lean_02, nonhorn, _, 5,
	      [xlc,mrc,l,r1,hd1,r5,sq(0),r8(al)]).
cm_option_set(lean_02, nonhorn, _, 7,
	      [xlc,l,mrn,hd1,r5,sq(0),r8(al)]).
cm_option_set(lean_02, horn, _, 1,
	      [xlc,l,r5,hd1,mrn,no_fan,sg(0),r8(al)]).
cm_option_set(lean_2, horn, _, 5,
	      [xlc,l,r5,hd1,mrn,no_fan,sq(0),r8(al)]).

cm_option_set(std_01, nonhorn, _, 1,
	      [r1,mr2,r4,r5,hd1,sg(0),r4_limit(100000)]).
cm_option_set(std_01, nonhorn, _, 5,
	      [r1,mr2,hd1,r4a,r5,sq(0)]).
cm_option_set(std_01, nonhorn, _, 8,
	      [hd1,mr2,sq(0)]).
cm_option_set(std_01, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000)]).
cm_option_set(std_01, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0)]).

cm_option_set(lem, nonhorn, _, 1,
	      [r1,mr2,r4,r5,hd1,sg(0),r4_limit(100000),l]).
cm_option_set(lem, nonhorn, _, 5,
	      [r1,mr2,hd1,r4a,r5,sq(0),l]).
cm_option_set(lem, nonhorn, _, 8,
	      [hd1,mr2,sq(0),l]).
cm_option_set(lem, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),l]).
cm_option_set(lem, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0),l]).

cm_option_set(lem_hd, nonhorn, _, 1,
	      [r1,mr2,r4,r5,hd,sg(0),r4_limit(100000),l]).
cm_option_set(lem_hd, nonhorn, _, 5,
	      [r1,mr2,hd,r4a,r5,sq(0),l]).
cm_option_set(lem_hd, nonhorn, _, 8,
	      [hd,mr2,sq(0),l]).
cm_option_set(lem_hd, horn, _, 1,
	      [r4,r5,hd,mrn,no_fan,sg(0),r4_limit(100000),l]).
cm_option_set(lem_hd, horn, _, 5,
	      [r4a,r5,hd,mrn,no_fan,sq(0),l]).

%% incomplete, leancop style cut after reduction and after lemma solution:
cm_option_set(lean2_01, nonhorn, _, 1,
	      [xlc,l,r1,r5,hd1,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean2_01, nonhorn, _, 5,
	      [xlc,l,r1,hd1,r5,sq(0),mrc]).
cm_option_set(lean2_01, nonhorn, _, 7,
	      [xlc,l,mrn,hd1,r5,sq(0),mrc]).
cm_option_set(lean2_01, horn, _, 1,
	      [xlc,l,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean2_01, horn, _, 5,
	      [xlc,l,r5,hd1,mrn,no_fan,sq(0),mrc]).

%% incomplete, leancop style cut after reduction:
cm_option_set(lean1_01, nonhorn, _, 1,
	      [l,r1,r5,hd1,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean1_01, nonhorn, _, 5,
	      [l,r1,hd1,r5,sq(0),mrc]).
cm_option_set(lean1_01, nonhorn, _, 7,
	      [l,mrn,hd1,r5,sq(0),mrc]).
cm_option_set(lean1_01, horn, _, 1,
	      [l,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean1_01, horn, _, 5,
	      [l,r5,hd1,mrn,no_fan,sq(0),mrc]).

%% incomplete, leancop style cut after reduction:
cm_option_set(lean_01, nonhorn, _, 1,
	      [r1,r4,r5,hd1,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean_01, nonhorn, _, 5,
	      [r1,hd1,r4a,r5,sq(0),mrc]).
cm_option_set(lean_01, nonhorn, _, 7,
	      [mrn,hd1,r4a,r5,sq(0),mrc]).
cm_option_set(lean_01, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000),mrc]).
cm_option_set(lean_01, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0),mrc]).

cm_option_set(bup, nonhorn, _, 1,
	      [r1,r4,r5,hd1,sg(0),r4_limit(100000)]).
cm_option_set(bup, nonhorn, _, 5,
	      [r1,hd1,r4a,r5,sq(0)]).
cm_option_set(bup, nonhorn, _, 7,
	      [mrn,hd1,r4a,r5,sq(0)]).
cm_option_set(bup, horn, _, 1,
	      [r4,r5,hd1,mrn,no_fan,sg(0),r4_limit(100000)]).
cm_option_set(bup, horn, _, 5,
	      [r4a,r5,hd1,mrn,no_fan,sq(0)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pr_option_set(std, [i(dyna(2,2,10,10,1))]).
pr_option_set(bup, [i(dyna(2,2,10,10,1))]).
pr_option_set(lean, [i(dyna(2,2,10,10,1))]).
