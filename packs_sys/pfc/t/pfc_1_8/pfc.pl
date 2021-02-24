%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

pfcVersion(1.2).

pfcFile('pfcsyntax').	% operator declarations.
pfcFile('pfccore').	% core of Pfc.
pfcFile('pfcsupport').	% support maintenance
pfcFile('pfcdb').	% predicates to manipulate database.
pfcFile('pfcdebug').	% debugging aids (e.g. tracing).
pfcFile('pfcjust').	% predicates to manipulate justifications.
pfcFile('pfcwhy').	% interactive exploration of justifications.

pfcLoad :- pfcFile(F), ensure_loaded(F), fail.
pfcLoad.

pfcFcompile :- pfcFile(F), fcompile(F), fail.
pfcFcompile.

:- pfcLoad.

