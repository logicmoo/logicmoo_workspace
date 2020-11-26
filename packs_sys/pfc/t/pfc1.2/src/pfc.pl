%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

mpred_Version(2.2).

mpred_File('pfcsyntax').	% operator declarations.
mpred_File('pfccore').	% core of Pfc.
mpred_File('pfcsupport').	% support maintenance
mpred_File('pfcdb').	% predicates to manipulate database.
mpred_File('pfcdebug').	% debugging aids (e.g. tracing).
mpred_File('pfcjust').	% predicates to manipulate justifications.
mpred_File('pfcwhy').	% interactive exploration of justifications.

pfcLoad :- mpred_File(F), ensure_loaded(F), fail.
pfcLoad.

pfcFcompile :- mpred_File(F), fcompile(F), fail.
pfcFcompile.

:- pfcLoad.

