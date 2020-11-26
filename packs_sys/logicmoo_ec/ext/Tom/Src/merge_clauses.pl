%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: merge.pl,v 1.5 1995/01/27 13:45:38 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\chapter[Die Datei {\tt tom\_merge\_clauses}]{Die Datei {\Huge \tt tom\_merge\_clauses}}

\Predicate merge_clauses/3 (+ClauseList1, +ClauseList2, -MergedClauseList).

The clauses of this predicate try to merge two lists of clauses.
The predicate basically performs a cross prduct on the elements of
the lists |ClauseList1| and |ClauseList2|.

Assuming, we have two list \((x_1)_{i=1,\ldots,n}\) and \((y_j)_{j=1,\ldots,m}\),
the cross product is a list \((f(x_i,y_j))_{i = 1,\ldots,n \atop j = 1,
\ldots,m}\). The function \(f\) is analysing the structure of the terms \(x_i\)
and \(y_i\).

The code for this is adapted from

\begin{center}
Richard O'Keefe\\
The Craft of Prolog\\
MIT Press, Cambridge, Mass.\\
1990, p.\ 243
\end{center}

\PL*/

merge_clauses([],_,[]).
merge_clauses([Clause | ClauseList1],ClauseList2,EntryList):-
	merge_clauses(ClauseList2,Clause,EntryList,Accumulator),
	merge_clauses(ClauseList1,ClauseList2,Accumulator).

merge_clauses([],_) --> [].
merge_clauses([ Clause | ClauseList1 ],ClauseList2) -->
	{ merge_to_formula(Clause,ClauseList2,ResultingClause) },
	[ResultingClause],
	merge_clauses(ClauseList1,ClauseList2).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_to_formula/3 (+Clause1, +Clause2, -MergedClause).

If we have two terms or formulas, their structures are analysed within the
predicate |merge_to_formula/3|.

We merge the two clauses according to the usual propositional
equivalences:
\begin{eqnarray*}
(\varphi_1 \to \psi_1) \vee (\varphi_2 \to \psi_2) & = & (\varphi_1 \wedge \varphi_2) \to (\psi_1 \vee \psi_2)\\
(\varphi_1 \to \psi_1) \vee \varphi_2 & = & \varphi_1 \to (\psi_1 \vee \psi_2)\\
\varphi_1 \vee (\varphi_2 \to \psi_2) & = & \varphi_2 \to (\varphi_1 \vee \psi_2)
\end{eqnarray*}

\PL*/

merge_to_formula(L1, L2, Clause):-
	( L1 = implies (Prem1, Conc1) ->
	    ( L2 = implies(Prem2, Conc2) ->
	         Clause = implies(and(Prem1,Prem2), or(Conc1,Conc2))
	    ; Clause = implies(Prem1, or(Conc1,L2))
	    )
	;   ( L2 = implies(Prem2, Conc2) ->
	        Clause = implies(Prem2, or(Conc2,L1))
	    ; Clause = or(L1, L2)
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */




