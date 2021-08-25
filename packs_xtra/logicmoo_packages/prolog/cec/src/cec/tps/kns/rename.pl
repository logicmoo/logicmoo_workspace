/*
 *	file:		rename.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to rename operators in kns.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

%       Renaming of operators in kns.
%
%       The rename predicate expects a list of old-new operator associations.
%       For operators in the precedence which don't occur in the association
%       list the identity association is assumed.
%
%       The association is expected to be a function (uniqueness). Two cases
%       can be distinguished:
%
%         - the association is injective
%
%           only the syntactical renaming of operators
%           in the precedence is neccessary. Prior termination proofs
%           remain modulo renaming valid.
%
%         - the association is not injective
%		(not any longer admitted 19.12.88)
%
%           it now may happen, that via renaming prior different operators
%           become identical, so that prior termination proofs are no longer
%           valid. Only in the case where such identifcations can be regarded
%           as consistent precedence extensions no reproof is neccessary.
%
%
%       kns_rename(->AssocList).
%
%       Renames the operators in the current precedence according the
%       association list "AssocList", if consistently possible (prior
%       termination proofs remain valid). Otherwise the predicate fails.
%
kns_rename(_).


