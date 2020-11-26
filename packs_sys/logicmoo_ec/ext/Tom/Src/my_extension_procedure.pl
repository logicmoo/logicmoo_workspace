%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: extension_procedure.pl,v 1.6 1994/05/17 15:56:04 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1994 Gerd Neugebauer
%%% 
%%% Net: gerd@intellektik.informatik.th-darmstadt.de
%%% 
%%%****************************************************************************

:- module(my_extension_procedure).
:- compile(library(capri)).

require_option(equality,[off]).
force_option('ProCom::post_link' = ['upp_unify.pl']).
force_option(connect_weak_unifyable = off).

descriptor
	proof(reduction(Pred)),
	template(Pred),
	template(-Pred,path).

descriptor
	proof(extension(-Pred,Index)),
	template(Pred),
	constructor((writeln(Pred),normalize_unify_a1(Pred, Pred2))),
	template(-Pred2,extension(Index)).


