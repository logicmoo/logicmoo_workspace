/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(outline_demo_multifile_contribution, []).

% Multifile contribution for entity outline_demo.
% 
% The outline presents this information with the arrow on the module icon.
:- multifile(outline_demo:likes/2).

outline_demo:likes(jack_torrance, the_overlook_hotel).

own_predicate.
