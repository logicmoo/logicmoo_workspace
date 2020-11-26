/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * This module provides the infrastructure for managing CTC data 
 * (PEFs) and CTC programs (CTs and CT sequences) in multiple  
 * modules without having to add a module parameter to each CTC 
 * operation.
 * 
 * The directive ":- ctc_module(name, F/N_list)." to be added at the  
 * beginning of a CTC program file declares that this file contains 
 * CT and/or CT sequence definitions in the Prolog module 'name' 
 * with their heads' functor/arity according to F/N_list.  
 */
 
:- module(ctc_admin_copy,[
     % These two rather belong to the interpreter:
     ctc_id_init_pdt/0,
     new_node_id_pdt/1                    % (-NewId)
]).

:- use_module(pdt_prolog_library(logging)).
/*****************************************************************
 * Predefined predicates: prev_ctc_id/1, ctc_id_init/0, new_node_id/1
 */

/*
 * Identity counter. Initialized to 10000.
 */
:- dynamic prev_ctc_id/1.

ctc_id_init_pdt :-
  retractall(prev_ctc_id(_)),
  assert(prev_ctc_id(10000)).

?- ctc_id_init_pdt.

/*
 * new_node_id(-Id): Bind id to a unique term.
 */
new_node_id_pdt(NewId) :-
   clause(user:new_id(_),_) % If new_id is defined (=JT is running)
    -> user:new_id(NewId)        % ... use new_id to ensure consistency
     ; ( var(NewId)         % ... otherwise use own implementation
         -> ( prev_ctc_id(Last),
              NewId is Last+1,
              retract(prev_ctc_id(Last)),
              !,
              assert(prev_ctc_id(NewId))
            )
          ; ( term_to_atom(NewId,Id),
              ctc_warning('Ignored call of operation new_node_id(Id) with non-variable Parameter (Id = ~w).' ,[Id]) 
            )
       ).

