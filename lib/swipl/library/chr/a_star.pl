/*  Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(a_star,
	[
		a_star/4
	]).

:- use_module(binomialheap).

:- use_module(find).

:- use_module(library(dialect/hprolog)).

a_star(DataIn,FinalData,ExpandData,DataOut) :-
	a_star_node(DataIn,0,InitialNode),
	empty_q(NewQueue),
	insert_q(NewQueue,InitialNode,Queue),
	a_star_aux(Queue,FinalData,ExpandData,EndNode),
	a_star_node(DataOut,_,EndNode).

a_star_aux(Queue,FinalData,ExpandData,EndNode) :-
	delete_min_q(Queue,Queue1,Node),
	( final_node(FinalData,Node) ->
		Node = EndNode
	;
		expand_node(ExpandData,Node,Nodes),
		insert_list_q(Nodes,Queue1,NQueue),
		a_star_aux(NQueue,FinalData,ExpandData,EndNode)
	).

final_node(D^Call,Node) :-
	a_star_node(Data,_,Node),
	term_variables(Call,Vars),
	chr_delete(Vars,D,DVars),
	copy_term(D^Call-DVars,Data^NCall-DVars),
	call(NCall).

expand_node(D^Ds^C^Call,Node,Nodes) :-
	a_star_node(Data,Score,Node),
	term_variables(Call,Vars),
	chr_delete(Vars,D,DVars0),
	chr_delete(DVars0,Ds,DVars1),
	chr_delete(DVars1,C,DVars),
	copy_term(D^Ds^C^Call-DVars,Data^EData^Cost^NCall-DVars),
	term_variables(Node,NVars,DVars),
	find_with_var_identity(ENode,NVars,(NCall,EScore is Cost + Score,a_star:a_star_node(EData,EScore,ENode)),Nodes).

a_star_node(Data,Score,Data-Score).
