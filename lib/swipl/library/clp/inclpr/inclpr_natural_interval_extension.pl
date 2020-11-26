/*  Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2011, K.U. Leuven
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

:- module(inclpr_natural_interval_extension,
	[
	    n_i_e_create_base_form/3,
	    n_i_e_create_partial_evaluation/4,
	    n_i_e_create_full_evaluation/4,
	    n_i_e_create_direct_full_evaluation/3
	]).

:- use_module(inclpr_interval_arithmetic,
	[
	    ia_binary_operator/4,
	    ia_eval/2,
	    ia_eval_g/2,
	    ia_unary_operator/3,
	    interval/1
	]).
:- use_module(inclpr_core,
	[
	    get_domain/2
	]).
:- use_module(inclpr_consistency,
	[
	    instantiate_nat/2,
	    instantiate_all_nat/1
	]).

% Module for creating evaluations of the natural interval extension.

% n_i_e_create_base_form(Function,BaseForm,Variables)
%
% Creates a base form of function <Function> for calculating the natural
% interval extension of <Function>. The base form is made by replacing all
% variable occurrences by the variable alias for the domain and by computing
% the result of ground subterms. The base form is returned in <BaseForm> and
% the variables in <Function> are returned in <Variables>.

n_i_e_create_base_form(Term,BaseForm,BaseFormVars) :-
	n_i_e_create_base_form_int(Term,BaseForm),
	term_variables(Term,BaseFormVars).

% n_i_e_create_base_form_int(Function,BaseForm)
%
% Internal predicate for creating the base form of function <Function> which
% is stored in <BaseForm>. See n_i_e_create_base_form/3.

n_i_e_create_base_form_int(Term,BaseForm) :-
	(   var(Term)
	->  get_attr(Term,inclpr_aliases,cd(_,BaseForm))
	;   number(Term)
	->  BaseForm = i(Term,Term)
	;   Term = i(_,_)
	->  BaseForm = Term
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,L),
		arg(2,Term,R),
		n_i_e_create_base_form_int(L,LBF),
		n_i_e_create_base_form_int(R,RBF),
		ia_binary_operator(Op,LBF,RBF,BaseForm)
	    ;   arg(1,Term,X),
		n_i_e_create_base_form_int(X,XBF),
		ia_unary_operator(Op,XBF,BaseForm)
	    )
	).

% n_i_e_create_partial_evaluation(BaseForm,Variables,PartialEvaluation,
%	variable)
%
% Creates a partial evaluation of the base form <BaseForm> by replacing all
% domain aliases of variables in <Variables> by the domain of the respective
% variable, except for variable <Variable>. The results of ground subterms in
% the partial evaluation are calculated. Partial evaluations are used to speed
% up consistency checks and domain narrowing, since only one variable changes
% its domain for these operations.

n_i_e_create_partial_evaluation(BaseForm,BaseFormVars,PartialEvaluation,Var) :-
	instantiate_nat(BaseFormVars,Var),
	ia_eval(BaseForm,PartialEvaluation).

% n_i_e_create_full_evaluation(PartialEvaluation,Variable,Domain,
%	FullEvaluation)
%
% Creates in <FullEvaluation> a full evaluation (i.e. an interval) from the
% partial evaluation <PartialEvaluation> by replacing the only uninstantiated
% domain alias, namely that of variable <Variable> by <Domain>.

n_i_e_create_full_evaluation(PartialEvaluation,Var,Domain,FullEvaluation) :-
	get_attr(Var,inclpr_aliases,cd(_,Domain)),
	ia_eval_g(PartialEvaluation,FullEvaluation).

% n_i_e_create_direct_full_evaluation(BaseForm,BaseFormVars,FullEvaluation)
%	
% Creates in <FullEvaluation> a full evaluation (i.e. an interval) of the base
% form <BaseForm> by replacing all domain aliases of variables in <Variables>
% by the domain of the respective variable and computing the result of the
% resulting ground expression. This predicate is used for inverted constraints,
% since they only require one evaluation per domain checking/narrowing, which
% makes partial evaluations unnecessary.

n_i_e_create_direct_full_evaluation(BF,BFV,FE) :-
	instantiate_all_nat(BFV),
	ia_eval_g(BF,FE).