
:- module(lps_term_expander, [lps_term_expander/3,lps_f_term_expansion_now/3, lps_f_term_expansion/3]).

:- nodebug(lps(term_expand)).

:- module_transparent(lps_term_expander/3).

:- use_module(('../utils/psyntax.P'),[ 
	lps2p_file/2, lps2p/3, syntax2p_file/4, syntax2p/4, syntax2p_literal/7, golps/2, golps/1, dumpjs/2, dumpjs/1,
	file_generator_name/2, may_clear_hints/0,term_colours/2,timeless_ref/1, set_top_term/1, dumploaded/2
	]).

get_source_location(File,Line):- source_location(File,Line),!.
get_source_location(File,Line):-
	prolog_load_context(source,File), 
	prolog_load_context(term_position,TP), stream_position_data(line_position,TP,Line),!.

add_source_location(_Module,ExpandedTerm,Output):-  nonvar(ExpandedTerm), 
  \+ ((ExpandedTerm=(Compound:_), compound(Compound))),
  get_source_location(File,Line),
  Output = ('$source_location'(File, Line):ExpandedTerm), !.
add_source_location(_Module,Output,Output).

% on SWISH we'll avoid the file to file translation, by converting on a term by term basis, assuming the transform to be 1-1 (except for nlp)
% we assume the LPS transform to preserve Prolog 
lps_f_term_expansion(Module,NiceTerm,Output) :-         
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(
	% atom_prefix(File,'pengine://'), % process only SWISH windows
	lps_f_term_expansion_now(Module,NiceTerm,ExpandedTerm),!,
	add_source_location(Module,ExpandedTerm,Output),
	maybe_inform(Module,NiceTerm,ExpandedTerm).



lps_f_term_expansion_now(_Module,NiceTerm,ExpandedTerm):- 
	notrace(catch(call(call,lps_nlp_translate(NiceTerm,ExpandedTerm)),_,fail)), !. % hook for LogicalContracts extension
lps_f_term_expansion_now(_Module,NiceTerm,ExpandedTerm) :- 
	may_clear_hints, set_top_term(NiceTerm),!,
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	% somehow this fails to... some terms;-) prolog_load_context(file,File), mylog(normal-File),
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm). 

/* their origninal TE 
% on SWISH we'll avoid the file to file translation, by converting on a term by term basis, assuming the transform to be 1-1 (except for nlp)
% we assume the LPS transform to preserve Prolog 
user:term_expansion(NiceTerm,'$source_location'(File, Line):ExpandedTerms) :- 
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(
	context_module(user), % LPS programs are in the user module
	prolog_load_context(source,File), atom_prefix(File,'pengine://'), % process only SWISH windows
	prolog_load_context(term_position,TP), stream_position_data(line_position,TP,Line),
	catch(lps_nlp_translate(NiceTerm,ExpandedTerms),_,fail), !. % hook for LogicalContracts extension
user:term_expansion(NiceTerm,ExpandedTerm) :- 
	context_module(user), % LPS programs are in the user module
	prolog_load_context(source,File), atom_prefix(File,'pengine://'), % process only SWISH windows
	may_clear_hints, set_top_term(NiceTerm),
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	% somehow this fails to... some terms;-) prolog_load_context(file,File), mylog(normal-File),
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm). 

*/

:- volatile(tmp:module_dialect_lps/4).
:- thread_local(tmp:module_dialect_lps/4).

is_lps_module_and_stream_ok(Module):- dialect_input_stream(In), tmp:module_dialect_lps(In,_,Module,_),!.
is_lps_module_and_stream_ok(Module):- debugging(lps(term_expand)), 
     use_module(library(listing)),
     dialect_input_stream(In),
     debug(lps(term_expand),'~p',(tryed(tmp:module_dialect_lps(In,_,Module,_)))),
     listing(tmp:module_dialect_lps/4),!,fail.
%is_lps_module_and_stream_ok(Module):- tmp:module_dialect_lps(_,_,Module,_), !.

using_lps(Module):- 
  \+ current_prolog_flag(emulated_dialect, swi),
  is_lps_module_and_stream_ok(Module),!.


lps_term_expander(Module,NiceTerm,ExpandedTerm):- 
  using_lps(Module),
  % context_module(user), % LPS programs are in the user module
  lps_f_term_expansion(Module,NiceTerm,ExpandedTerm),!,
  prolog_load_context(variable_names, Vars),
  maybe_save_varname_info(NiceTerm,Vars,module(Module)),
  maybe_save_varname_info(ExpandedTerm,Vars,module(Module)),!.

maybe_save_varname_info(ExpandedTerm,Vars,Why):-
  expand_to_hb(ExpandedTerm,H,B),
  ignore((Vars\==[], assertz(varname_cache:varname_info(H,B,Vars,Why)))),!.
  
  
  
maybe_inform(_Module,NiceTerm,ExpandedTerm):-   
  ignore(((debugging(lps(term_expand)),
          NiceTerm\=@=ExpandedTerm,flush_output(user_error),
          format(user_error,'~N~n% LPS:  ~p.~n% Into: ~p.~n',[NiceTerm,ExpandedTerm]),
          flush_output(user_error)))).

:- module_transparent(system:term_expansion/2).

system:term_expansion(NiceTerm,ExpandedTerm):- 
  current_prolog_flag(emulated_dialect,lps),
  % compound(NiceTerm), NiceTerm \= (_:_), 
  % context_module(Module),
  prolog_load_context(module,Module),
  lps_term_expander(Module,NiceTerm,ExpandedTerm),!.
