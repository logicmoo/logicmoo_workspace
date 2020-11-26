/*************************************************************************

  name: mode-props.pl
  description: reset_operations and set_properties for different modes,
               wrt amount of material to be produced by different
	       modalities
 
*************************************************************************/

%-------------------------------------------------
%
%   Driving mode
%
%-------------------------------------------------

reset_operations_driving( [ set( program_state, run),
		    set( language, Lang ),
		    set( voice, max ), 
		    set( graph, no ), 
		    set( speechinput, indet ), 
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
%				 telephone=device_telephone]) ),
%		    set( devices, record([telephone=device_telephone]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	language( Lang ),
	domain( Domain ).


set_properties_driving:-
	selected_resources(Rs),
	setprop( resources,Rs),
	
	setprop( tis, yes),
	selected_datatypes(Ds),
	setprop( tis-datatype_files, Ds),
	rivs(RIVs),
	setprop( tis-rivs,RIVs),
	mivs(MIVs),
	setprop( tis-mivs,MIVs),
	infostate_variable_of_type(ISVar,ISType),
	setprop( tis-infostate, ISVar:ISType),
	setof(alias(Alias,Path),alias(Alias,Path),Aliases),
	setprop( tis-aliases,Aliases),
	setof(macro_cond(Macro,Cond),macro_cond(Macro,Cond),MacroConds),
	%setof(macro_op(Macro,Op),macro_op(Macro,Op),MacroOps),
	%append(MacroOps,MacroConds,Macros),
	setprop( tis-macros,MacroConds),
	reset_operations_driving(ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
%	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,no),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).





%-------------------------------------------------
%
%   Telephone mode
%
%-------------------------------------------------


reset_operations_telephone( [ set( program_state, run),
		    set( language, Lang ),
		    set( voice, min ), 
		    set( graph, no ), 
		    set( speechinput, indet ), 
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
%				 telephone=device_telephone]) ),
%		    set( devices, record([telephone=device_telephone]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	language( Lang ),
	domain( Domain ).


set_properties_telephone:-
	selected_resources(Rs),
	setprop( resources,Rs),
	
	setprop( tis, yes),
	selected_datatypes(Ds),
	setprop( tis-datatype_files, Ds),
	rivs(RIVs),
	setprop( tis-rivs,RIVs),
	mivs(MIVs),
	setprop( tis-mivs,MIVs),
	infostate_variable_of_type(ISVar,ISType),
	setprop( tis-infostate, ISVar:ISType),
	setof(alias(Alias,Path),alias(Alias,Path),Aliases),
	setprop( tis-aliases,Aliases),
	setof(macro_cond(Macro,Cond),macro_cond(Macro,Cond),MacroConds),
	%setof(macro_op(Macro,Op),macro_op(Macro,Op),MacroOps),
	%append(MacroOps,MacroConds,Macros),
	setprop( tis-macros,MacroConds),
	reset_operations_telephone(ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
%	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,no),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).



%-------------------------------------------------
%
%   Meeting mode
%
%-------------------------------------------------


reset_operations_meeting( [ set( program_state, run),
		    set( language, Lang ),
		    set( voice, no ), 
		    set( graph, max ), 
		    set( speechinput, indet ), 
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
%				 telephone=device_telephone]) ),
%		    set( devices, record([telephone=device_telephone]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	language( Lang ),
	domain( Domain ).


set_properties_meeting:-
	selected_resources(Rs),
	setprop( resources,Rs),
	
	setprop( tis, yes),
	selected_datatypes(Ds),
	setprop( tis-datatype_files, Ds),
	rivs(RIVs),
	setprop( tis-rivs,RIVs),
	mivs(MIVs),
	setprop( tis-mivs,MIVs),
	infostate_variable_of_type(ISVar,ISType),
	setprop( tis-infostate, ISVar:ISType),
	setof(alias(Alias,Path),alias(Alias,Path),Aliases),
	setprop( tis-aliases,Aliases),
	setof(macro_cond(Macro,Cond),macro_cond(Macro,Cond),MacroConds),
	%setof(macro_op(Macro,Op),macro_op(Macro,Op),MacroOps),
	%append(MacroOps,MacroConds,Macros),
	setprop( tis-macros,MacroConds),
	reset_operations_meeting(ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
%	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,no),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).



%-------------------------------------------------
%
%   At Home mode
%
%-------------------------------------------------


reset_operations_athome( [ set( program_state, run),
		    set( language, Lang ),
		    set( voice, min ), 
		    set( graph, max ), 
		    set( speechinput, indet ), 
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
%				 telephone=device_telephone]) ),
%		    set( devices, record([telephone=device_telephone]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	language( Lang ),
	domain( Domain ).


set_properties_athome:-
	selected_resources(Rs),
	setprop( resources,Rs),
	
	setprop( tis, yes),
	selected_datatypes(Ds),
	setprop( tis-datatype_files, Ds),
	rivs(RIVs),
	setprop( tis-rivs,RIVs),
	mivs(MIVs),
	setprop( tis-mivs,MIVs),
	infostate_variable_of_type(ISVar,ISType),
	setprop( tis-infostate, ISVar:ISType),
	setof(alias(Alias,Path),alias(Alias,Path),Aliases),
	setprop( tis-aliases,Aliases),
	setof(macro_cond(Macro,Cond),macro_cond(Macro,Cond),MacroConds),
	%setof(macro_op(Macro,Op),macro_op(Macro,Op),MacroOps),
	%append(MacroOps,MacroConds,Macros),
	setprop( tis-macros,MacroConds),
	reset_operations_athome(ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
%	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,no),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).


