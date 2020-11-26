/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

:- use_module(library(pfc_lib)).

:- dynamic(baseKB:used_ain_syntax/8).
:- prolog_load_context(source,File),stream_property(Source,file_name(File)),
   once(line_count(Source,Start);(stream_property(Source,position(Pos)),stream_position_data(line_count,Pos,Start))),
   set_how_virtualize_file(false,File,Start),
   %set_how_virtualize_file(bodies,File,Start),
   asserta(baseKB:used_ain_syntax(prolog,File,Start,Ln,VZ,M,In, (:- (M:dyn_load(In,(mfl4(VZ,M,File,Ln),ax)))))).

/*

:- multifile(term_expansion/4).
:- dynamic(term_expansion/4).
:- module_transparent(term_expansion/4).
term_expansion(MIn,Pos,Out,PosOut):- nonvar(Pos), nonvar(MIn),
   must((once(prolog_load_context(file,File);prolog_load_context(stream,File)),
   once(nb_current('$variable_names',VZ);VZ=[]),
   once(stream_position_data(line_count,Pos,Ln);(prolog_load_context(stream,Str),line_count(Str,Ln))),
   strip_module(MIn,M,In))),
   notrace(In \= (:- _)),
   is_ain_clause(M,In),
   show_failure(baseKB:used_ain_syntax(File,Start,Ln,VZ,M,In,Out)), % 
   show_failure(Ln>=Start),!,
   must(get_current_clause(MIn)),
   dmsg_pretty(Out), PosOut=Pos,!.

*/

