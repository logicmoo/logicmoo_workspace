/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


:- dynamic(pattern/4).
:- dynamic(allo/9).
:- dynamic(edge/7).
:- dynamic(syntagmeme/5).
:- dynamic(allosyntagma/5).
:- dynamic(morph_synt/5).
:- dynamic(m/4).
:- dynamic(hm/5).
:- dynamic(paradigm/2).
:- dynamic(subdialect/3).
:- dynamic(phonetic_class/3).
:- dynamic(default_analysis/5).
:- dynamic(ph/10).
:- dynamic(historic_pf/5).
:- dynamic(tp_pf/4).
:- dynamic(current_lang/1).
:- dynamic(current_l2/1).
:- dynamic(current_output_file/1).
:- dynamic(cg/3).
:- dynamic(transcr_table/2).

:- dynamic(trace_morph/1).
:- dynamic(trace_synt/1).
:- dynamic(trace_gen/1).
:- dynamic(tracing_mode/1).

:- dynamic(available_language/2).

/* DEPENDENCY PARSER */

:- dynamic(morph_grammar_type/2).
:- dynamic(synt_grammar_type/2).
:- dynamic(dep/4).
:- dynamic(arc/3).
:- dynamic(morph_threshold/2).
:- dynamic(lexical/4).
:- dynamic(dg_class_macro/4).
:- dynamic(dep_threshold/3).
:- dynamic(max_dep_threshold/2).


/* TOKI PONA */
:- dynamic(tparadigm/2).
:- dynamic(tpconstr/4).

