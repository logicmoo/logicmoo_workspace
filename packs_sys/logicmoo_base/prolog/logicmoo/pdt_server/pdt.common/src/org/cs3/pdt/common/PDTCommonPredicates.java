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

package org.cs3.pdt.common;

public class PDTCommonPredicates {

	// entry points
	public static final String MODULE_PDT_ENTRY_POINTS = "pdt_entry_points";
	public static final String PDT_ENTRY_POINT = "pdt_entry_points:pdt_entry_point";
	public static final String ADD_ENTRY_POINT = "pdt_entry_points:add_entry_point";
	public static final String REMOVE_ENTRY_POINTS = "pdt_entry_points:remove_entry_points";
	
	// search
	public static final String FIND_PREDICATE_REFERENCE = "pdt_search:find_predicate_reference";
	public static final String UPDATE_PREDICATE_REFERENCE_SEARCH_TERM_TO_CONTEXT = "pdt_search:update_predicate_reference_search_term_to_context";
	public static final String FIND_PREDICATE_DEFINITIONS = "pdt_search:find_predicate_definitions";
	public static final String FIND_CATEGORIZED_PREDICATE_DEFINITIONS = "pdt_search:find_categorized_predicate_definitions";
	public static final String FIND_PRIMARY_DEFINITION_VISIBLE_IN = "pdt_search:find_primary_definition_visible_in";
	public static final String FIND_ALTERNATIVE_PREDICATES = "pdt_search:find_alternative_predicates";
	public static final String FIND_DEFINITION_CONTAINED_IN = "pdt_search:find_definition_contained_in";
	public static final String FIND_COMPLETION = "pdt_search:find_completion";
	public static final String FIND_MODULE_REFERENCE = "pdt_search:find_module_reference";
	public static final String FIND_ENTITY_DEFINITION = "pdt_search:find_entity_definition";
	public static final String LOADED_FILE = "pdt_search:loaded_file";

	// source files
	public static final String PDT_SOURCE_FILES = "source_files:pdt_source_files";
	public static final String PDT_SOURCE_FILE = "source_files:pdt_source_file";
	
	// call analysis
	public static final String FIND_UNDEFINED_CALL = "pdt_call_analysis:find_undefined_call";
	public static final String FIND_DEAD_PREDICATE = "pdt_call_analysis:find_dead_predicate";

	// meta predicate search
	public static final String FIND_UNDECLARED_META_PREDICATE = "pdt_call_analysis:find_undeclared_meta_predicate";
	
	// call hierarchy
	public static final String FIND_PREDICATE_DECLARATION_AND_VISIBILITY = "pdt_call_hierarchy:find_predicate_declaration_and_visibility";
	public static final String FIND_CALLER = "pdt_call_hierarchy:find_caller";
	public static final String FIND_CALLEE = "pdt_call_hierarchy:find_callee";
	public static final String FIND_CALL_LOCATION = "pdt_call_hierarchy:find_call_location";
	
}


