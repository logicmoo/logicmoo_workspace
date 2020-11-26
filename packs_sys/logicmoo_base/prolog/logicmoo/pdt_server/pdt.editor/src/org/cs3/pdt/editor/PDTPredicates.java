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

package org.cs3.pdt.editor;

public class PDTPredicates {
	
	private PDTPredicates() {}

	// edit hook
	public static final String PDT_EDIT_HOOK = "pdt_edit_hook";
	
	// breakpoints
	public static final String PDT_SET_BREAKPOINT = "pdt_editor_breakpoints:pdt_set_breakpoint";
	public static final String PDT_BREAKPOINT_PROPERTIES = "pdt_editor_breakpoints:pdt_breakpoint_properties";

	// editor highlighting
	public static final String PREDICATES_WITH_PROPERTY = "pdt_editor_highlighting:predicates_with_property";
	
	// smells
	public static final String SMELL_MARKER_PDT = "pdt_smells:smell_marker_pdt";

	// outline
	public static final String MODULE_PROPERTY = "module_property";
	public static final String ENTITY_PROPERTY = "utils4entities::entity_property";
	
	// reloading included files
	public static final String FILE_TO_RELOAD_FOR_INCLUDED_FILE = "pdt_editor_files:file_to_reload_for_included_file";

}


