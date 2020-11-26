/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.search;

public final class SearchConstants {

	public static final String RESULT_KIND_SINGLE = "single";
	public static final String RESULT_KIND_MULTIFILE = "multifile";
	public static final String RESULT_KIND_FOREIGN = "foreign";
	public static final String RESULT_KIND_DYNAMIC = "dynamic";
	public static final String RESULT_KIND_TRANSPARENT = "transparent";
	public static final String RESULT_KIND_DWIM = "dwim";
	
	public static final String VISIBILITY_PUBLIC = "public";
	public static final String VISIBILITY_PROTECTED = "protected";
	public static final String VISIBILITY_PRIVATE = "private";
	public static final String VISIBILITY_LOCAL = "local";
	
	public static final String COMPLETION_KIND_PREDICATE = "predicate";
	public static final String COMPLETION_KIND_MODULE = "module";
	public static final String COMPLETION_KIND_ATOM = "atom";
	
	public static final String COMPLETION_DOC_KIND_NODOC = "nodoc";
	public static final String COMPLETION_DOC_KIND_TEXT = "text";
	public static final String COMPLETION_DOC_KIND_HTML = "html";
	public static final String COMPLETION_DOC_KIND_FILE = "file";
	public static final String COMPLETION_DOC_KIND_LGT_HELP_FILE = "lgt_help_file";
	
	// search goals
	public static final String PREDICATE_GOAL_FUNCTOR = "predicate";
	
	public static final String PROPERTY_LABEL = "label";
	public static final String PROPERTY_SHOW_LINE = "show_line";
	public static final String PROPERTY_PREFIX = "prefix";
	public static final String PROPERTY_SUFFIX = "suffix";
	public static final String PROPERTY_LINE = "line";

	private SearchConstants() {}
}
