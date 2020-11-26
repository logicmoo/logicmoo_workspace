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

package org.cs3.pdt.graphicalviews.preferences;

import org.cs3.pdt.graphicalviews.model.labels.BracketLabel;
import org.cs3.pdt.graphicalviews.model.labels.MiddleLabel;
import org.cs3.pdt.graphicalviews.model.labels.PostfixLabel;
import org.cs3.pdt.graphicalviews.model.labels.PrefixLabel;

/**
 * Constant definitions for plug-in preferences
 */
public class PreferenceConstants {

	/* BASE PREFERENCES */
	
	public static final String BASE_TEMPLATE = "BASE_TEMPLATE";
	
	public static final String BASE_TEMPLATE_DEFAULT = "Default";
	
	public static final String BASE_TEMPLATES_STORAGE = "BASE_TEMPLATES_STORAGE";
	
	/* REFRESH MODE */
	
	public static final String REFRESH_MODE = "REFRESH_MODE";
	
	public static final String REFRESH_MODE_MANUAL = "REFRESH_MODE_MANUAL";
	
	public static final String REFRESH_MODE_AUTOMATIC = "REFRESH_MODE_AUTOMATIC";
	
	public static final String SHOW_TOOLTIPS = "SHOW_TOOLTIPS";
	
	/* NAME CROPPING */
	
	public static final String NAME_CROPPING = "NAME_CROPPING";
	
	public static final String NAME_CROPPING_PREFIX = PrefixLabel.class.getSimpleName();
	
	public static final String NAME_CROPPING_POSTFIX = PostfixLabel.class.getSimpleName();
	
	public static final String NAME_CROPPING_BRACKET = BracketLabel.class.getSimpleName();
	
	public static final String NAME_CROPPING_MIDDLE = MiddleLabel.class.getSimpleName();
	
	/* NODE SIZE */
	
	public static final String NODE_SIZE = "NODE_SIZE";
	
	public static final String NODE_SIZE_FIXED = "NODE_SIZE_FIXED";
	
	public static final String NODE_SIZE_FIXED_WIDTH = "NODE_SIZE_FIXED_WIDTH";
	
	public static final String NODE_SIZE_NUMBER_OF_LINES = "NODE_SIZE_NUMBER_OF_LINES";
	
	public static final String NODE_SIZE_MEDIAN = "NODE_SIZE_MEDIAN";
	
	public static final String NODE_SIZE_MAXIMUM = "NODE_SIZE_MAXIMUM";
	
	public static final String NODE_SIZE_INDIVIDUAL = "NODE_SIZE_INDIVIDUAL";
	
	/* LAYOUT ALGORITHM */
	
	public static final String LAYOUT = "LAYOUT";
	
	public static final String LAYOUT_HIERARCHY = "LAYOUT_HIERARCHY";
	
	public static final String LAYOUT_ORGANIC = "LAYOUT_ORGANIC";
	
	public static final String LAYOUT_COMPACTNESS = "APPEARANCE_COMPACTNESS";

	
	/* Appearance */
	
	public static final String APPEARANCE_PREDICATE_COLOR = "APPEARANCE_PREDICATE_COLOR";
	
	public static final String APPEARANCE_EXPORTED_PREDICATE_COLOR = "APPEARANCE_EXPORTED_PREDICATE_COLOR";
	
	public static final String APPEARANCE_BORDER_COLOR = "APPEARANCE_BORDER_COLOR";
	
	public static final String APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR = "APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR";
	
	public static final String APPEARANCE_MODULE_HEADER_COLOR = "APPEARANCE_MODULE_HEADER_COLOR";
	
	public static final String APPEARANCE_NONMODULE_HEADER_COLOR = "APPEARANCE_NONMODULE_HEADER_COLOR";
	
	public static final String APPEARANCE_MODULE_FILE_BACKGROUND_COLOR = "APPEARANCE_MODULE_FILE_BACKGROUND_COLOR";
	
	public static final String APPEARANCE_LINE_COLOR = "APPEARANCE_LINE_COLOR";
	
	public static final String APPEARANCE_BORDER_STYLE = "APPEARANCE_BORDER_STYLE";

	public static final int APPEARANCE_BORDER_STYLE_SOLID = 0;
	
	public static final int APPEARANCE_BORDER_STYLE_DASHED = 1;
	
	public static final int APPEARANCE_BORDER_STYLE_DOTTED = 2;
	
	public static final int APPEARANCE_BORDER_STYLE_DASHED_DOTTED = 3;
	
	public static final String APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE = "APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE";
	
	/* VISIBILITY */
	
	public static final String VISIBILITY_PDT_PREDICATES = "VISIBILITY_PDT_PREDICATES";
	
	public static final String VISIBILITY_SWI_PREDICATES = "VISIBILITY_SWI_PREDICATES";
	
	public static final String VISIBILITY_METAPREDICATE_CALLS = "VISIBILITY_METAPREDICATE_CALLS";
	
	public static final String VISIBILITY_INFERRED_CALLS = "VISIBILITY_INFERRED_CALLS";
}


