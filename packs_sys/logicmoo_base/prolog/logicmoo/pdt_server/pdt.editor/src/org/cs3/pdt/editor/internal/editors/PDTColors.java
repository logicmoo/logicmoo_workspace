/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.editors;

import org.eclipse.swt.graphics.RGB;

public interface PDTColors {
	public static final RGB BACKGROUND        = new RGB(255, 255, 255);
	public static final RGB BACKGROUND_EXTERN = new RGB(240, 240, 240);
	public static final RGB DEFAULT           = new RGB(  0,   0,   0);   // black        
	public static final RGB STRING            = new RGB(  0,   0, 255);
	public static final RGB COMMENT           = new RGB( 63, 127,  95);   // RGB values for Eclipse Java comments.	
	public static final RGB VARIABLE          = new RGB(139,   0,   0);
	public static final RGB UNDEFINED         = new RGB(255,   0,   0);   // RED = Call to undefined  predicate
	public static final RGB BUILTIN           = new RGB(  0,   0, 128);   // 
	public static final RGB DYNAMIC           = new RGB(110,  40,  40);   // dark brown
	public static final RGB TRANSPARENT       = new RGB(255,  80, 180);   // pink for module_transparent
	public static final RGB META              = new RGB( 15, 160,  15);   // dark green for meta_predicate
	
	public static final String PREF_BACKGROUND = "pdt.editor.colors.background";
	public static final String PREF_BACKGROUND_EXTERNAL_FILES = "pdt.editor.colors.backgroundextern";
	public static final String PREF_DEFAULT = "pdt.editor.colors.default";
	public static final String PREF_STRING = "pdt.editor.colors.string";
	public static final String PREF_COMMENT = "pdt.editor.colors.comment";
	public static final String PREF_VARIABLE = "pdt.editor.colors.variable";
	public static final String PREF_UNDEFINED = "pdt.editor.colors.undefined";
	public static final String PREF_BUILTIN = "pdt.editor.colors.buildin";
	public static final String PREF_DYNAMIC = "pdt.editor.colors.dynamic";
	public static final String PREF_TRANSPARENT = "pdt.editor.colors.transparent";
	public static final String PREF_META = "pdt.editor.colors.meta";
	
	public static final String META_PREDICATE_STRING = "Meta-Predicate";
	public static final String MODULE_TRANSPARENT_STRING = "Module-Transparent";
	public static final String DYNAMIC_STRING = "Dynamic";
	public static final String BUILT_IN_STRING = "Built-In";
	public static final String UNDEFINED_STRING = "Undefined";
	public static final String VARIABLE_STRING = "Variable";
	public static final String COMMENT_STRING = "Comment";
	public static final String STRING_STRING = "Atom";
	public static final String DEFAULT_STRING = "Other";
	public static final String BACKGROUND_STRING = "Project files";
	public static final String BACKGROUND_EXTERN_STRING = "External files";
}


