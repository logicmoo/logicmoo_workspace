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

package org.cs3.pdt.console;


final public class PDTConsole {
	public static final String CONSOLE_VIEW_ID="org.cs3.pdt.console.internal.views.PrologConsoleView";
    /*
     * the port on which the prolog console server is listening.
     */

	public static final String CONTRIB_PROCESS_SELECTOR_ID = "pdt.console.contribution.processselector";
	public static final String COMMAND_PASTE_FILENAME = "pdt.console.paste_filename";
	public static final String CONTEXT_USING_CONSOLE_VIEW = "org.cs3.pdt.console";
	public static final int ERR_UNKNOWN = -1;
	public static final int ERR_PROCESS = -2;
	public static final int CX_CONSOLE_VIEW_ATTACH_TO_PROCESS = -3;
	public static final int CX_CONSOLE_SWITCH_PROCESS = -4;

	// Font & Color
	public static final String PREF_CONSOLE_FONT = "pdt.console.font";
	public static final String PREF_CONSOLE_COLOR_NORMAL = "pdt.console.colors.normal";
	public static final String PREF_CONSOLE_COLOR_ERROR = "pdt.console.colors.error";
	public static final String PREF_CONSOLE_COLOR_WARNING = "pdt.console.colors.warning";

	public static final String PREF_CONSOLE_COLOR_INFO = "pdt.console.colors.info";

	public static final String PREF_CONSOLE_COLOR_DEBUG = "pdt.console.colors.debug";

	public static final String PREF_CONSOLE_COLOR_BACKGROUND_NORMAL = "pdt.console.colors.background.normal";
	public static final String PREF_CONSOLE_COLOR_BACKGROUND_SINGLE_CHAR_MODE = "pdt.console.colors.background.singlecharmode";
	public static final String PREF_CONSOLE_COLOR_BACKGROUND_DISABLED = "pdt.console.colors.background.disabled";
	
	// Main
	public static final String PREF_CONTEXT_TRACKERS = "pdt.console.trackers";
	
	public static final String PREF_CONSOLE_HISTORY_SIZE = "pdt.console.history.size";

	public static final String PREFERNCE_PAGE_ID = "org.cs3.pdt.console.preferences.PreferencePageMain";

}


