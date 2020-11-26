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

package org.cs3.pdt.common;

public final class PDTCommon {
	
	private PDTCommon() {}
	
	/**
	 * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
	 * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
	 * 
	 * The property will be read out once the Debug class is loaded, and the
	 * debug level will be set accordingly. After that, the level can be changed
	 * using the static Debug.setDeubgLevel(int) method.
	 */
	public final static String PREF_DEBUG_LEVEL = "debug.level";
	public final static String PREF_DEBUG_OUTPUT_TO = "debug.output.to";
	
	/**
	 * log file location used by the pdt plugin.
	 */
	public static final String PREF_CLIENT_LOG_FILE_DIR = "pdt.logfile";
	
	public static final String PREF_FILE_SIZE_LIMIT = "pdt.file.size.limit";
	
	public static final String PREF_RECONSULT_ON_RESTART = "pdt.reconsult.on.restart";
	public static final String RECONSULT_NONE = "pdt.reconsult.none";
	public static final String RECONSULT_ENTRY = "pdt.reconsult.entry.points";
	public static final String RECONSULT_ALL = "pdt.reconsult.all";

	public static final String PERSPECTIVE_CONSOLE_FOLDER = "prolog.perspective.console.folder";
	public static final String PERSPECTIVE_VIEWS_FOLDER = "prolog.perspective.views.folder";
	
	public static final String CONSULTED_FILES = "CONSULTED_FILES";
	
	public static final String PROCESS_SPECIFIC_RECONSULT_STRATEGY = "process.specific.reconsult.strategy";

}
