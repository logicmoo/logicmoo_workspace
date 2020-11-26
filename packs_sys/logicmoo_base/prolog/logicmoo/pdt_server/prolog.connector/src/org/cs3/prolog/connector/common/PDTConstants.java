/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Lukas Degener, Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.common;

/**
 * Contains constants for Prolog executable names. 
 *
 */
public class PDTConstants {
	
	// with graphic environment: First value is default!
	public static final String WINDOWS_EXECUTABLES_SWI = "swipl-win";           // plwin -> swipl-win since 5.9.9
	public static final String WINDOWS_EXECUTABLES_YAP = "yap";

	@Deprecated
	public static final String WINDOWS_EXECUTABLES = WINDOWS_EXECUTABLES_SWI;
	
	// without graphic environment: First value is default!
	public static final String WINDOWS_COMMAND_LINE_EXECUTABLES = "swipl";  // plcon -> swipl since 5.9.9

	// With or without graphic environment: First value is default!
	public static final String UNIX_COMMAND_LINE_EXECUTABLES_SWI = "swipl";      // xpce deleted since 5.9.9
	public static final String UNIX_COMMAND_LINE_EXECUTABLES_YAP = "yap";

	@Deprecated
	public static final String UNIX_COMMAND_LINE_EXECUTABLES = UNIX_COMMAND_LINE_EXECUTABLES_SWI;
	
	public static final String DIALECT_SWI = "dialect.swi";
	public static final String DIALECT_YAP = "dialect.yap";
	
}
