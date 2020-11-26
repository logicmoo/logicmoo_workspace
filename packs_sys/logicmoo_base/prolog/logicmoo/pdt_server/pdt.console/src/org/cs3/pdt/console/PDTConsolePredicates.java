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

package org.cs3.pdt.console;

public class PDTConsolePredicates {
	
	private PDTConsolePredicates(){}
	
	// console server
	public static final String PDT_CURRENT_CONSOLE_SERVER = "pdt_console_server:pdt_current_console_server";
	public static final String PDT_START_CONSOLE_SERVER = "pdt_console_server:pdt_start_console_server";
	public static final String PDT_STOP_CONSOLE_SERVER = "pdt_console_server:pdt_stop_console_server";
	public static final String CONSOLE_THREAD_NAME = "pdt_console_server:console_thread_name";

}
