/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector;

public class PrologConnectorPredicates {
	
	private PrologConnectorPredicates() {}
	
	// pdt reload
	public static final String PDT_RELOAD = "pdt_reload:pdt_reload";
	public static final String ERRORS_AND_WARNINGS = "pdt_reload:errors_and_warnings";
	public static final String RELOADED_FILE = "pdt_reload:reloaded_file";
	public static final String WAIT_FOR_RELOAD_FINISHED = "pdt_reload:wait_for_reload_finished";

}
