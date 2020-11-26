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

package org.cs3.pdt.connector;

public class PDTConnector {
	
	public static final String PLUGIN_ID = "org.cs3.pdt.connector";
	public static final String LIB_COMMON = "pdt.runtime.library.common";
	public static final String LIB_ATTIC = "pdt.runtime.library.attic";
	public static final String LIB_PDT = "pdt.runtime.library.pdt";
	public static final String LIB_TESTDATA = "pdt.runtime.library.testdata";
	
    public static final String PREF_CONFIGURATION = "process.configuration";
	public static final String CONFIGURATION_ATTRIBUTE = "preference.configuration";
	public static final String CONFIGURATION_SWI = "SWI Prolog";
	public static final String CONFIGURATION_SWI_LOGTALK = "SWI Prolog & Logtalk";
//	public static final String CONFIGURATION_YAP = "YAProlog";
//	public static final String CONFIGURATION_YAP_LOGTALK = "YAProlog & Logtalk";

	public static final String EP_BOOTSTRAP_CONTRIBUTION = "bootstrapContribution";
	public static final String EP_PROLOG_LIBRARY = "prologLibrary";
	public static final String EP_HOOKS = "hooks";
    
}


