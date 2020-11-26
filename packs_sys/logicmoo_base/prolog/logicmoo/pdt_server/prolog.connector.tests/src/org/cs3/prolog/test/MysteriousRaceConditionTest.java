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

package org.cs3.prolog.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

public class MysteriousRaceConditionTest extends TestCase {

	private PrologProcess process;

	@Override
	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		
		process = Connector.newUninitializedPrologProcess();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		process.stop();
	}

	public void testMyteriousRaceCondition() throws PrologProcessException {
		
		PrologSession session = process.getSession();
		
		String workspace="/home/lukas/workspace";
		String query = 
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/attic')" +
				"	->	true" +
				"	;	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/attic')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/attic')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/process')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/process')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/process')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/pdt')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/pdt')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/pdt')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.runtime/library/common')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.runtime/library/common')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.runtime/library/common')))," +
				"(	user:file_search_path(library, '"+workspace+"/pdt.core/engine')" +
				"->	true" +
				";	user:assert(file_search_path(library, '"+workspace+"/pdt.core/engine')" +
				"), " +
				"pdt_util:assert(pdt_hidden_path('"+workspace+"/pdt.core/engine')))";
		session.queryOnce(query);
		session.queryOnce("ensure_loaded(library('facade/pdt_facade'))");
		session.queryOnce("guitracer");
		session.queryOnce("tspy(parse:spyme)"); //this predicate exists.
	}

}


