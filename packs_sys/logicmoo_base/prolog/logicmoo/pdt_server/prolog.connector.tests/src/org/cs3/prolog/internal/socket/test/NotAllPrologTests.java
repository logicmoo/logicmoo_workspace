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

package org.cs3.prolog.internal.socket.test;


import junit.framework.Test;
import junit.framework.TestSuite;

import org.cs3.prolog.test.AsyncSocketSessionTest;
import org.cs3.prolog.test.LazyStartupRaceTest;
import org.cs3.prolog.test.SocketSessionTest;
import org.cs3.prolog.test.SocketSessionThrowTest;
import org.cs3.prolog.test.XpceTest;

public class NotAllPrologTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.cs3.pl.prolog.internal");
		
		// $JUnit-BEGIN$
		suite.addTestSuite(SocketSessionTest.class);
		suite.addTestSuite(SocketSessionThrowTest.class);
		suite.addTestSuite(AsyncSocketSessionTest.class);
		suite.addTestSuite(RestartTest.class);
		//suite.addTestSuite(ConnectionToRunningPrologServerTest.class);
		suite.addTestSuite(LazyStartupRaceTest.class);
		suite.addTestSuite(XpceTest.class);
		// $JUnit-END$
		return suite;
	}

}


