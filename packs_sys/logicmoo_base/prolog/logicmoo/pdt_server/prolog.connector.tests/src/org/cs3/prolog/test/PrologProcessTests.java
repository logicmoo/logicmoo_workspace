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

import junit.framework.Test;
import junit.framework.TestSuite;

public class PrologProcessTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.cs3.pl.prolog.tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(XpceTest.class);
		suite.addTestSuite(SocketSessionTest.class);
		suite.addTestSuite(AsyncSocketSessionTest.class);
		//suite.addTestSuite(MysteriousRaceConditionTest.class);
		suite.addTestSuite(SocketSessionThrowTest.class);
		suite.addTestSuite(LifeCycleHookTest.class);
		suite.addTestSuite(LazyStartupRaceTest.class);
		//$JUnit-END$
		return suite;
	}

}


