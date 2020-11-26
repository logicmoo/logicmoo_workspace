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

package org.cs3.prolog.internal.socket.test;


import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.internal.process.socket.JackTheProcessRipper;
import org.cs3.prolog.connector.internal.session.socket.SocketSession;
import org.cs3.prolog.connector.process.PrologProcess;

public class RestartTest extends TestCase {
	public void testRecover() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		
		PrologProcess process = Connector.newUninitializedPrologProcess();
		
		process.start();

		
		SocketSession session = (SocketSession) process.getSession(PrologProcess.DEFAULT);
		long pid = session.getClient().getServerPid();
		JackTheProcessRipper.getInstance().markForDeletion(pid);
		try{
			process.stop();
		}
		catch(Throwable t){
			;
		}
		process.start();
		session = (SocketSession) process.getSession(PrologProcess.DEFAULT);
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
	}
	
	
	public void testRecover_lazy() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologProcess process = Connector.newUninitializedPrologProcess();
		
		SocketSession session = (SocketSession) process.getSession(PrologProcess.DEFAULT);
		long pid = session.getClient().getServerPid();
		JackTheProcessRipper.getInstance().markForDeletion(pid);
		try{
			process.stop();
		}
		catch(Throwable t){
			;
		}
		session = (SocketSession) process.getSession(PrologProcess.DEFAULT);
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
	}
}


