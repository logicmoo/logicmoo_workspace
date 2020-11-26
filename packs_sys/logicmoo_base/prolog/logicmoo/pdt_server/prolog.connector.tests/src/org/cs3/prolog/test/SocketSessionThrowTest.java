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

package org.cs3.prolog.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

/**
 * @author terra
 */
public class SocketSessionThrowTest extends TestCase {
	private PrologProcess process;

    @Override
	protected void setUp() throws Exception {
      process = Connector.newUninitializedPrologProcess();
      process.start();
    }
    
    @Override
	protected void tearDown() throws Exception {
        process.stop();
    }
	
	/**
	 * http://roots.iai.uni-bonn.de/jira/browse/PDT-10
	 * @throws PrologException
	 * @throws PrologProcessException 
	 */
	public void testThrow() throws PrologException, PrologProcessException{
		PrologSession ss = process.getSession();
		try  {
		ss.queryOnce("throw(A)");
		} catch(Exception ex){
			System.out.println("");
		}
		ss.dispose();
	}}


