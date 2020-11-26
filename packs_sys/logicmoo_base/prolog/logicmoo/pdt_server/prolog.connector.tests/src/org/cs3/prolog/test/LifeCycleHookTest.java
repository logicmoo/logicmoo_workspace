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
import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

public class LifeCycleHookTest extends TestCase {


	private class MyHook implements LifeCycleHook{

//		private int lateInit;
//		private int onError;
//		private int setData;
		private int afterInit;
		private int beforeShutdown;
		private int onInit;

		@Override
		public void lateInit(PrologProcess process) {
			Debug.debug("lateInit");
//			lateInit++;
			
		}

		@Override
		public void onError(PrologProcess process) {
			Debug.debug("onError");
//			onError++;
			
		}

		@Override
		public void setData(Object data) {
			Debug.debug("setData");
//			setData++;
			
		}

		@Override
		public void afterInit(PrologProcess process)
				throws PrologProcessException {
			Debug.debug("afterInit");
			afterInit++;
			
		}

		@Override
		public void beforeShutdown(PrologProcess process, PrologSession session)
				throws PrologProcessException {
			Debug.debug("beforeShutdown");
			beforeShutdown++;
			
		}

		@Override
		public void onInit(PrologProcess process, PrologSession initSession)
				throws PrologProcessException {
			Debug.debug("onInit");
			onInit++;
			
		}
		
	}
	
	
	private PrologProcess process;

	@Override
	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
//		this.process=(PrologProcess) PrologProcessFactory.newInstance().create();
		this.process = Connector.newUninitializedPrologProcess();
		
	}

	public void testPDT_295_00() throws Exception{
		MyHook X = new MyHook();
		process.addLifeCycleHook(X, "X", new String[0]);
		process.getSession(PrologProcess.NONE).dispose();
		process.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
	
	public void testPDT_295_01() throws Exception{
		MyHook X = new MyHook();
		process.addLifeCycleHook(X, "X", new String[0]);
		process.removeLifeCycleHook(X,"X");
		process.addLifeCycleHook(X, "X", new String[0]);
		process.getSession(PrologProcess.NONE).dispose();
		process.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
}


