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

package org.cs3.prolog.connector.internal.lifecycle;

import java.util.HashSet;

import org.cs3.prolog.connector.process.PrologProcessException;

public class ShutdownState extends AbstractState {

	public ShutdownState(LifeCycle context) {
		super(context);
	
	}

	
	
	@Override
	public void enter() {
		
		HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
		context.getPrologProcess();
		context.clearWorkQueue(); //there may be afterINit hooks left. dump them.
		
		for (LifeCycleHookWrapper w : context.getHooks().values()) {			
			w.beforeShutdown(done);
		}
		
		context.enqueueWork(new NamedWorkRunnable("shutdown_server") {
			
			@Override
			public void run() throws PrologProcessException {
				try {
					context.disposeSessions();
					context.stopServer();
					context.workDone();
				} catch (Throwable e) {
					throw new PrologProcessException(e);					
				}
				
			}
		});
	}

	

	
	@Override
	public State workDone() {	
		return new DownState(context); //reset when hooks are through.
	}

	

}


