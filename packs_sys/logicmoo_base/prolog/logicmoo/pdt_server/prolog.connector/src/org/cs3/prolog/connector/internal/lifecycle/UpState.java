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

import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcessException;

public class UpState extends AbstractState {

	protected UpState(LifeCycle context) {
		super(context);
	}
	
	@Override
	public boolean isUp() {
		return true;
	}
	
	@Override
	public State addLifeCycleHook(final LifeCycleHook hook, String id,
			String[] dependencies) {
		if (isNewHook(hook,id)) {
			context.enqueueWork(new NamedWorkRunnable("lateInit_on_"+id) {
				@Override
				public void run() throws PrologProcessException {
					hook.lateInit(context.getPrologProcess());
				}
			});
		}
		return super.addLifeCycleHook(hook, id, dependencies);
	}

	private boolean isNewHook(LifeCycleHook hook,String id) {
		LifeCycleHookWrapper w= context.getHooks().get(id);
		return w==null || ! w.hooks.contains(hook);		
	}
	
	
	@Override
	public State stop() {	
		return new ShutdownState(context);
	}

}


