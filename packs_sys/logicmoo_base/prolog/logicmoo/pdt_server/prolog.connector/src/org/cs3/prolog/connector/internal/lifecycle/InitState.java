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

import java.util.HashMap;
import java.util.HashSet;

import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcessException;

public class InitState extends AbstractState {

	protected InitState(LifeCycle context) {
		super(context);
	}

	@Override
	public void enter() {
		HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		
		context.enqueueWork(new NamedWorkRunnable("startServer"){
			@Override
			public void run() throws PrologProcessException {
				try {
					context.startServer();
				} catch (Throwable e) {
					throw new PrologProcessException(e);
				}
			}	
		});

		for (LifeCycleHookWrapper h : hooks.values()) {
			h.onInit(done);
		}

		context.enqueueWork(new NamedWorkRunnable("workDoneInit") {	
			@Override
			public void run() throws PrologProcessException {
				context.workDone();
			}
		});
	}

	
	@Override
	public State workDone() {
		return new AfterInitState(context);
	}

	
	@Override
	public State addLifeCycleHook(final LifeCycleHook hook, String id,
			String[] dependencies) {
		if (isNewHook(hook,id)) {
			context.enqueueWork(new NamedWorkRunnable("lateInit on"+id) {
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
}


