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
import java.util.Iterator;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcessException;

public abstract class AbstractState implements State {

	protected final LifeCycle context;

	
	@Override
	public boolean isUp() {	
		return false;
	}
	
	@Override
	public boolean isDown() {	
		return false;
	}
	
	
	@Override
	public PrologProcessException getError() {	
		return null;		
	}
	
	protected AbstractState(LifeCycle context) {
		this.context = context;

	}

	
	@Override
	public State reset() {
	
		return this;
	}
	
	@Override
	public void enter() {
	
		
	}
	
	@Override
	public State addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies) {

		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		if (id == null) {
			id = "<<" + hooks.size() + ">>";
		}
		if (dependencies == null) {
			dependencies = new String[0];
		}
		Debug.debug("requested to add hook: id=\"" + id + "\", dependencies=\""
				+ Util.prettyPrint(dependencies) + "\"");

		LifeCycleHookWrapper node = hooks.get(id);

		if (node == null) {
			Debug.debug("\t-> hook unknown, new wrapper created.");
			node = new LifeCycleHookWrapper(context,hook, id);

			hooks.put(id, node);

		} else {
			Debug
					.debug("\t-> hook exists, reusing wrapper, but adding hook code..");
			node.hooks.add(hook);

		}
		for (int i = 0; i < dependencies.length; i++) {
			LifeCycleHookWrapper dep = hooks
					.get(dependencies[i]);
			Debug.debug("\t-> looking up dependency \"" + dependencies[i]
					+ "\"");
			if (dep == null) {
				Debug.debug("\t\t-> hook unknown, new wrapper created.");
				dep = new LifeCycleHookWrapper(context,null, dependencies[i]);

				hooks.put(dependencies[i], dep);
			}
			dep.pre.add(node);
			node.post.add(dep);
			Debug.debug("\t-> edges added.");
		}

		return this;
	}

	
	@Override
	public State error(Throwable e) {
		if(e instanceof PrologProcessException){
			return new ErrorState(context,(PrologProcessException) e);
		}
		return new ErrorState(context, new PrologProcessException(e));
	}

	
	@Override
	public State workDone() {
		return this;
	}

	
	@Override
	public State removeLifeCycleHook(String hookId) {
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		LifeCycleHookWrapper wrapper = hooks.get(hookId);
		if (wrapper != null) {
			removeWrapper(hooks, wrapper);
			wrapper.hooks.clear();// extra paranoia :-)
		}
		return this;
	}

	
	@Override
	public State removeLifeCycleHook(LifeCycleHook hook, String hookId) {
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		LifeCycleHookWrapper wrapper = hooks.get(hookId);
		if (wrapper != null) {
			wrapper.hooks.remove(hook);
			if (wrapper.hooks.isEmpty()) {
				removeWrapper(hooks, wrapper);
			}
		}
		return this;
	}

	private void removeWrapper(HashMap<String, LifeCycleHookWrapper> hooks,
			LifeCycleHookWrapper wrapper) {
		hooks.remove(wrapper);
		for (Iterator<LifeCycleHookWrapper> it = wrapper.pre.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm = it.next();
			elm.post.remove(wrapper);
		}
		for (Iterator<LifeCycleHookWrapper> it = wrapper.post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm = it.next();
			elm.pre.remove(wrapper);
		}
	}

	
	@Override
	public State start() {
		return this;
	}

	
	@Override
	public State stop() {
		return this;
	}

}


