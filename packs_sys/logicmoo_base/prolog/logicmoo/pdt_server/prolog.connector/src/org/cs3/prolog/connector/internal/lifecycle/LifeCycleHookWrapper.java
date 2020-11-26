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

package org.cs3.prolog.connector.internal.lifecycle;

import java.util.HashSet;

import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcessException;

public class LifeCycleHookWrapper {

	String id;

	/** things I depend on */
	public HashSet<LifeCycleHookWrapper> post = new HashSet<LifeCycleHookWrapper>();

	/** things that depend on me */
	public HashSet<LifeCycleHookWrapper> pre = new HashSet<LifeCycleHookWrapper>();

	// public LifeCycleHook hook;
	public HashSet<LifeCycleHook> hooks = new HashSet<LifeCycleHook>();

	private final LifeCycle context;

	public LifeCycleHookWrapper(LifeCycle context,LifeCycleHook hook, String id) {
		this.context = context;
		if (hook != null) {
			this.hooks.add(hook);
		}
		this.id = id;
	}

	public LifeCycleHookWrapper(LifeCycleHookWrapper value) {
		this.context = value.context;
		this.hooks = new HashSet<LifeCycleHook>(value.hooks);
		this.id = value.id;
		this.post = new HashSet<LifeCycleHookWrapper>(value.post);
		this.pre = new HashSet<LifeCycleHookWrapper>(value.pre);
	}

	public void onInit(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : post) {
			elm.onInit( done);

		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("onInit_on_"+id) {
				
				@Override
				public void run() throws PrologProcessException {
					hook.onInit(context.getPrologProcess(), context.getInitialSession());
				}

			});
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : post) {
			elm.afterInit( done);
		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("afterInit_on_"+id) {
				
				@Override
				public void run() throws PrologProcessException {
					hook.afterInit(context.getPrologProcess());
				}

			});
		}

	}

	public void beforeShutdown(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : pre) {
			elm.beforeShutdown( done);
		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("beforeShutdown_on_"+id) {
				
				@Override
				public void run() throws PrologProcessException {
					hook.beforeShutdown(context.getPrologProcess(),context.getShutdownSession());
				}

			});
		}

	}

	public void onError() {
		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("onError_on_"+id) {
				@Override
				public void run() throws PrologProcessException {
					hook.onError(context.getPrologProcess());
				}
			});
		}
	}
}


