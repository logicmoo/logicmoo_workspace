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
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

public abstract class LifeCycle {

	private class DispatcherThread extends Thread {
		private boolean shouldBeRunning = true;
		private BlockingQueue<WorkRunnable> queue = new LinkedBlockingQueue<WorkRunnable>();

		public DispatcherThread(String name) {
			super(name);
			setDaemon(true);
		}

		private synchronized void enqueue(WorkRunnable r) {
			String name = runnableName(r);
			Debug.debug("enqueue on " + getName() + ": " + name);
			queue.add(r);
		}

		private String runnableName(WorkRunnable r) {
			String name = r instanceof NamedWorkRunnable ? ((NamedWorkRunnable) r)
					.getName()
					: "unnamed work";
			return name;
		}

		
		@Override
		public void run() {
			while (shouldBeRunning) {
				try {
					WorkRunnable r = queue.take();
					String name = runnableName(r);
					Debug.debug("processing: " + name);
					r.run();
				} catch (Throwable t) {

					Debug.report(t);
					Debug.warning("clearing queue");
					queue.clear();
					error(t);
				}
			}
		}

		public void stopRunning() {
			queue.clear();
			queue.add(new NamedWorkRunnable("shouldBeRunning=false") {

				
				@Override
				public void run() throws PrologProcessException {
					shouldBeRunning = false;

				}

			});
			try {
				join();
			} catch (InterruptedException e) {

			}
		}
	}

	private State state = null;

	private HashMap<String, LifeCycleHookWrapper> hooks = new HashMap<String, LifeCycleHookWrapper>();

	private DispatcherThread workThread;
	private DispatcherThread transitionThread;

	public LifeCycle(String name) {

		setState(new DownState(this));
		workThread = new DispatcherThread("Prolog Process " + name + " Work");
		workThread.start();
		transitionThread = new DispatcherThread("Prolog Process " + name + " Transitions");
		transitionThread.start();
	}

	public synchronized void dispose() {
		workThread.stopRunning();
		transitionThread.stopRunning();
		hooks.clear();
		hooks = null;

		workThread = null;
		transitionThread = null;
	}

	public synchronized void waitUntilUp() throws InterruptedException,
			PrologProcessException {
		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"Invalid thread access. Cannot call 'waitUntilUp' from transition or work queue.");
		}

		while (getError() == null && !isUp()) {
			this.wait(1000);
			if (isDown()) {
				start();
			}
		}
		if (getError() != null) {
			throw new PrologProcessException(getError());
		}
	}

	public synchronized void waitUntilDown(boolean ignoreErrors) throws InterruptedException,
			PrologProcessException {

		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"Invalid thread access. Cannot call 'waitUntilDown' from transition or work queue.");
		}
		while ((ignoreErrors||getError() == null) && !isDown()) {
			this.wait(1000);
			if (isUp()) {
				stop();
			}

		}
		if (null != getError()) {
			throw new PrologProcessException(getError());
		}
	}

	

	public void waitUntilError() throws InterruptedException {
		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"Invalid thread access. Cannot call 'waitUntilError' from transition or work queue.");
		}
		getError();
		while ((getError()) == null) {
			this.wait(1000);
		}

	}

	public synchronized boolean isUp() {
		return state.isUp();
	}

	public synchronized boolean isDown() {
		return state.isDown();
	}

	public synchronized PrologProcessException getError() {
		return state.getError();
	}

	private synchronized void setState(State newState) {
		if (newState == state) {
			return;
		}

		String oldStateName = state == null ? "null" : state.getClass()
				.getSimpleName();
		state = newState;
		String newStateName = state == null ? "null" : state.getClass()
				.getSimpleName();
		Debug.debug(oldStateName + " ------> " + newStateName);
		state.enter();
		notifyAll();
	}

	public synchronized void start() {

		transitionThread.enqueue(new NamedWorkRunnable("start") {
			
			@Override
			public void run() throws PrologProcessException {

				setState(state.start());

			}
		});

	}

	public synchronized void stop() {
		transitionThread.enqueue(new NamedWorkRunnable("stop") {
			
			@Override
			public void run() throws PrologProcessException {
				setState(state.stop());
			}
		});
	}

	public synchronized void error(final Throwable e) {
		transitionThread.enqueue(new NamedWorkRunnable("error") {
			
			@Override
			public void run() throws PrologProcessException {
				setState(state.error(e));

			}
		});
	}

	protected synchronized void workDone() {
		transitionThread.enqueue(new NamedWorkRunnable("workDone") {
			
			@Override
			public void run() throws PrologProcessException {
				setState(state.workDone());

			}
		});
	}

	public synchronized void addLifeCycleHook(final LifeCycleHook hook,
			final String id, final String[] dependencies) {
		transitionThread.enqueue(new NamedWorkRunnable("addLifeCycleHook/3") {
			
			@Override
			public void run() throws PrologProcessException {
				setState(state.addLifeCycleHook(hook, id, dependencies));

			}
		});
	}

	public synchronized void removeLifeCycleHook(final String hookId) {
		transitionThread
				.enqueue(new NamedWorkRunnable("removeLifeCycleHook/1") {
					
					@Override
					public void run() throws PrologProcessException {
						setState(state.removeLifeCycleHook(hookId));

					}
				});
	}

	public synchronized void removeLifeCycleHook(final LifeCycleHook hook,
			final String hookId) {

		transitionThread
				.enqueue(new NamedWorkRunnable("removeLifeCycleHook/2") {
					
					@Override
					public void run() throws PrologProcessException {
						setState(state.removeLifeCycleHook(hook, hookId));

					}
				});
	}

	public synchronized void reset() {
		transitionThread.enqueue(new NamedWorkRunnable("reset") {
			
			@Override
			public void run() throws PrologProcessException {
				setState(state.reset());

			}
		});
	}

	public HashMap<String, LifeCycleHookWrapper> getHooks() {
		return hooks;
	}

	public void enqueueWork(WorkRunnable r) {
		workThread.enqueue(r);
	}

	public void clearWorkQueue() {
		workThread.queue.clear();

	}

	public abstract PrologProcess getPrologProcess();

	public abstract PrologSession getShutdownSession()
			throws PrologProcessException;

	public abstract PrologSession getInitialSession()
			throws PrologProcessException;

	public abstract void startServer() throws Throwable;

	public abstract void stopServer() throws Throwable;

	public abstract boolean isServerRunning() throws Throwable;

	public abstract void disposeSessions() throws Throwable;

}


