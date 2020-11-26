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

package org.cs3.pdt.connector.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.connector.PrologContextTracker;
import org.cs3.pdt.connector.PrologContextTrackerListener;
import org.cs3.pdt.connector.PrologContextTrackerService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

public class DefaultPrologContextTrackerService implements
		PrologContextTrackerService {
	
	
	private HashMap<String, PrologContextTracker> contextTrackers=new HashMap<String, PrologContextTracker>();

	private HashMap<String, Set<PrologContextTrackerListener>> contextTrackerListeners=new HashMap<String, Set<PrologContextTrackerListener>>();
	

	/**
	 * register a context tracker with the runtime plugin.
	 * Any listeners that have already subscribed to the trackers id will be added.
	 * @param tracker
	 */
	@Override
	public void registerPrologContextTracker(PrologContextTracker tracker) {
		contextTrackers.put(tracker.getId(),tracker);
		Set<PrologContextTrackerListener> listeners = contextTrackerListeners.get(tracker.getId());
		if(listeners==null){
			listeners=new HashSet<PrologContextTrackerListener>();
			contextTrackerListeners.put(tracker.getId(),listeners);
		}
		for (Iterator<PrologContextTrackerListener> it = listeners.iterator(); it.hasNext();) {
			PrologContextTrackerListener nextListener = it.next();
			tracker.addPrologContextTrackerListener(nextListener);
		}
		lateInit(tracker);
	}

	private void lateInit(final PrologContextTracker tracker) {
		if(PlatformUI.isWorkbenchRunning()){
			final IWorkbench workbench = PlatformUI.getWorkbench();
			workbench.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					if(PlatformUI.getWorkbench().getActiveWorkbenchWindow()!=null){
						tracker.init(workbench);	
					}
				}
			});
			
		}
	}

	/**
	 * unregister a context tracker from the runtime plugin.
	 * Listeners that were subscribed through the runtime plugin's global listener registry
	 * will be removed from the tracker.
	 * @param tracker
	 */
	@Override
	public void unregisterPrologContextTracker(PrologContextTracker tracker) {
		contextTrackers.remove(tracker.getId());
		Set<PrologContextTrackerListener> listeners = contextTrackerListeners.remove(tracker.getId());
		if(listeners==null){
			return;
		}
		
		for (Iterator<PrologContextTrackerListener> it = listeners.iterator(); it.hasNext();) {
			PrologContextTrackerListener nextListener = it.next();
			tracker.removePrologContextTrackerListener(nextListener);
		}
	}
	
	/**
	 * registers a listener for a particular tracker at the earliest possible point in time.
	 * If the tracker is already registered, the listener is added at once. Otherwise it will be 
	 * added when the tracker registeres.
	 * @param trackerID
	 * @param l
	 */
	@Override
	public void addPrologContextTrackerListener(String trackerID, PrologContextTrackerListener l){
		Set<PrologContextTrackerListener> s = contextTrackerListeners.get(trackerID);
		if(s==null){
			s=new HashSet<PrologContextTrackerListener>();
			contextTrackerListeners.put(trackerID,s);
		}
		s.add(l);
		PrologContextTracker tracker = contextTrackers.get(trackerID);
		if(tracker!=null){
			tracker.addPrologContextTrackerListener(l);
		}
	}
	
	/**
	 * unregisters a listener from a particular tracker id.
	 * If the tracker is registered, the listener is removed. It is also removed from the plugins
	 * global listener table and will not be added to any other tracker registering with the same 
	 * tracker id in the future.
	 * 
	 * In othe words, this undoes the effect of addPrologContextTrackerListener(String,PrologContextListener)
	 * @param trackerID
	 * @param l
	 */
	@Override
	public void removePrologContextTrackerListener(String trackerID, PrologContextTrackerListener l){
		Set<PrologContextTrackerListener> listeners = contextTrackerListeners.get(trackerID);
		if(listeners==null){
			return;
		}
		listeners.remove(l);
		PrologContextTracker tracker = contextTrackers.get(trackerID);
		if(tracker!=null){
			tracker.removePrologContextTrackerListener(l);
		}
	}
	
	
	/**
	 * @return an array containing all context trackers currently registered with the runtime plugin.
	 */
	@Override
	public PrologContextTracker[] getContextTrackers() {		
		return contextTrackers.values().toArray(new PrologContextTracker[contextTrackers.size()]);
	}

	@Override
	public PrologContextTracker getContextTracker(String trackerId) {
		return contextTrackers.get(trackerId);
	}

}


