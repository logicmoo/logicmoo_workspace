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

package org.cs3.pdt.connector;

/**
 * Service managing PrologContextTrackers.
 * 
 * There is no "current PrologProcess". Period.
 * 
 * Clients that are interested in finding and/or following the "current" PrologProcess should
 * check with this service to get a list of "opinions" on that matter. Each
 * PrologContextTracker represents such an opinion. Typically clients will
 * present the user with a choice of this opinions and let her decide.
 * 
 * 
 * Clients that want to contribute their own opinion on what is (and how to track) the 
 * current PrologProcess should subclass AbstractPrologContextTracker or otherwise implement
 * the interface PrologContextTracker and register it here. Another way to register a
 * tracker is adding an extension to the extension point prologContextTracker of the
 * pdt.runtime plugin. 
 * @author lukas
 * 
 */
public interface PrologContextTrackerService {
	/**
	 * register a context tracker with the runtime plugin. Any listeners that
	 * have already subscribed to the trackers id will be added.
	 * 
	 * @param tracker
	 */
	public void registerPrologContextTracker(PrologContextTracker tracker);

	/**
	 * unregister a context tracker from the runtime plugin. Listeners that were
	 * subscribed through the runtime plugin's global listener registry will be
	 * removed from the tracker.
	 * 
	 * @param tracker
	 */
	public void unregisterPrologContextTracker(PrologContextTracker tracker);

	/**
	 * registers a listener for a particular tracker at the earliest possible
	 * point in time. If the tracker is already registered, the listener is
	 * added at once. Otherwise it will be added when the tracker registeres.
	 * 
	 * @param trackerID
	 * @param l
	 */
	public void addPrologContextTrackerListener(String trackerID,
			PrologContextTrackerListener l);

	/**
	 * unregisters a listener from a particular tracker id. If the tracker is
	 * registered, the listener is removed. It is also removed from the plugins
	 * global listener table and will not be added to any other tracker
	 * registering with the same tracker id in the future.
	 * 
	 * In othe words, this undoes the effect of
	 * addPrologContextTrackerListener(String,PrologContextListener)
	 * 
	 * @param trackerID
	 * @param l
	 */
	public void removePrologContextTrackerListener(String trackerID,
			PrologContextTrackerListener l);

	/**
	 * @return an array containing all context trackers currently registered
	 *         with the runtime plugin.
	 */
	public PrologContextTracker[] getContextTrackers();

	/**
	 * retrieve the PrologContextTracker that was registered for the given ID.
	 * 
	 * @param trackerId
	 * @return the tracker with the given ID or null, if no tracker was
	 *         registered with this id.
	 */
	public PrologContextTracker getContextTracker(String trackerId);
}


