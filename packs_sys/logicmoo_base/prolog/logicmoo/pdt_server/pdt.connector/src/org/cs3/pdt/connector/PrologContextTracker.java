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

import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.ui.IWorkbench;

/**
 * keeps track of "the current" PrologProcess.
 * 
 * As there might be several (conflicting) notions or strategies of determining
 * the "active" or "current" PrologProcess at a given point in time, I decided
 * to attach a name to each of this strategies and let the user decide which one
 * to use.
 * 
 * Clients who are interested in finding out or following the currently "active"
 * PrologProcess should check with the ContextTrackerService to obtain a list
 * of all contributed strategies and either choose one that seems fitting or
 * even beter: ask the user.
 * 
 * Clients can overide this Interface to contribute their own
 * PrologContextTracker strategy and make it available to other clients by
 * registering it at the PDTConnector's ContextTrackerService. Typically
 * this is done by extending the extension point prologContextTracker of the
 * pdt.runtime plugin. 
 * 
 * @author lukas
 * 
 */
public interface PrologContextTracker {
	/**
	 * add a listener.
	 * 
	 * Note: In most cases, listeners should be registered with the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * The listener will be informed whenever the tracker thinks that the active
	 * PrologProcess has changed, becomes available or invalid.
	 * 	 * 
	 * Implemntations are responsible for calling the listener methods.
	 * 
	 * 
	 * @param l
	 */
	public void addPrologContextTrackerListener(PrologContextTrackerListener l);

	/**
	 * remove a listener.
	 * 
	 * Note: In most cases, listeners should be unregistered from the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * @param l
	 */
	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l);

	/**
	 * @return a descriptive label for this tracker. This will be presented to
	 *         the user.
	 */
	public String getLabel();

	/**
	 * @return a unique identifier for this tracker.
	 */
	public String getId();

	/**
	 * There is no current PrologProcess.
	 * find out what this tracker THINKS is the currently active PrologProcess.
	 * 
	 * @return the PrologProcess or null, if the tracker thinks that none is
	 *         active.
	 * @throws PrologProcessException 
	 * @throws PrologProcessException 
	 */
	public PrologProcess getCurrentPrologProcess() ;

	/**
	 * initialize the tracker.
	 * 
	 * This method is called by the pdt.runtime plugin at the earliest possible
	 * time that satisfies the following conditions: - the tracker has been
	 * registered with the plugin's tracker service - the eclipse workbench ui
	 * is up and running.
	 * 
	 * Implementation should seize the oportunity to attach to parts of the
	 * workbench ui they might be interested in.
	 * 
	 * @param workbench
	 */
	public void init(IWorkbench workbench);
}


