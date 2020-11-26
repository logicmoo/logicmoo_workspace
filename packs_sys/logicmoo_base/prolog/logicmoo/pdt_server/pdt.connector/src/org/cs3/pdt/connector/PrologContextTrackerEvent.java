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

import java.util.EventObject;

import org.cs3.prolog.connector.process.PrologProcess;

public class PrologContextTrackerEvent extends EventObject {


	private static final long serialVersionUID = 1L;
	private PrologProcess process;

	/**
	 * creates a new PrologContextTrackerEvent.
	 * 
	 * 
	 * @param source
	 *            this should be the tracker that caused the event.
	 * @param process
	 *            this should be what the process thinks is the currently active
	 *            PrologProcess _AFTER_ the change. Maybe null to indicate
	 *            that no process is currently active according to the source
	 *            tracker.
	 */
	public PrologContextTrackerEvent(Object source, PrologProcess process) {
		super(source);
		this.process = process;
	}

	/**
	 * @return the currently active PrologProcess (according to the tracker
	 *         that send this event), or null if no PrologProcess is active
	 *         (dito).
	 */
	public PrologProcess getPrologProcess() {
		return process;
	}
}


