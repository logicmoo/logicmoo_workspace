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

import java.util.Iterator;
import java.util.Vector;

import org.cs3.prolog.connector.process.PrologProcessException;

/**
 * Convenience class for implementing PrologContextTracker.
 * 
 * Subclasses need to provide implementations for init() and
 * getCurrentPrologProcess(). In addition, subclasses are responsible for
 * calling fireContextChanged() when apropiate, i.e. when they think that the
 * "current" PrologProcess has changed, become available or invalid.
 * 
 * Clients that want to register a static PrologContextTracer instance via the
 * extension point prologContextTracker *MUST* subclass this class.
 * 
 * Other clients are free to use this class as a starting point.
 * 
 * @author lukas
 * 
 */
public abstract class AbstractPrologContextTracker implements
		PrologContextTracker {

	private String id;

	private String label;

	private Vector<PrologContextTrackerListener> listeners = new Vector<PrologContextTrackerListener>();

	/**
	 * Notify listeners that the "current" PrologProcess has changed.
	 * 
	 * Subclasses should call this method whenever they think the "current"
	 * PrologProcess has changed, become available or invalid.
	 * @throws PrologProcessException 
	 */
	protected void fireContextChanged()  {

		PrologContextTrackerEvent e=null;
		e = new PrologContextTrackerEvent(this,
				getCurrentPrologProcess());
		Vector<PrologContextTrackerListener> cloned = getAListenersClone();
		for (Iterator<PrologContextTrackerListener> it = cloned.iterator(); it.hasNext();) {
			PrologContextTrackerListener l = it.next();
			l.contextChanged(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Vector<PrologContextTrackerListener> getAListenersClone() {
		Vector<PrologContextTrackerListener> cloned = null;
		synchronized (listeners) {
			cloned = (Vector<PrologContextTrackerListener>) listeners.clone();
		}
		return cloned;
	}

	@Override
	public void addPrologContextTrackerListener(PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	@Override
	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}

	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public String getId() {
		return id;
	}

	public AbstractPrologContextTracker() {
		id = null;
		label = null;
	}

	public AbstractPrologContextTracker(String id, String label) {
		this.id = id;
		this.label = label;
	}

	public void setLabel(String label) {
		this.label = label;

	}

	public void setId(String id) {
		this.id = id;
	}

}


