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

package org.cs3.pdt.console.internal;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsoleEvent;
import org.cs3.pdt.console.PrologConsoleListener;
import org.cs3.pdt.console.PrologConsoleService;

public class DefaultPrologConsoleService implements PrologConsoleService, PrologConsoleListener {

	private Vector<PrologConsoleListener> listeners = new Vector<PrologConsoleListener>();
	private HashSet<PrologConsole> visibleConsoles=new HashSet<PrologConsole>();
	private Vector<PrologConsole> consoles=new Vector<PrologConsole>();
	private PrologConsole activeConsole;
	
	public DefaultPrologConsoleService() {
		addPrologConsoleListener(this);
	}
	
	@Override
	public void registerPrologConsole(PrologConsole console) {
		
		synchronized (consoles) {
			if(!consoles.contains(console)){
				consoles.add(console);
			}			
		}
	}

	@Override
	public void unregisterPrologConsole(PrologConsole console) {
		synchronized (consoles) {
			if(consoles.contains(console)){
				consoles.remove(console);
				visibleConsoles.remove(console);
//				removePrologConsoleListener(this);
				if(console==activeConsole){
					activeConsole=null;
				}
			}			
		}		
	}

	@Override
	public PrologConsole[] getRegisteredPrologConsoles() {		
		return consoles.toArray(new PrologConsole[consoles.size()]);
	}

	@Override
	public PrologConsole getActivePrologConsole() {
		if(activeConsole!=null){
			return activeConsole;
		}
		if(visibleConsoles.size()==1){
			return visibleConsoles.iterator().next();
		}
		return null;
	}

	
	@Override
	public void consoleRecievedFocus(PrologConsoleEvent e) {
		activeConsole=(PrologConsole) e.getSource();
		
	}

	@Override
	public void consoleLostFocus(PrologConsoleEvent e) {
		activeConsole=null;		
	}


	@Override
	public void consoleVisibilityChanged(PrologConsoleEvent e) {
		PrologConsole c = (PrologConsole) e.getSource();
		if(c.isVisible()){
			visibleConsoles.add(c);
		}
		else{
			visibleConsoles.remove(c);
		}
		
	}

	
	@Override
	public void activePrologProcessChanged(PrologConsoleEvent e) {
		
	}

	@Override
	public void addPrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	@Override
	public void removePrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	public void fireConsoleRecievedFocus(PrologConsole console) {
		Vector<PrologConsoleListener> clone = getAListenersClone();
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleRecievedFocus(e);
		}
	}	

	public void fireActivePrologProcessChanged(PrologConsole console) {
		Vector<PrologConsoleListener> clone = getAListenersClone();
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.activePrologProcessChanged(e);
		}
	}

	public void fireConsoleLostFocus(PrologConsole console) {
		Vector<PrologConsoleListener> clone = getAListenersClone();
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleLostFocus(e);
		}
	}

	public void fireConsoleVisibilityChanged(PrologConsole console) {
		Vector<PrologConsoleListener> clone = getAListenersClone();
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleVisibilityChanged(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Vector<PrologConsoleListener> getAListenersClone() {
		Vector<PrologConsoleListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologConsoleListener>) listeners.clone();
		}
		return clone;
	}

	
}


