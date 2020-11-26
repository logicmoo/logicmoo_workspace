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

package org.cs3.pdt.connector.registry;


import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.prolog.connector.process.PrologProcess;

abstract public class DefaultPrologProcessRegistry implements PrologProcessRegistry {

	private HashMap<String, PrologProcess> processes = new HashMap<String, PrologProcess>();
	private HashMap<String, Subscription> subscriptions = new HashMap<String, Subscription>();
	private HashMap<String, HashSet<Subscription>> subscriptionLists = new HashMap<String, HashSet<Subscription>>();
	private Vector<PrologProcessRegistryListener> listeners = new Vector<PrologProcessRegistryListener>();
	private HashMap<PrologProcess, String> processKeys = new HashMap<PrologProcess, String>();

	@Override
	public void addPrologProcessRegistryListener(
			PrologProcessRegistryListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}
	}

	@Override
	public void removePrologProcessRegistryListener(
			PrologProcessRegistryListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	@SuppressWarnings("unchecked")
	public void firePrologProcessAdded(String key) {
		PrologProcessRegistryEvent e = new PrologProcessRegistryEvent(this,
				key);
		Vector<PrologProcessRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologProcessRegistryListener>) listeners.clone();
		}
		for (PrologProcessRegistryListener l : clone) {				
			l.processAdded(e);
		}
	}


	@SuppressWarnings("unchecked")
	public void firePrologProcessRemoved(String key) {
		PrologProcessRegistryEvent e = new PrologProcessRegistryEvent(this,
				key);
		Vector<PrologProcessRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologProcessRegistryListener>) listeners.clone();
		}
		for (PrologProcessRegistryListener l : clone) {
			l.processRemoved(e);
		}
	}


	@SuppressWarnings("unchecked")
	public void fireSubscriptionAdded(Subscription s) {
		PrologProcessRegistryEvent e = new PrologProcessRegistryEvent(this,
				s);
		Vector<PrologProcessRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologProcessRegistryListener>) listeners.clone();
		}
		for (PrologProcessRegistryListener l : clone) {
			l.subscriptionAdded(e);
		}
	}


	@SuppressWarnings("unchecked")
	public void fireSubscriptionRemoved(Subscription s) {
		PrologProcessRegistryEvent e = new PrologProcessRegistryEvent(this,
				s);
		Vector<PrologProcessRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologProcessRegistryListener>) listeners.clone();
		}
		for (PrologProcessRegistryListener l : clone) {
			l.subscriptionRemoved(e);
		}
	}

	@Override
	public Set<String> getRegisteredKeys() {

		return processes.keySet();
	}

	@Override
	public String getKey(PrologProcess prologProcess) {
		return processKeys.get(prologProcess);
	}

	@Override
	public PrologProcess getPrologProcess(String key) {
		return processes.get(key);
	}

	@Override
	public Subscription getSubscription(String key) {
		return subscriptions.get(key);

	}

	@Override
	public Set<String> getAllKeys() {
		Set<String> s = new HashSet<String>(getRegisteredKeys());
		s.addAll(subscriptionLists.keySet());
		return s;
	}

	@Override
	public Set<String> getAllSubscriptionIDs() {
		return new HashSet<String>(subscriptions.keySet());
	}

	public Set<Subscription> getAllSubscriptions() {
		return new HashSet<Subscription>(subscriptions.values());
	}
	
	@Override
	public Set<Subscription> getSubscriptionsForProcess(String key) {
		Collection<Subscription> coll = subscriptionLists.get(key);
		HashSet<Subscription> subscripitions = new HashSet<Subscription>();
		if(coll != null){
			subscripitions.addAll(coll);
		}
		return subscripitions;
	}

	@Override
	public void addPrologProcess(String key, PrologProcess process) {
		Object old = processes.get(key);
		if (old == process) {
			return;
		}
		if (old != null) {
			removePrologProcess(key);
		}
		processes.put(key, process);
		processKeys.put(process, key);
		Set<Subscription> l = getSubscriptionsForProcess(key);
		for (Subscription s: l) {
			s.configure(process);
		}
		firePrologProcessAdded(key);
	}


	@SuppressWarnings("unchecked")
	@Override
	public void removePrologProcess(String key) {
		PrologProcess process = processes.get(key);
		if (process == null) {
			return;
		}
		HashSet<Subscription> keySet =  subscriptionLists.get(key);
		if (keySet != null) {
			keySet =  (HashSet<Subscription>) keySet.clone();
			for (Subscription s : keySet) {
				s.deconfigure(process);
			}
		}
		firePrologProcessRemoved(key);
		processKeys.remove(process);
		processes.remove(key);

	}

	@Override
	public void addSubscription(Subscription s) {
		// do not add anonymous subscriptions
		String sid = s.getId();
		if (sid == null) {
			return;
		}

		Object old = subscriptions.get(sid);
		if (old == s) {
			return;
		}
		if (old != null) {
			removeSubscription(sid);
		}
		HashSet<Subscription> l = subscriptionLists.get(s.getProcessKey());
		if (l == null) {
			l = new HashSet<Subscription>();
			subscriptionLists.put(s.getProcessKey(), l);
		}
		l.add(s);
		subscriptions.put(sid, s);

		if (this.processes.containsKey(s.getProcessKey())) {
			s.configure(getPrologProcess(s.getProcessKey()));
		}
		fireSubscriptionAdded(s);
	}

	@Override
	public void removeSubscription(String id) {
		removeSubscription(getSubscription(id));
	}

	@Override
	public void removeSubscription(Subscription subscription) {
		// do not remove anonymous subscriptions
		if (subscription == null) {
			return;
		}
		String id = subscription.getId();
		if (id == null) {
			return;
		}
		if (!subscriptions.containsKey(id)) {
			return;
		}
		String processKey = subscription.getProcessKey();
		if (processes.containsKey(processKey)) {
			subscription.deconfigure(getPrologProcess(processKey));
			Set<Subscription> otherSubscriptions = getSubscriptionsForProcess(processKey);
			otherSubscriptions.remove(subscription);
		}
		subscriptions.remove(id);

		Set<Subscription> keySet = subscriptionLists.get(processKey);
		if (keySet == null) {
			return;
		}
		if (keySet.contains(subscription)) {
			keySet.remove(subscription);
			fireSubscriptionRemoved(subscription);

		}

	}
}


