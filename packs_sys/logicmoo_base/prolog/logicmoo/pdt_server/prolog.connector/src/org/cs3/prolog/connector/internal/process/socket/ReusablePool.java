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

/*
 */
package org.cs3.prolog.connector.internal.process.socket;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cs3.prolog.connector.common.Debug;

/**
 * Creation of new client sessions seems to be rather costly due to the involved
 * rpc overhead. So we try to pool unused sessions for later use.
 */
public class ReusablePool {
	HashMap<Class<? extends Reusable>, LinkedList<Reusable>> pool = new HashMap<Class<? extends Reusable>, LinkedList<Reusable>>();

	int maxPoolSize = 5;

	int poolSize = 0;


	public void recycle(Reusable s) {
	    s.recylce();
		addInstance(s);
	}

	protected void addInstance(Reusable s) {
		synchronized (pool) {
            Debug.debug("poolSize="+poolSize+", maxPoolSize="+maxPoolSize);
			if (poolSize >= maxPoolSize) {
				Debug.debug("maximum pool size exeeded. instance destroyed.");                
				s.destroy();				
				return;
			}			
			Class<? extends Reusable> clazz = s.getClass();

			LinkedList<Reusable> l = pool.get(clazz);
			if (l == null) {
				l = new LinkedList<Reusable>();
				pool.put(clazz, l);
			}
			l.addLast(s);
			
			poolSize++;
			Debug.debug("new instance added to the pool. poolSize now: "+poolSize);
		}
	}

	public Reusable findInstance(Class<? extends Reusable> clazz) {
		Reusable instance = null;
		synchronized (pool) {
			LinkedList<Reusable> references = pool.get(clazz);
			if (noInstanceExists(references)) {
				Debug.debug("no reusable instance in pool");
				return null;
			}
			poolSize--;
			instance = references.removeFirst();

		}
		instance.reuse();
		Debug.debug("instance taken from pool and reanimated. poolSize now: "+poolSize);
		return instance;
	}

	
	private boolean noInstanceExists(LinkedList<Reusable> references) {
		return references == null || references.isEmpty();
	}

	public int getMaxTotalSize() {
		synchronized (pool) {
			return maxPoolSize;
		}
	}

	public void setMaxTotalSize(int size) {
		synchronized (pool) {
			this.maxPoolSize = size;
		}
	}
	// TRHO: fixed PDT-262
	public void clear(){
		synchronized (pool) {
		Collection<LinkedList<Reusable>> collection = pool.values();
			for (Iterator<LinkedList<Reusable>> outerIterator = collection.iterator(); outerIterator.hasNext();) {
				List<Reusable> list = outerIterator.next();
				for (Iterator<Reusable> reusableIterator = list.iterator(); reusableIterator.hasNext();) {
					Reusable s = reusableIterator.next();
					s.destroy();
				}
				list.clear();
			}
			pool.clear();
			poolSize=0;
		}
	}

}


