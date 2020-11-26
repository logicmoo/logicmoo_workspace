/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.prolog.connector.internal.session.socket;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

/**
 * Iterator class for queries.
 * 
 * This iterator returns the results of a query individually. It is created by calling the PrologSession#queryIterator(String) method.
 */
public abstract class IterableQuery implements Iterator<Map<String, Object>> {
	
	private boolean closed = false;
	private boolean readMore = false;
	private Map<String, Object> nextResult;
	private SocketSession session;
	
	public IterableQuery(SocketSession session) {
		this.session = session;
	}
	
	@Override
	public boolean hasNext() {
		if (closed) {
			throw new RuntimeException("Iterator is closed.");
		}
		if (nextResult == null) {
			storeNextResult();
		}
		return nextResult != null;
	}

	@Override
	public Map<String, Object> next() {
		if (closed) {
			throw new RuntimeException("Iterator is closed.");
		}
		Map<String, Object> result = null;
		if (nextResult != null) {
			result = nextResult;
			nextResult = null;
		} else {
			try {
				result = read();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return result;
	}
	
	private void storeNextResult() {
		try {
			nextResult = read();
		} catch (IOException e) {
			e.printStackTrace();
			nextResult = null;
		}
	}
	
	private Map<String, Object> read() throws IOException {
		if (readMore) {
			return readMore();
		} else {
			readMore = true;
			return readFirstResult();
		}
	}
	
	@Override
	public String toString() {
		if (nextResult == null) {
			storeNextResult();
		}
		
		if (nextResult != null) {
			return nextResult.toString();
		} else {
			return "[]";
		}
	}
	
	/**
	 * Closes the Iterator. All the following operations are going to throw exceptions.
	 * @throws IOException 
	 */
	public void close() throws IOException {
		closed = true;
		session.closeIterator();
	}
	
	// just to make jenkins happy
	@Override
	public void remove() {
        throw new UnsupportedOperationException("remove");
    }

	protected abstract Map<String, Object> readFirstResult() throws IOException;
	protected abstract Map<String, Object> readMore() throws IOException;

}
