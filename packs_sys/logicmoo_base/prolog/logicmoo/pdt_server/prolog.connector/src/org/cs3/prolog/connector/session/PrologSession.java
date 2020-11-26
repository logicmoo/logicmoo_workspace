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

package org.cs3.prolog.connector.session;

import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.internal.session.socket.IterableQuery;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;

/**
 * Prolog Session for synchronous queries.
 * 
 * A single Prolog engine thread is attached to this session through its
 * complete lifetime.
 * 
 */
public interface PrologSession extends Disposable {
	
	/**
	 * @param query
	 * @return the first result of the query or null if the query fails
	 * @throws PrologException
	 * @throws PrologProcessException
	 * @see PrologProcess#queryOnce(String...)
	 */
    public Map<String,Object> queryOnce(String query) throws PrologException, PrologProcessException;

	/**
	 * @param query
	 * @return all results of the query or an empty list if the query fails
	 * @throws PrologException
	 * @throws PrologProcessException
	 * @see PrologProcess#queryAll(String...)
	 */
    public List<Map<String,Object>> queryAll(String query) throws PrologException, PrologProcessException;

    
    /**
     * Returns an iterator which iterates over the results of the query. Each result is only computed when requested.
     * In most cases it's better to use queryAll for better performance. You should only use queryIterator if you can't
     * use queryAll (e.g. if the query has an infinite number of results).
     * @param query 
     * @return the iterator
     * @throws PrologException 
     * @throws PrologProcessException 
     */
    public IterableQuery queryIterator(String query) throws PrologException, PrologProcessException;

}


