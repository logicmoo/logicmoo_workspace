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

/**
 * A listener interface for asynchronous Prolog sessions.
 * 
 * @see AsyncPrologSessionEvent
 * @see AsyncPrologSession
 */
public interface AsyncPrologSessionListener {

	void joinComplete(AsyncPrologSessionEvent e);

	void abortComplete(AsyncPrologSessionEvent e);

	void goalSucceeded(AsyncPrologSessionEvent e);

	void goalFailed(AsyncPrologSessionEvent e);

	void goalRaisedException(AsyncPrologSessionEvent e);

	void goalHasSolution(AsyncPrologSessionEvent e);

	void goalSkipped(AsyncPrologSessionEvent e);

	void goalCut(AsyncPrologSessionEvent e);

	void batchComplete(AsyncPrologSessionEvent e);
	
	void batchError(AsyncPrologSessionEvent e);

}


