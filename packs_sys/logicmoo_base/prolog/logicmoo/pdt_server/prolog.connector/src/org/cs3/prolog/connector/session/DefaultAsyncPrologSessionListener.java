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

import org.cs3.prolog.connector.common.Debug;

/**
 * Default implementation of an asynchronous Prolog session listener.
 */
public class DefaultAsyncPrologSessionListener implements AsyncPrologSessionListener {

	@Override
	public void joinComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void abortComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalSucceeded(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalFailed(AsyncPrologSessionEvent e) {
		Debug.info("Goal failed: "+e.query );
	}

	@Override
	public void goalRaisedException(AsyncPrologSessionEvent e) {
		Debug.error("Goal raised exception: "+e.message+"\n query: "+e.query +"\n ticket: "+e.ticket);
	}

	@Override
	public void goalHasSolution(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalSkipped(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalCut(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void batchComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void batchError(AsyncPrologSessionEvent e) {
		Debug.error("Fatal error during batch processing (probably the connection to the server was lost).");
		
	}

}


