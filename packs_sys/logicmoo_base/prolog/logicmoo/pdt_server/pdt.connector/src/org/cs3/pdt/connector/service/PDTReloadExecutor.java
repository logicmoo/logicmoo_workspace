/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.service;

import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * This interface is used to define an executor of the pdt_reload/1 predicate
 * during the consulting process in an {@link IPrologProcessService}.
 */
public interface PDTReloadExecutor {

	/**
	 * @return The priority of this executor.
	 */
	int getPriority();

	/**
	 * @param process
	 *            the {@link PrologProcess} on which the pdt_reload/1
	 *            predicate is called to consult the list of files
	 * @param query
	 *            the reload query which should be executed
	 * @param monitor
	 *            a progress monitor
	 * @return True if consulting is successful, otherwise false
	 * @throws PrologProcessException
	 */
	boolean executePDTReload(PrologProcess process, String query, IProgressMonitor monitor) throws PrologProcessException;

}
