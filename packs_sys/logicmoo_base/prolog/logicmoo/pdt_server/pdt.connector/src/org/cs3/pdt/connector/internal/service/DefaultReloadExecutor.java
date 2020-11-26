/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.internal.service;

import org.cs3.pdt.connector.service.PDTReloadExecutor;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.runtime.IProgressMonitor;

public class DefaultReloadExecutor implements PDTReloadExecutor {
	
	@Override
	public int getPriority() {
		return 0;
	}

	@Override
	public boolean executePDTReload(PrologProcess process, String query, IProgressMonitor monitor) throws PrologProcessException {
		monitor.beginTask("", 1);
		try {
			process.queryOnce(query);
		} finally {
			monitor.done();
		}
		return true;
	}
	
}


