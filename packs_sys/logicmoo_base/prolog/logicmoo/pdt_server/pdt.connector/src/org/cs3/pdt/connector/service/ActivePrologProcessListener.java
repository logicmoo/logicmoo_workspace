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

/**
 * This interface is used to listen to the active {@link PrologProcess}
 * defined by an {@link IPrologProcessService}.
 */
public interface ActivePrologProcessListener {

	/**
	 * The active {@link PrologProcess} has changed
	 * 
	 * @param process
	 *            the new {@link PrologProcess}
	 */
	void activePrologProcessChanged(PrologProcess process);

}
