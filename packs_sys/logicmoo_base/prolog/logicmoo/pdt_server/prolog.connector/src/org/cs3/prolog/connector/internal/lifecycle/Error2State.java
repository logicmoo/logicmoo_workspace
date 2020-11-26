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

package org.cs3.prolog.connector.internal.lifecycle;

import org.cs3.prolog.connector.process.PrologProcessException;

public class Error2State extends AbstractState{

	private PrologProcessException error;


	protected Error2State(LifeCycle context,PrologProcessException error) {
		super(context);
		this.error=error;
	}
	
	
	
	@Override
	public State error(Throwable e) {
		return this; // ignore further errors.
	}
	
	
	@Override
	public PrologProcessException getError() {	
		return error;
	}
	
	
	@Override
	public State reset() {
	
		return new DownState(context);
	}
}


