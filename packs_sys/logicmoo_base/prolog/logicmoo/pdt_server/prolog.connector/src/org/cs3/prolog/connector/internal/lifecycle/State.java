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

import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcessException;

public interface State{
	public void enter();
	public PrologProcessException getError();
	public boolean isUp();
	public boolean isDown();
	
	public State start();
	public State stop();
	public State error(Throwable e);
	public State workDone();
	public State addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies);
	public State removeLifeCycleHook(final String hookId);
	public State removeLifeCycleHook(final LifeCycleHook hook,
			final String hookId);
	public State reset();
	
}

