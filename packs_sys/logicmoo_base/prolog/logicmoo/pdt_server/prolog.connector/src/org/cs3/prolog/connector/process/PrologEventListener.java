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
package org.cs3.prolog.connector.process;

import java.util.EventListener;

/**
 * The listener interface for receiving notification of Prolog events.
 */
public interface PrologEventListener extends EventListener {

	/**
	 * Invoked when an event is triggered from Prolog.
	 * 
	 * @param e
	 *            the PrologEvent
	 */
    void update(PrologEvent e);

	
}


