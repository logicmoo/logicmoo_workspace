/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.prolog.connector.process;

public class PrologProcessStartException extends RuntimeException {

	private static final long serialVersionUID = -4834985720925340938L;
	
	public PrologProcessStartException() {
		super();
	}

	public PrologProcessStartException(String message, Throwable cause) {
		super(message, cause);
	}

	public PrologProcessStartException(String message) {
		super(message);
	}

	public PrologProcessStartException(Throwable cause) {
		super(cause);
	}

}
