/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: beckera (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.console.internal.views;

public class SingleCharModeException extends RuntimeException {

	private static final long serialVersionUID = 3903537127871615780L;

	public SingleCharModeException() {
	}

	public SingleCharModeException(String message) {
		super(message);
	}

	public SingleCharModeException(Throwable cause) {
		super(cause);
	}

	public SingleCharModeException(String message, Throwable cause) {
		super(message, cause);
	}

}
