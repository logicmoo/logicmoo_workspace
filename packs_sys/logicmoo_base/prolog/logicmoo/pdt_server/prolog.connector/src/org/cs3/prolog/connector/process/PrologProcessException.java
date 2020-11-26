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


/**
 * A PrologProcessException is thrown when a PrologProcess looses
 * the connection to the prolog process due to an error.
 */
public class PrologProcessException extends Exception {

	private static final long serialVersionUID = 1298404946804072338L;

	/**
	 * Creates a PrologProcessException.
	 */
    public PrologProcessException() {
        super();
    }
    
    /**
	 * Creates a PrologProcessException with the specified detail message.
	 * 
	 * @param message
	 */
    public PrologProcessException(String message) {
        super(message);
    }
    
	/**
	 * Creates a PrologProcessException with the specified cause.
	 * 
	 * @param cause
	 */
    public PrologProcessException(Throwable cause) {
        super(cause);
    }
    
	/**
	 * Creates a PrologProcessException with the specified detail message and
	 * the specified cause.
	 * 
	 * @param message
	 * @param cause
	 */
    public PrologProcessException(String message, Throwable cause) {
        super(message, cause);
     }

}


