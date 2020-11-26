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
 * A prolog exception is thrown when a prolog query
 * executed through a PrologSession raised an exception in 
 * the prolog runtime.
 */
public class PrologException extends RuntimeException {

    private static final long serialVersionUID = 2832762018351845476L;

	/**
	 * Creates a PrologException with the specified detail message.
	 * 
	 * @param message
	 */
    public PrologException(String message) {
        super(message);
    }

	/**
	 * Creates a PrologException with the specified cause.
	 * 
	 * @param cause
	 */
    public PrologException(Throwable cause) {
        super(cause);
     }
    
	/**
	 * Creates a PrologException with the specified detail message and the
	 * specified cause.
	 * 
	 * @param message
	 * @param cause
	 */
    public PrologException(String message, Throwable cause) {
        super(message, cause);
     }

}


