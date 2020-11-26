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

import java.util.EventObject;

/**
 * A Prolog event consisting of a subject and subject-specific data.
 */
public class PrologEvent extends EventObject {

	private static final long serialVersionUID = 1L;
    private String subject;
    private String data;

	/**
	 * Constructs a Prolog event.
	 * 
	 * @param source
	 *            the {@link PrologEventDispatcher}
	 * @param subject
	 *            the subject for which a {@link PrologEventListener} can be
	 *            registered
	 * @param data
	 *            subject-specific data of the event
	 */
    public PrologEvent(Object source, String subject, String data) {
        super(source);
        this.subject=subject;
        this.data=data;
    }
    
    /**
     * @return subject-specific data of the event
     */
    public String getData() {
        return data;
    }

	/**
	 * Returns the subject for which a {@link PrologEventListener} can be
	 * registered.
	 * 
	 * @return the subject
	 */
    public String getSubject() {
        return subject;
    }
    
    
}


