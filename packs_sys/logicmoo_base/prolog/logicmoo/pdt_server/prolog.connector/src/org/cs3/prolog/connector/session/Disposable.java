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

package org.cs3.prolog.connector.session;

import java.io.Closeable;

/**
 * A simple Interface to support disposing of a session and checking if a
 * session has been disposed.
 *
 */
public interface Disposable extends Closeable {

    /**
     * Disposes the session. Any further call (except of further dispose calls
     * will cause the system to throw an IllegalStateException.
     */
    public void dispose();

    /**
     * checks if the session has been disposed. This can happen without the
     * users explicitly calling dispose, if for example restart() is called on
     * the PrologProcess.
     * 
     * @return true if the interface has been disposed, false otherwise
     */
    public boolean isDisposed();

}


