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
package org.cs3.prolog.connector.internal.process.socket;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

import org.cs3.prolog.connector.common.Debug;

public class ReusableSocket extends Socket implements Reusable {

    private LogBuffer logBuffer = new SimpleLogBuffer();

    public ReusableSocket(String host, int port) throws UnknownHostException,
            IOException {
        super(host, port);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#reuse()
     */
    @Override
	public void reuse() {
        logBuffer.log("socket","reuse");;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#destroy()
     */
    @Override
	public void destroy() {
        logBuffer.log("socket","destroy");
        try {
            close();
        } catch (IOException e) {
	        Debug.report(e);
	        throw new RuntimeException(e.getMessage());
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#recylce()
     */
    @Override
	public void recylce() {
        logBuffer.log("socket","recycle");
    }

    public LogBuffer getLogBuffer() {
        return this.logBuffer;
    }

}


