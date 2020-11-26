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

package org.cs3.prolog.connector.internal.process.socket;

import java.io.IOException;
import java.io.OutputStream;

class OutputStreamProxy extends OutputStream {
	private OutputStream out;
	private SocketClient client;

	private LogBuffer logBuf;


	private OutputStreamProxy(OutputStream out, LogBuffer logBuf) {
		super();
		this.out = out;
		this.logBuf = logBuf;
	}

	public OutputStreamProxy(OutputStream out, LogBuffer logBuf, SocketClient client) {
		this(out,logBuf);
		this.client = client;
	}

	@Override
	public void close() throws IOException {
		client.close();
	}

	@Override
	public void flush() throws IOException {
		out.flush();
	}

	@Override
	public void write(byte[] b) throws IOException {
		out.write(b);
		logBuf.log("write", b);
	}

	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		out.write(b, off, len);
		logBuf.log("write", b, off, len);
	}

	@Override
	public void write(int b) throws IOException {
		out.write(b);
		logBuf.log("write", (char) b);
	}
}


