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
import java.io.InputStream;

class InputStreamProxy extends InputStream {
	private InputStream in;
	private SocketClient client;

	private LogBuffer logBuf;


	private InputStreamProxy(InputStream in, LogBuffer logBuf) {
		super();
		this.in = in;
		this.logBuf = logBuf;
	}

	public InputStreamProxy(InputStream in, LogBuffer logBuf, SocketClient client) {
		this(in,logBuf);
		this.client = client;
	}

	@Override
	public int available() throws IOException {
		return in.available();
	}

	@Override
	public void close() throws IOException {
		client.close();
	}

	@Override
	public synchronized void mark(int readlimit) {
		in.mark(readlimit);
	}

	@Override
	public boolean markSupported() {
		return in.markSupported();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException {
		int read = in.read();
		logBuf.log("read", (char) read);
		return read;
	}

	@Override
	public int read(byte[] b) throws IOException {
		int read = in.read(b);
		logBuf.log("read", b, 0, read);
		return read;
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		int read = in.read(b, off, len);
		logBuf.log("read", b, off, read);
		return read;
	}

	@Override
	public synchronized void reset() throws IOException {
		in.reset();
	}

	@Override
	public long skip(long n) throws IOException {
		return in.skip(n);
	}

}


