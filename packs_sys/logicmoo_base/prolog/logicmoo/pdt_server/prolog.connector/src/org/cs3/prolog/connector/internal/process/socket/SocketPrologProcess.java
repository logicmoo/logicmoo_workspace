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

package org.cs3.prolog.connector.internal.process.socket;

import java.io.File;
import java.io.IOException;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.PreferenceProvider;
import org.cs3.prolog.connector.internal.process.AbstractPrologProcess;
import org.cs3.prolog.connector.internal.process.ServerStartAndStopStrategy;
import org.cs3.prolog.connector.internal.session.socket.AsyncSocketSession;
import org.cs3.prolog.connector.internal.session.socket.SocketSession;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.PrologSession;

public class SocketPrologProcess extends AbstractPrologProcess {

	private class InitSession extends SocketSession {
		public InitSession(SocketClient client, AbstractPrologProcess process,int flags)
				throws IOException {
			super(client, process,flags);
		}

		@Override
		public void dispose() {
			Debug.warning("Ignoring attempt to dispose an initial session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private class ShutdownSession extends SocketSession {
		public ShutdownSession(SocketClient client, AbstractPrologProcess process, int flags)
				throws IOException {
			super(client, process,flags);
		}

		@Override
		public void dispose() {
			Debug.warning("Ignoring attempt to dispose a shutdown session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private boolean useSessionPooling = true;
	private int port = 9999;
	private boolean hidePlwin;

	private String serverLogDir;
	private String consultServerLocation;
		

	public void setPort(int port) {
		this.port = port;
	}
	public void setServerPort(String port) {
		this.port = Integer.parseInt(port);
	}
	public void setUseSessionPooling(boolean useSessionPooling) {
		this.useSessionPooling = useSessionPooling;
		pool = useSessionPooling ? new ReusablePool() : null;
	}
	public int getPort() {
		return port;
	}	
	public boolean isHidePlwin() {
		return hidePlwin;
	}
	public void setHidePlwin(boolean hidePlwin) {
		this.hidePlwin = hidePlwin;
	}
	public void setHidePlwin(String hidePlwin) {
		this.hidePlwin = Boolean.parseBoolean(hidePlwin);
	}
	public void setServerLogDir(String path){
		serverLogDir = path;
	}
	public String getServerLogDir(){
		return serverLogDir;
	}
	@Override
	public void initOptions(PreferenceProvider provider) {
		super.initOptions(provider);
//		setServerPort(provider.getPreference(PrologRuntime.PREF_PORT));
		setHidePlwin(provider.getPreference(Connector.PREF_HIDE_PLWIN));
//		setUseSessionPooling(true);
		setServerLogDir(provider.getPreference(Connector.PREF_SERVER_LOGDIR));		

	}
	
	public String getConsultServerLocation() {
		return consultServerLocation;
	}
	
	public void setConsultServerLocation(String consultServerLocation) {
		this.consultServerLocation = consultServerLocation; 
	}
	
	
	/************************************************/
	/**** Options [End] *****/
	/************************************************/	


	private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;
	
	private File lockFile;
	private ServerStartAndStopStrategy startAndStopStrategy;


	public SocketPrologProcess(String name) {		
		super(name);		
		setDefaults();
		setStartAndStopStrategy(new SocketServerStartAndStopStrategy());
	}
	
	public void setDefaults() {
		setHidePlwin(true);
		setUseSessionPooling(true);
		setServerLogDir(System.getProperty("java.io.tmpdir"));		
	}
	
	@Override
	public PrologSession getSession_impl(int flags) throws Throwable {
		ReusableSocket socket = null;
		try {
			if (useSessionPooling) {
				socket = (ReusableSocket) pool
						.findInstance(ReusableSocket.class);
			}
			if (socket == null) {
				socket = new ReusableSocket(getHost(), port);
				Debug.info("sync session creating new ReusableSocket: " + socket.getLocalPort());

			} else {
				Debug.info("sync session reusing old ReusableSocket: " + socket.getLocalPort());
			}

			SocketClient client = new SocketClient(socket);
			client.setPool(pool);
			SocketSession s = new SocketSession(client, this,flags);

			return s;
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	@Override
	public AsyncPrologSession getAsyncSession_impl(int flags) throws Throwable {
		ReusableSocket socket = null;
		try {
			if (useSessionPooling) {
				socket = (ReusableSocket) pool
						.findInstance(ReusableSocket.class);
			}
			if (socket == null) {
				socket = new ReusableSocket(getHost(), port);
				Debug.info("async session creating new ReusableSocket: " + socket.getLocalPort());
			} else {
				Debug.info("async session reusing old ReusableSocket: " + socket.getLocalPort());
			}
			SocketClient client = new SocketClient(socket);
			client.setParanoiaEnabled(false);
			client.setPool(pool);
			
			AsyncPrologSession s = new AsyncSocketSession(client, this,flags);

			return s;
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#disposeInitialSession(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	protected void disposeInitialSession(PrologSession initSession) {
		InitSession s = (InitSession) initSession;
		s.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#disposeShutdownSession(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	protected void disposeShutdownSession(PrologSession s) {
		ShutdownSession ss = (ShutdownSession) s;
		ss.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#getInitialSession()
	 */
	@Override
	protected PrologSession getInitialSession() throws PrologProcessException {
		try {
			//FIXME: LEGACY for now, should be specified by client somehow.
			return new InitSession(new SocketClient(getHost(), port), this, getSessionFlag());
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#getShutdownSession()
	 */
	@Override
	protected PrologSession getShutdownSession()
			throws PrologProcessException {
		try {
			//FIXME: LEGACY for now, should be specified by client somehow.
			return new ShutdownSession(new SocketClient(getHost(), port), this, getSessionFlag());
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#stop()
	 */
	@Override
	public  void stop() throws PrologProcessException {
		try {
			super.stop();
		} finally {
			if (pool != null) {
				pool.clear();
			}
		}
	}

	@Override
	public  PrologProcessException error(Throwable e) {
		try {
			super.error(e);
		} finally {
			if (pool != null) {
				pool.clear();
			}
		}
		return getError();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologProcess#stop()
	 */
	@Override
	public void start() throws PrologProcessException {
		Debug.info("pdt: Start Socket ");
		if (pool != null) {
			pool.clear();
		}
		super.start();
	}

	public void setLockFile(File string) {
		this.lockFile = string;

	}

	public File getLockFile() {
		return lockFile;
	}

	private File errorLogFile;
	
	public void setErrorLogFile(File file) {
		this.errorLogFile = file;

	}

	public File getErrorLogFile() {
		return errorLogFile;
	}

	@Override
	public ServerStartAndStopStrategy getStartAndStopStrategy() {	
		return this.startAndStopStrategy;
	}

	/**
	 * @param startAndStopStrategy
	 *            The startAndStopStrategy to set.
	 */
	public void setStartAndStopStrategy(ServerStartAndStopStrategy startAndStopStrategy) {
		this.startAndStopStrategy = startAndStopStrategy;
	}

	@Override
	public boolean hasError() {
		return getError()!=null;
	}


}


