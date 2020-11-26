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
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.TimeUnit;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;

public class JackTheProcessRipper extends Thread {

	private static final String LINUX_AND_OTHER_KILL_COMMAND = "kill -KILL ";
	private static final String WINDOWS_KILL_COMMAND = "taskkill /F /PID ";
	private static final Long NONVALID_PID = Long.valueOf(-1);
	private static final long TIMEOUT_WAITING_FOR_AN_PID = 1000L;
	
	static JackTheProcessRipper instance;
	static private SynchronousQueue<Long> toBeDestroyed = new SynchronousQueue<Long>();
	static private boolean shuttingDown = false;

	private JackTheProcessRipper() {
		super("Jack the Process Ripper");
		setDaemon(false);
		Runtime theRuntime = Runtime.getRuntime();
		Thread shutdownHook = new Thread("Jack The Process Ripper Shutdown Hook") {
			@Override
			public void run() {
				try {
					Thread.sleep(TIMEOUT_WAITING_FOR_AN_PID);
				} catch(Exception E) {
					;
				}
				shuttingDown = true;
			}
		};
		theRuntime.addShutdownHook(shutdownHook);
		setDaemon(true);
		start();
	}

	public static JackTheProcessRipper getInstance() {
		if (instance == null) {
			instance = new JackTheProcessRipper();
		}
		return instance;
	}

	/**
	 * Runs until the Shutdown Hook is activated and destroys every
	 * Prolog-processed referenced by a PID that is given via
	 * JackTheProcessRipper.markForDeletion(long).
	 */
	@Override
	public void run() {
		Long processId = NONVALID_PID;
		while (!shuttingDown) {
			try {
				processId=toBeDestroyed.poll(TIMEOUT_WAITING_FOR_AN_PID,TimeUnit.MILLISECONDS);
				if (isValidProcessId(processId)) {
					killRuntimeProcess(processId);
				}
			} catch (Throwable t) {
				Debug.report(t);
			} finally {
				processId = NONVALID_PID;
			}
		}
	}

	private static boolean isValidProcessId(Long processId) {
		if (processId==null)
			return false;
		return processId.compareTo(NONVALID_PID)>0;
	}

	private static void killRuntimeProcess(long processId) throws IOException, InterruptedException {
		String killCommand;
		if(Util.isWindows()){
			killCommand= WINDOWS_KILL_COMMAND + processId;
		} else {
			killCommand= LINUX_AND_OTHER_KILL_COMMAND + processId;
		}		
		Runtime.getRuntime().exec(killCommand);
	}

	public void markForDeletion(long processId) throws InterruptedException{
		if (shuttingDown) {
			throw new IllegalStateException(
			"Java is shutting down. Processes cannot be registered for termination anymore.");
		}
		toBeDestroyed.put(processId);
	}
}

