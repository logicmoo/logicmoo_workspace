/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.service;

import java.util.List;

import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;

/**
 * An IPrologProcessService manages an active {@link PrologProcess} and
 * consults files into this active PrologProcess.<br/>
 * The active PrologProcess can be accessed and set.
 * {@link ActivePrologProcessListener}s can be registered to listen to each
 * change of the active PrologProcess.<br/>
 * Consults can be triggered via the <code>consultFile(s)</code> methods.
 * The consult will be done in the active PrologProcess or in a given PrologProcess. The call of the
 * consult predicate <code>pdt_reload/1</code> is executed by the registered
 * {@link PDTReloadExecutor} with the highest priority. If an executor fails,
 * the next one will be tried out. Utility methods to retrieve file names
 * conforming to prolog syntax can be found in {@link QueryUtils}. Registered
 * {@link ConsultListener}s are notified before and after executing
 * <code>pdt_reload/1</code>.<br/>
 * This service can be acquired via
 * 
 * <pre>
 * PrologConnectorPlugin.getDefault().getPrologProcessService()
 * </pre>
 * 
 */
public interface IPrologProcessService {

	/**
	 * Registers an {@link PDTReloadExecutor}.
	 * 
	 * @param executor
	 *            the executor
	 */
	void registerPDTReloadExecutor(PDTReloadExecutor executor);

	/**
	 * Unregisters an {@link PDTReloadExecutor}.
	 * 
	 * @param executor
	 *            the executor
	 */
	void unRegisterPDTReloadExecutor(PDTReloadExecutor executor);

	/**
	 * Unregisters an {@link ConsultListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void registerConsultListener(ConsultListener listener);

	/**
	 * Unregisters an {@link ConsultListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void unRegisterConsultListener(ConsultListener listener);

	/**
	 * Consults a file into the active PrologProcess.
	 * 
	 * @param file
	 *            the file
	 */
	void consultFile(IFile file);

	/**
	 * Consults a file into the given PrologProcess.
	 * 
	 * @param file
	 *            the file
	 * @param process
	 *            the PrologProcess
	 */
	void consultFile(IFile file, PrologProcess process);

	/**
	 * Consults a file into the given PrologProcess and prints the given message.
	 * 
	 * @param file
	 *            the file
	 * @param process
	 *            the PrologProcess
	 * @param message
	 *            the message
	 */
	void consultFile(IFile file, PrologProcess process, String message);
	
	/**
	 * Consults a file into the active PrologProcess.
	 * 
	 * @param file
	 *            the file
	 */
	void consultFile(String file);

	/**
	 * Consults a file into the given PrologProcess.
	 * 
	 * @param file
	 *            the file
	 * @param process
	 *            the PrologProcess
	 */
	void consultFile(String file, PrologProcess process);
	
	/**
	 * Consults a file into the given PrologProcess and prints the given message.
	 * 
	 * @param file
	 *            the file
	 * @param process
	 *            the PrologProcess
	 * @param message
	 *            the message
	 */
	void consultFile(String file, PrologProcess process, String message);
	
	/**
	 * Consults a list of files into the active PrologProcess.
	 * 
	 * @param files
	 *            the list of files
	 */
	void consultFiles(List<IFile> files);

	/**
	 * Consults a list of files into the given PrologProcess.
	 * 
	 * @param files
	 *            the list of files
	 * @param process
	 *            the PrologProcess
	 */
	void consultFiles(List<IFile> files, PrologProcess process);
	
	/**
	 * Consults a list of files into the given PrologProcess and prints the given message.
	 * 
	 * @param files
	 *            the list of files
	 * @param process
	 *            the PrologProcess
	 * @param message
	 *            the message
	 */
	void consultFiles(List<IFile> files, PrologProcess process, String message);
	
	/**
	 * Registers an {@link ActivePrologProcessListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void registerActivePrologProcessListener(ActivePrologProcessListener listener);

	/**
	 * Unregisters an {@link ActivePrologProcessListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void unRegisterActivePrologProcessListener(ActivePrologProcessListener listener);

	/**
	 * Accesses the active PrologProcess.
	 * 
	 * @return the active PrologProcess
	 */
	PrologProcess getActivePrologProcess();

	/**
	 * Checks if an active PrologProcess is set.
	 * 
	 * @return True if an active PrologProcess is set, false otherwise.
	 */
	boolean hasActivePrologProcess();
	
	/**
	 * Sets the active PrologProcess.
	 * 
	 * @param process
	 *            the PrologProcess
	 */
	void setActivePrologProcess(PrologProcess process);

}
