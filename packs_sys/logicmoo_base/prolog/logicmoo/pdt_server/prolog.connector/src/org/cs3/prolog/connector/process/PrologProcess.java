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

package org.cs3.prolog.connector.process;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.common.PreferenceProvider;
import org.cs3.prolog.connector.cterm.CTerm;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.PrologSession;

/**
 * Provides the main interface for interacting with Prolog processes. Each
 * instance of PrologProcess corresponds to exactly one process.
 */
public interface PrologProcess {

	/**
	 * Session flag.
	 * 
	 * All bindings are reported as java.lang.String objects using the canonical
	 * syntax. Atoms are quoted when necessary. lists are not processed. I.e.
	 * all bindings should be of a form as created by write_canonical/1.
	 * 
	 */
	public final static int NONE = 0;

	/**
	 * Session flag.
	 * 
	 * Deviates from {@link #NONE} in that bindings that are atoms are unquoted.
	 * This is supposed to mimic the "old" behavior where bindings where written
	 * into the stream using write/2 rather than write_canonical/2 or writeq/2.
	 * Note that this will NOT un-quote atoms nested in complex terms, so the
	 * behavior is slightly different than it was before.
	 */
	public final static int UNQUOTE_ATOMS = 1;

	/**
	 * Session flag.
	 * 
	 * Deviates from {@link #NONE} in that bindings that are lists are reported
	 * as java.util.List instances. Elements are processed recursively.
	 */
	public final static int PROCESS_LISTS = 2;

	/**
	 * Session flag.
	 * 
	 * Deviates from NONE in that all bindings are reported as instances of
	 * {@link CTerm}. Cannot be used together with {@link #UNQUOTE_ATOMS}. Doing so will
	 * raise an IllegalArgumentException. Can be combined with {@link #PROCESS_LISTS}.
	 * 
	 */
	public final static int CTERMS = 4;

	
	/**
	 * Session flag.
	 * 
	 * If this flag is set, all variables will be part of the result, even
	 * the variables which are not bound (you will have entries like A=A)
	 * 
	 */
	public final static int UNBOUND_VARIABLES = 8;
	
	/**
	 * Session flag.
	 * 
	 * This is what will be used by if no other flag is selected. Atoms will be
	 * unquoted and lists will be processed.
	 */
	public final static int DEFAULT = UNQUOTE_ATOMS | PROCESS_LISTS;
	
	/**
	 * 
	 * Session flag.
	 * 
	 * This is what should be used by JPC. It creates CTerms and create
	 * result entries for unbound variables.
	 * 
	 */
	public final static int JPC = CTERMS | UNBOUND_VARIABLES;
	
	/**
	 * Returns a prolog session.
	 * <p>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologProcess is in UP state. During startup, this
	 * call will block until the process is up. In state SHUTODWN or DOWN, this
	 * will raise an IllegalStateException.
	 * <p>
	 * Uses the flag provided by {@link #getSessionFlag()}.
	 * 
	 * @return a new Session Object
	 * @throws PrologProcessException
	 */
	public abstract PrologSession getSession() throws PrologProcessException;

	/**
	 * Returns a prolog session.
	 * <p>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologProcess is in UP state. During startup, this
	 * call will block until the process is up. in state SHUTODWN or DOWN, this
	 * will raise an IllegalStateException.
	 * <p>
	 * Flag sets the kind of objects returned by the queries.
	 * 
	 * @param flags
	 * @return a new Session Object
	 * @throws PrologProcessException
	 */
	public abstract PrologSession getSession(int flags) throws PrologProcessException;
	
	/**
	 * Stop the prolog system (if it is up). This will terminate all running
	 * sessions and shut down the prolog process.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void stop() throws PrologProcessException;

	/**
	 * Starts the prolog system (if it is down).
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void start() throws PrologProcessException;

	/**
	 * Restarts the prolog system.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void restart() throws PrologProcessException;

	/**
	 * Stops and resets the prolog system.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void reset() throws PrologProcessException;

	/**
	 * Checks whether the process is up and running.
	 * 
	 * @return true if the process is up and running
	 */
	public boolean isUp();

	/**
	 * Checks whether the process is down.
	 * <p>
	 * This is not the same as <code>!{@link #isUp()}</code>. During startup and
	 * shutdown both methods return false.
	 * 
	 * @return true if the process is down
	 */
	public boolean isDown();

	/**
	 * Initializes options of this prolog interface from the given preference provider.
	 * 
	 * @param provider the preference provider
	 */
	public void initOptions(PreferenceProvider provider);	
	
	/**
	 * @return the Operating System invocation
	 */
	public String getOSInvocation();
	
	/**
	 * Sets the Operating System invocation.
	 * 
	 * @param osInvocation
	 */
	public void setOSInvocation(String osInvocation);
	
	/**
	 * 
	 * @return the path to the Prolog executable
	 */
	public String getExecutablePath();
	
	/**
	 * Sets the path to the Prolog executable.
	 * @param executablePath
	 */
	public void setExecutablePath(String executablePath);
	
	/**
	 * 
	 * @return the command line arguments
	 */
	public String getCommandLineArguments();
	
	/**
	 * Sets the command line arguments.
	 * @param commandLineArguments
	 */
	public void setCommandLineArguments(String commandLineArguments);
	
	/**
	 * @return the additional startup file
	 */
	public String getAdditionalStartupFile();
	
	/**
	 * Sets the additional startup file.
	 * 
	 * @param additionalStartupFile
	 *            path to the startup file
	 */
	public void setAdditionalStartupFile(String additionalStartupFile);
	
	/**
	 * @return the extra environment variables
	 */
	public String getEnvironment() ;
	
	/**
	 * Sets the extra environment variables.
	 * 
	 * The entries are separated by <code>;</code> and have the structure:
	 * <pre>VARIABLE=VALUE</pre>
	 * @param environmentVariables
	 */
	public void setEnvironment(String environmentVariables) ;
	
	/**
	 * @return the host
	 */
	public String getHost();
	
	/**
	 * Sets the host.
	 * @param host
	 */
	public void setHost(String host);
	
	/**
	 * Returns the time in milliseconds to wait for the initialization of the process.
	 * 
	 * @return timeout in milliseconds. 
	 */
	public int getTimeout();
	
	/**
	 * Sets the time in milliseconds to wait for the initialization of the process.
	 * 
	 * @param timeout in milliseconds
	 */
	public void setTimeout(String timeout);
	
	/**
	 * Generic way to get an attribute.
	 * @param attribute
	 * @return the value of the attribute;<br>null if attribute is not defined
	 */
	public Object getAttribute(String attribute);
	
	/**
	 * Generic way to set an attribute.
	 * @param attribute
	 * @param value
	 */
	public void setAttribute(String attribute, Object value);
	
	/**
	 * @return the {@link StartupStrategy}
	 */
	public StartupStrategy getStartupStrategy();
	
	/**
	 * Sets the {@link StartupStrategy}.
	 * @param startupStrategy
	 */
	public void setStartupStrategy(StartupStrategy startupStrategy);

	/**
	 * Adds a life cycle hook to this process.
	 * 
	 * @param hook
	 *            the life cycle hook
	 * @param id
	 *            the id of the life cycle hook
	 * @param dependencies
	 *            ids of other registered life cycle hooks on which this hook
	 *            depends
	 */
	public void addLifeCycleHook(LifeCycleHook hook, String id, String[] dependencies);
	
	/**
	 * Unregister a lifeCycleHook.
	 * 
	 * This will remove ALL hooks registered for this id.
	 * 
	 * @param hookId
	 */
	public void removeLifeCycleHook(String hookId);
	
	/**
	 * Unregister a specific lifeCycleHook.
	 * 
	 * This will remove only the specified hook.
	 * 
	 * @param hook
	 * @param hookId
	 */
	public void removeLifeCycleHook(final LifeCycleHook hook,final String hookId);
	
	/**
	 * Returns an asynchronous prolog session.
	 * <p>
	 * Contrary to a {@link PrologSession} asynchronous sessions don't wait for a query to finish.
	 * <p>
	 * Uses the flag provided by {@link #getSessionFlag()}.
	 * 
	 * @return a new asynchronous session object
	 * @throws PrologProcessException
	 * @see #getSession()
	 */
	public AsyncPrologSession getAsyncSession() throws PrologProcessException;
	
	/**
	 * Returns an asynchronous prolog session.
	 * <p>
	 * Contrary to a {@link PrologSession} asynchronous sessions don't wait for
	 * a query to finish.
	 * 
	 * @param flags
	 * 
	 * @return a new asynchronous session object
	 * @throws PrologProcessException
	 * @see #getSession(int)
	 */
	public AsyncPrologSession getAsyncSession(int flags) throws PrologProcessException;
	
	/**
	 * Checks if the process is in an error state, e.g. if the corresponding
	 * process has been killed externally.
	 * 
	 * @return true if the process is in an error state
	 */
	public boolean hasError();
	
	/**
	 * Executes the given query and returns all results. The query is created by
	 * connecting the given goals conjunctively. The result is always a list of
	 * maps. Each map represents one result of the query containing the bindings
	 * for all variables. The variables are the keys of each map. If the query
	 * fails the returned list is empty.
	 * 
	 * <h3>Result types</h3>
	 * 
	 * The type of the result differs, depending on the given flag. If not
	 * specified otherwise the {@link #DEFAULT} flag will be used. This means,
	 * that the results will be instances of Strings or instances of
	 * java.util.List (in case of Prolog lists). The elements in this list can
	 * also be of type String or List. This is the recommended way if the
	 * results only consist of atoms, numbers and lists or if the string
	 * representation of a result is sufficient for the context of the program.
	 * <p>
	 * If the result contains compound Prolog terms and the values inside of
	 * these terms are important and need to be processed at a later point, the
	 * recommended flag is {@link #CTERMS} (see {@link CTerm}). Every element
	 * (atom, integer, compound term ...) will be transformed to a corresponding
	 * Java object. See the package {@link org.cs3.prolog.connector.cterm} for a
	 * list of possible objects.
	 * <p>
	 * Uses the flag provided by {@link #getSessionFlag()}.
	 * 
	 * @param predicates
	 *            a number of goals
	 * @return all results of the query or an empty list if the query fails
	 * @throws PrologProcessException
	 */
	public List<Map<String, Object>> queryAll(String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns all results. The query is created by
	 * connecting the given goals conjunctively. The result is always a list of
	 * maps. Each map represents one result of the query containing the bindings
	 * for all variables. The variables are the keys of each map. If the query
	 * fails the returned list is empty.
	 * 
	 * <h3>Result types</h3>
	 * 
	 * The type of the result differs, depending on the given flag. If not
	 * specified otherwise the {@link #DEFAULT} flag will be used. This means,
	 * that the results will be instances of Strings or instances of
	 * java.util.List (in case of Prolog lists). The elements in this list can
	 * also be of type String or List. This is the recommended way if the
	 * results only consist of atoms, numbers and lists or if the string
	 * representation of a result is sufficient for the context of the program.
	 * <p>
	 * If the result contains compound Prolog terms and the values inside of
	 * these terms are important and need to be processed at a later point, the
	 * recommended flag is {@link #CTERMS} (see {@link CTerm}). Every element
	 * (atom, integer, compound term ...) will be transformed to a corresponding
	 * Java object. See the package {@link org.cs3.prolog.connector.cterm} for a
	 * list of possible objects.
	 * 
	 * @param flag
	 *            kind of objects returned by the query
	 * @param predicates
	 *            a number of goals
	 * @return all results of the query or an empty list if the query fails
	 * @throws PrologProcessException
	 */
	public List<Map<String, Object>> queryAll(int flag, String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns the first result. The query is
	 * created by connecting the given goals conjunctively. If the query
	 * succeeds, the result is a map containing the bindings for all variables.
	 * The variables are the keys of the map. If the query fails this method
	 * returns null.
	 * 
	 * <h3>Result types</h3>
	 * 
	 * The type of the result differs, depending on the given flag. If not
	 * specified otherwise the {@link #DEFAULT} flag will be used. This means,
	 * that the results will be instances of Strings or instances of
	 * java.util.List (in case of Prolog lists). The elements in this list can
	 * also be of type String or List. This is the recommended way if the
	 * results only consist of atoms, numbers and lists or if the string
	 * representation of a result is sufficient for the context of the program.
	 * <p>
	 * If the result contains compound Prolog terms and the values inside of
	 * these terms are important and need to be processed at a later point, the
	 * recommended flag is {@link #CTERMS} (see {@link CTerm}). Every element
	 * (atom, integer, compound term ...) will be transformed to a corresponding
	 * Java object. See the package {@link org.cs3.prolog.connector.cterm} for a
	 * list of possible objects.
	 * <p>
	 * Uses the flag provided by {@link #getSessionFlag()}.
	 * 
	 * @param predicates
	 *            a number of goals
	 * @return the first result of the query or null if the query fails
	 * @throws PrologProcessException
	 */
	public Map<String, Object> queryOnce(String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns the first result. The query is
	 * created by connecting the given goals conjunctively. If the query
	 * succeeds, the result is a map containing the bindings for all variables.
	 * The variables are the keys of the map. If the query fails this method
	 * returns null.
	 * 
	 * <h3>Result types</h3>
	 * 
	 * The type of the result differs, depending on the given flag. If not
	 * specified otherwise the {@link #DEFAULT} flag will be used. This means,
	 * that the results will be instances of Strings or instances of
	 * java.util.List (in case of Prolog lists). The elements in this list can
	 * also be of type String or List. This is the recommended way if the
	 * results only consist of atoms, numbers and lists or if the string
	 * representation of a result is sufficient for the context of the program.
	 * <p>
	 * If the result contains compound Prolog terms and the values inside of
	 * these terms are important and need to be processed at a later point, the
	 * recommended flag is {@link #CTERMS} (see {@link CTerm}). Every element
	 * (atom, integer, compound term ...) will be transformed to a corresponding
	 * Java object. See the package {@link org.cs3.prolog.connector.cterm} for a
	 * list of possible objects.
	 * <p>
	 * Flag sets the kind of objects returned by the query.
	 * 
	 * @param flag
	 *            kind of objects returned by the query
	 * @param predicates
	 *            a number of goals
	 * @return the first result of the query or null if the query fails
	 * @throws PrologProcessException
	 */
	public Map<String, Object> queryOnce(int flag, String... predicates) throws PrologProcessException;
	
	/**
	 * Returns the flag for new sessions. If no flag is specified for a query or
	 * a session, this flag will be used.
	 * 
	 * @return the flag for new sessions
	 */
	public int getSessionFlag();

	/**
	 * Sets the flag for new sessions. If no flag is specified for a query or a
	 * session, this flag will be used.
	 * 
	 * @param flag
	 *            the flag for new sessions
	 */
	public void setSessionFlag(int flag);
	
	/**
	 * Consults the given file to this process.
	 * <p>
	 * This is done by calling the Prolog predicate <code>consult/1</code>.
	 * 
	 * @param file
	 *            the file to consult
	 * @throws PrologProcessException
	 */
	public void consult(File file) throws PrologProcessException;
	
}


