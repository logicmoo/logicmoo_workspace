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

import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;


/**
 * Prolog Session for asynchronous queries.
 * 
 * An AsyncPrologSession is actually a batch processor. As with the normal
 * PrologSession, a single prolog engine thread is attached to it through its
 * complete lifetime. This prolog engine thread is called "the processor" in the
 * remainder of this document.
 * 
 * The batch can be thought of as a FIFO queue. Clients add queries to the queue
 * using the queryAll(Object,String) and queryOnce(Object,String) methods. But
 * unlike the PrologSession, the AsyncPrologSession will not wait for any reply
 * from the processor, but will return control to the calling thread immediately.
 * Clients can obtain the results of the queries by registering a
 * AsyncPrologSessionListener. The processor will write all results back into
 * the output queue. On the client side, a dedicated dispatcher thread reads and
 * parses the incoming results an will dispatch them to the registered
 * listeners.
 * 
 */
public interface AsyncPrologSession extends  Disposable {
	
	/**
	 * Adds an {@link AsyncPrologSessionListener}
	 * 
	 * @param l
	 *            the listener
	 */
	public void addBatchListener(AsyncPrologSessionListener l);

	/**
	 * Removes an {@link AsyncPrologSessionListener}
	 * 
	 * @param l
	 *            the listener
	 */
	public void removeBatchListener(AsyncPrologSessionListener l);

	/**
	 * Enqueue a request for all solutions to a goal.
	 * <p>
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * <p>
	 * Uses the flag of the session.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @throws PrologProcessException
	 * @see AsyncPrologSessionListener
	 * @see PrologProcess#queryAll(String...)
	 */
	public void queryAll(Object ticket, String query) throws PrologProcessException;
	
	/**
	 * Enqueue a request for all solutions to a goal.
	 * <p>
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @param flags
	 * @throws PrologProcessException
	 * @see AsyncPrologSessionListener
	 * @see PrologProcess#queryAll(String...)
	 */
	public void queryAll(Object ticket, String query, int flags) throws PrologProcessException;

	/**
	 * Enqueue a request for the first solution to a goal.
	 * <p>
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * <p>
	 * Uses the flag of the session.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @throws PrologProcessException 
	 * @see AsyncPrologSessionListener
	 * @see PrologProcess#queryOnce(String...)
	 */
	public void queryOnce(Object ticket, String query) throws PrologProcessException;

	/**
	 * Enqueue a request for the first solution to a goal.
	 * <p>
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @param flags
	 * @throws PrologProcessException 
	 * @see AsyncPrologSessionListener
	 * @see PrologProcess#queryOnce(String...)
	 */
	public void queryOnce(Object ticket, String query, int flags) throws PrologProcessException;

	/**
	 * Wait for pending queries.
	 * 
	 * Blocks the calling thread until all currently pending queries have been
	 * processed. Conceptually, this works like an alarm clock: the calling
	 * thread puts a marker on the queue and then goes to sleep. When the
	 * processor encounters the marker it will send it back to the dispatcher,
	 * which will wake up the sleeping thread. You can use this, if your client
	 * needs (partial) results from the enqueued queries before it can go on.
	 * Other threads may continue adding queries after the mark while this
	 * thread is waiting. Also note that another thread may abort the batch -
	 * the marker will be processed nevertheless.
	 * 
	 * @throws PrologProcessException
	 */
	public void join() throws PrologProcessException;

	/**
	 * Abort the batch.
	 * 
	 * Adds a special abort marker to the queue and sends an abort message to
	 * the processor's message queue. The processor will cut the current query
	 * at the earliest possible time and will then skip all enqueued queries
	 * until it reaches the abort marker. (other markers will be processed
	 * normally) During the whole process, the batch is operational and can be
	 * used as always. Note that abort can be used to break out of endless
	 * loops, e.g. by cutting a a repeat/0. It does however not help in waking
	 * up blocking system calls, etc. Those have to be dealt with in an
	 * application-specific manner.
	 * 
	 * @throws PrologProcessException
	 * 
	 */
	public void abort() throws PrologProcessException;

	/**
	 * Abort the batch, using a specific monitor as ticket.
	 * 
	 * Like abort(), but allows the caller to choose the ticket that is used
	 * with the abort marker. The session uses this ticket as monitor when
	 * waiting for the marker echo. The idea is to allow something like this:
	 * <pre>
	 * final Object lock = new Object();
	 * session.queryOnce("some_blocking_predicate");
	 * synchronized (lock) {
	 *   Thread thread = new Thread() {
	 *      public void run() {
     *         synchronized (lock) {
	 *            //some code to wake up the processor
	 * 	          //It needs to be called AFTER the abort marker was written
	 *	          //and after the abort message was send.
	 *	       }
     *	    }
	 *   };
	 *   thread.start();
	 *   session.abort(lock);
	 * }
	 * </pre>
	 * <p>
	 * This method is mainly useful for test cases, where it is important that
	 * things happen in a certain order. You should overuse it in your
	 * application. 
	 * @param monitor 
	 * @throws PrologProcessException 
	 */
	public void abort(Object monitor) throws PrologProcessException;

	
	/**
	 * Checks whether a request is on queue.
	 * 
	 * @param ticket the ticket used with the request
	 * @return true if at least one request was enqueued using this ticket,
	 * and this request has not yet been processed
	 * 
	 */
	public boolean isPending(Object ticket);
	
	/**
	 * 
	 * @return true if there are no pending requests
	 */
	public boolean isIdle();

	/**
	 * Dispose the batch.
	 * 
	 * Adds a special end_of_batch marker to the queue. No more queries may
	 * follow this marker. The processor will continue to process all enqueued
	 * queries and markers until it reaches the end_of_batch marker. It will
	 * then notify the listeners, shut down the dispatcher and close the batch.
	 * 
	 * While the batch is disposing, it is still possible to call join() and
	 * abort(), Once the batch has been closed, this calls will have no effect.
	 */
	@Override
	public void dispose();

	/**
	 * Check if the batch is disposed.
	 * See: {@link #dispose()}
	 * 
	 * @return true if the batch is disposed or in the process of being
	 *         disposed.
	 */
	@Override
	public boolean isDisposed();

	/**
	 * @return the thread alias of the processor
	 */
	public String getProcessorThreadAlias();
}


