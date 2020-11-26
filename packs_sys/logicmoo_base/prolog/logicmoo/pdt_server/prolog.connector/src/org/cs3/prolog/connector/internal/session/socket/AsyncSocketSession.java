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

package org.cs3.prolog.connector.internal.session.socket;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.cterm.CTermUtil;
import org.cs3.prolog.connector.internal.process.socket.SocketClient;
import org.cs3.prolog.connector.internal.process.socket.SocketCommunicationConstants;
import org.cs3.prolog.connector.internal.process.socket.SocketPrologProcess;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.AsyncPrologSessionEvent;
import org.cs3.prolog.connector.session.AsyncPrologSessionListener;
import org.cs3.prolog.connector.session.PrologSession;

public class AsyncSocketSession implements AsyncPrologSession {
	
	private SocketPrologProcess process;
	private SocketClient client;
	private Thread dispatcher;
	private boolean disposing;
	private int ticketCounter;
	private HashMap<Integer,Object> tickets = new HashMap<Integer, Object>();
	private Vector<AsyncPrologSessionListener> listeners=new Vector<AsyncPrologSessionListener>();
	private Object lastAbortTicket;
	private HashMap<Integer,String> queries= new HashMap<Integer, String>();
	private HashMap<Integer,Integer> queryFlags = new HashMap<Integer, Integer>();
	private Exception batchError;
	private int flags;

	@Override
	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}	
	}

	@Override
	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}		
	}
	
	public AsyncSocketSession(SocketClient client, SocketPrologProcess process, int flags) throws IOException {
		this.client = client;
		this.process = process;
		this.flags=flags;
		enterBatch();	
	}

	private boolean readAndDispatch() throws PrologProcessException {
		try {
			String line=client.readln().trim();
			Thread.currentThread().setName("async result dispatcher ("+client.getProcessorThread()+")");
			if(SocketCommunicationConstants.EOB_COMPLETE.equals(line)){
				dispatchBatchComplete();
				return false;
			}
			else if(line.startsWith(SocketCommunicationConstants.ABORT_COMPLETE)){				
				dispatchAbortComplete(Integer.parseInt(line.substring(SocketCommunicationConstants.ABORT_COMPLETE.length())));
			}
			else if(line.startsWith(SocketCommunicationConstants.JOIN_COMPLETE)){				
				dispatchJoinComplete(Integer.parseInt(line.substring(SocketCommunicationConstants.JOIN_COMPLETE.length())));
			}
			else if(line.startsWith(SocketCommunicationConstants.RESULTS_FOR_QUERY)){				
				readAndDispatchResults(Integer.parseInt(line.substring(SocketCommunicationConstants.RESULTS_FOR_QUERY.length())));
			}
			else if(line.startsWith(SocketCommunicationConstants.SKIPPING_QUERY)){				
				dispatchSkippingQuery(Integer.parseInt(line.substring(SocketCommunicationConstants.SKIPPING_QUERY.length())));
			}
			
		} catch (IOException e) {
			handleBatchError(e);
			throw process.error(e);
		}
		return true;
	}
	
	

	private void handleBatchError(Exception e) {
		this.batchError=e;
		Vector<Object> cloned = new Vector<Object>();
		synchronized (tickets) {			
			cloned.addAll(tickets.values());
			tickets.clear();
			queries.clear();
		}
		for (Iterator<Object> it = cloned.iterator(); it.hasNext();) {
			Object ticket = it.next();
			synchronized (ticket) {
				ticket.notifyAll();	
			}
		}
		fireBatchError(e);		
	}

	private void dispatchBatchComplete() {
		fireBatchComplete();
		synchronized (tickets) {
			tickets.clear();
			Vector<Object> values = new Vector<Object>(tickets.values());
			for (Iterator<Object> it = values.iterator(); it.hasNext();) {
				Object ticket=it.next();				
				synchronized (ticket) {
					ticket.notifyAll();
				}
			}
		}
	}
	
	private void dispatchSkippingQuery(int id) {
		Object ticket = getTicket(id);
		fireGoalSkipped(id,ticket);
		removeTicket(id);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}

	private void dispatchJoinComplete(int id) {
		Object ticket = getTicket(id);
		fireJoinComplete(id,ticket);
		removeTicket(id);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}
	
	private void dispatchAbortComplete(int id) {
		Debug.info("abort complete recieved, id="+id);
		Object ticket = getTicket(id);
		synchronized (lastAbortTicket) {
			if(lastAbortTicket==ticket){
				lastAbortTicket=null;
			}
		}
		removeTicket(id);
		fireAbortComplete(id,ticket);
		Debug.info("listeners were notified about abort completion, id="+id);
		synchronized (ticket) {
			Debug.info("notifying waiting threads, id="+id);
			ticket.notifyAll();
			Debug.info("notifying done, id="+id);
		}
	}
	
	
	private void readAndDispatchResults(int id) throws IOException {
		Object ticket = getTicket(id);
		int flags = getFlags(id);
		try{				
			while (read_solution(id,ticket,flags));			
		}
		finally{
			synchronized (ticket) {
				ticket.notifyAll();
			}
		}
	}


	private void fireBatchComplete() {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {				
				l.batchComplete(e);
			}
		}
		
	}
	
	private void fireJoinComplete(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.joinComplete(e);
			}
		}
	}

	private void fireAbortComplete(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.abortComplete(e);
			}
		}
	}
	
	private void fireGoalSucceeded(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		e.query=getQuery(id);
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalSucceeded(e);
			}
		}
	}

	private void fireGoalFailed(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalFailed(e);
			}
		}
	}

	private void fireGoalRaisedException(int id,Object ticket, String string) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,string);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalRaisedException(e);
			}
		}
	}

	private void fireGoalHasSolution(int id,Object ticket, Map<String,Object> result) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,result);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalHasSolution(e);
			}
		}
	}

	private void fireGoalSkipped(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalSkipped(e);
			}
		}
	}

	private void fireGoalCut(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.goalCut(e);
			}
		}
	}
	private void fireBatchError(Exception e2) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		e.exception=e2;
		synchronized (listeners) {
			for (AsyncPrologSessionListener l:listeners) {
				l.batchError(e);	
			}
		}
	}
	
	
	
	private boolean read_solution(int id,Object ticket,int flags) throws IOException {
		HashMap<String,Object> result = new HashMap<String, Object>();
		// try to read a variable name
		while (true) {
			String varname = (String) client.readValue(PrologProcess.NONE);
			if (varname == null) {
				// there was no respective data
				String line = client.readln();
				// Debug.debug("parsing: "+line);
				if (line == null) {
					throw new IOException("don't know what to do.");
				}
				if (line.startsWith(SocketCommunicationConstants.ERROR)) {
					fireGoalRaisedException(id,ticket, line.substring(SocketCommunicationConstants.ERROR.length()));
					removeTicket(id);
					return false;
				}
				if (SocketCommunicationConstants.END_OF_SOLUTION.equals(line)) {// yes
					fireGoalHasSolution(id,ticket, result);
					return true;
				}
				if (SocketCommunicationConstants.NO.equals(line)) {
					fireGoalFailed(id,ticket);
					removeTicket(id);
					return false;
				}
				if (SocketCommunicationConstants.YES.equals(line)) {
					fireGoalSucceeded(id,ticket);
					removeTicket(id);
					return false;
				}
				if (SocketCommunicationConstants.CUT.equals(line)) {
					fireGoalCut(id,ticket);
					removeTicket(id);
					return false;
				}
			} else {
				// so we have a variable name.
				// then there should also be a variabe value.
				Object value = client.readValue(flags);
				if (value == null) {
					throw new IOException(
							"could not read value for variable " + varname);
				}
				result.put(varname, value);
			}
		}
	}

	private void enterBatch() throws IOException {
		this.dispatcher = new Thread("Async Query Result Dispatcher"){
			@Override
			public void run() {
				try{
					while(readAndDispatch());
				} catch(Exception e){
					Debug.report(e);
				}
			}
		};
		client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
		client.writeln(SocketCommunicationConstants.ENTER_BATCH);
		client.readUntil(SocketCommunicationConstants.GO_AHEAD);
		dispatcher.start();	
	}
	
	private void exitBatch() throws IOException, PrologProcessException{
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call exitBatch() from dispatch thread");
		}
		try{
			client.writeln(SocketCommunicationConstants.EOB);
			
			dispatcher.join(process.getTimeout());
			if(dispatcher.isAlive()){
				Debug.error("Dispatcher won't die. Trying to abort it.");
				abort();
				dispatcher.join(process.getTimeout());
				if(dispatcher.isAlive()){
					Debug.error("Thread is still alive. I will not longer wait for it.");					
				}
			}
			synchronized (listeners) {
				listeners.clear();
			}
			client.readUntil(SocketCommunicationConstants.OK);
		} catch (InterruptedException e) {
			Debug.rethrow(e);			
		}
	}

	private void configureProtocol(int flags) throws PrologProcessException {
		/*
		 * the only thing that needs to be configured on the server side, is
		 * whether or not lists are processed.
		 */
		boolean processLists = (flags & PrologProcess.PROCESS_LISTS) > 0;
		setProtocolOption("interprete_lists", Boolean.toString(processLists));
	}

	@Override
	public void queryOnce(Object ticket, String query) throws PrologProcessException {
		queryOnce(ticket,query,this.flags);
	}
	
	@Override
	public void queryOnce(Object ticket, String query,int flags) throws PrologProcessException {
		CTermUtil.checkFlags(flags);
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		configureProtocol(flags);
		
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket,query,flags);
		try {
			String command = "query_once("+id+").";
			client.writeln(command);
			client.writeln(query);
		} catch (IOException e) {
			throw process.error(e);
		}
	}
	@Override
	public void queryAll(Object ticket, String query) throws PrologProcessException {
		queryAll(ticket,query,this.flags);
	}
	@Override
	public void queryAll(Object ticket, String query,int flags) throws PrologProcessException {
		CTermUtil.checkFlags(flags);
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		configureProtocol(flags);
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket,query,flags);
		try {
			client.writeln("query_all("+id+").");
			client.writeln(query);
		} catch (IOException e) {
			throw process.error(e);
		}
	}

	private int storeTicket(Object ticket,String query,int flags) {
		int id=ticketCounter++;
		if(query==null&&!(ticket instanceof _AbortTicket)){
			Debug.debug("debug");
		}
		synchronized (tickets) {
			Integer key = new Integer(id);
			tickets.put(key,ticket);
			queries.put(key,query);
			this.queryFlags.put(key,flags);
		}		
		return id;
	}

	@Override
	public boolean isPending(Object ticket){
		synchronized (tickets) {
			return tickets.containsValue(ticket);
		}
	}
	public String getQuery(int id){
		synchronized (tickets) {
			return queries.get(new Integer(id));
		}
	}
	
	private int getFlags(int id) {
		synchronized(tickets){
			return queryFlags.get(id);
		}
	}
	
	private Object getTicket(int id){
		synchronized (tickets) {
			Integer key = new Integer(id);			
			Object ticket = tickets.get(key);			
			return ticket;
		}
	}
	
	private void removeTicket(int id){
		synchronized (tickets) {
			Integer key = new Integer(id);			
			tickets.remove(key);			
			queries.remove(key);
		}
	}

	@Override
	public void join() throws PrologProcessException{
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call join() from dispatch thread!");
		}
		Object ticket = new Object();
		int id = storeTicket(ticket,null,PrologProcess.NONE);
		try {
			synchronized (ticket) {
				if(!disposing){
					client.writeln("join("+id+").");
				}
				while(isPending(ticket)){
					ticket.wait();
				}
				if(this.batchError!=null){
					throw new PrologProcessException(batchError);
				}
			}
		} catch (IOException e) {
			throw process.error(e);
		} catch (InterruptedException e) {
			Debug.rethrow(e);
		}
	}
	private static class _AbortTicket{}
	
	@Override
	public void abort() throws PrologProcessException {
		abort(new _AbortTicket());
	}
	
	@Override
	public void abort(Object ticket) throws PrologProcessException {
		Debug.debug("enter 2");
		if(ticket==null){
			throw new IllegalArgumentException("null ticket!");
		}
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call abort() from dispatch thread!");
		}
		if(client==null){
			return;
		}
		if(isIdle()){
			fireAbortComplete(-1,ticket);
			synchronized (ticket) {
				
				ticket.notifyAll();
			}
			return;
		}
		//Object ticket = new Object();
		lastAbortTicket=ticket;
		int id = storeTicket(ticket,null,PrologProcess.NONE);
		Debug.info("abort ticket stored, id="+id);
		PrologSession controlSession = null;
		try {
			controlSession=process.getSession(PrologProcess.NONE);
			controlSession.queryOnce("thread_send_message('"+client.getProcessorThread()+"', batch_message(abort("+id+")))");
			Debug.info("async abort request queued, id="+id);
			synchronized (ticket) {
				if(!disposing){
					client.writeln("abort("+id+").");			
					Debug.info("sync abort marker queued, id="+id);
				}
				
				while(isPending(ticket)){
					Debug.info("waiting for pending ticket, id="+id);
					if(id==8){
						Debug.debug("debug");
					}
					Debug.debug("enter 4");
					ticket.wait();
				}
				Debug.info("abort ticket is not/no longer pending, id="+id);
				
			}
		} catch (IOException e) {
			throw process.error(e);
		} catch (InterruptedException e) {
			Debug.rethrow(e);
		} finally{
			if (controlSession != null){
				controlSession.dispose();
			}
		}	
	}
	
	@Override
	public void close() {
		dispose();
	}
	
	@Override
	public void dispose() {
		if (isDisposed()) {
			return;
		}
		disposing=true;
		try {
			if(process.getError()==null){
				exitBatch();
			}			
			client.close();
		} catch (Exception e) {
			process.error(e);
		} finally {
			client = null;
			disposing = false;
		}
	}
	
	public Object getLastAbortTicket(){
		return lastAbortTicket;
	}
	
	@Override
	public boolean isDisposed() {
		return disposing||client == null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#getPrologProcess()
	 */
	public PrologProcess getPrologProcess() {
		return process;
	}
	
	private void setProtocolOption(String id, String value) throws PrologProcessException {
		try {			
			client.writeln("set_option("+id+","+value+").");			
		} catch (IOException e) {
			throw process.error(e);
		}
	}
	
	@Override
	public String getProcessorThreadAlias() {
		return client.getProcessorThread();
	}

	@Override
	public boolean isIdle() {
		if(isDisposed()){
			return true;
		}
		synchronized (tickets) {
			return tickets.isEmpty();
		}
	}
}


