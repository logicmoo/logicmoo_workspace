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

package org.cs3.prolog.connector.ui.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.process.PrologEventDispatcher;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologEvent;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.process.PrologEventListener;
import org.cs3.prolog.connector.session.PrologSession;


/**
 * 
 * Before running this test, please start an external prolog process and consult the file
 * main_socket.pl 
 * TODO: include this in the test fixture.
 *
 */
public class ConnectionToRunningPrologServerTest extends TestCase {

	public final static String FACTORY="org.cs3.pl.prolog.internal.socket.Factory";

	static PrologProcess process = null;

	@Override
	protected void setUp() throws Exception {
		if(process == null) {
			process = init();
		}
		super.setUp();
	}
	
	@Override
	protected void tearDown() throws Exception {
		
		super.tearDown();
	}
	
	Object monitor = new Object();
	int counter = 0;

	
	
	public void testConnect() throws Exception {

		PrologSession session = process.getSession(PrologProcess.DEFAULT);
		
		PrologEventListener listener = new PrologEventListener(){
			@Override
			public void update(PrologEvent e) {
				System.out.println("RECEIVED EVENT: "+e.getData() + ", SUBJECT " + e.getSubject());
				System.out.flush();
				synchronized (monitor) {
					counter++;
					if(counter == 4) {
						monitor.notifyAll();
					}
				}
			}
		};

		PrologEventListener locationListener = new PrologEventListener(){
			@Override
			public void update(PrologEvent e) {
				System.out.println("LOCALISATION: RECEIVED EVENT: "+e.getData() + ", SUBJECT " + e.getSubject());
				System.out.flush();
				synchronized (monitor) {
					counter++;
					if(counter == 4) {
						monitor.notifyAll();
					}
				}
			}
		};
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(sync:init_term_ref))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(sync:notify_if_predicate_updated))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(erase))))");
		
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:handle_message))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:handle_command))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:notify/2))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:thread_get_message/1))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:cleanup_thread/1))))");
		PrologEventDispatcher dispatcher = new PrologEventDispatcher(process);
		dispatcher.addPrologEventListener("localisation:company_nearby(MAC, Other, Distance, 1000)",locationListener);

		session.queryOnce("sync:deleteAll(magicmap:location('00-09-2D-53-27-3A', _,_,_))");
		//Samsung
		session.queryOnce("sync:add(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.12, 0))");
		

		session.queryOnce("sync:commit");
		synchronized (monitor) {
			monitor.wait(500);
		}			

		//IBM
		session.queryOnce("sync:deleteAll(magicmap:location('00-09-2D-53-27-3A', _,_,_))");
		session.queryOnce("sync:add(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.122, 0))");
		session.queryOnce("sync:commit");
		synchronized (monitor) {
			monitor.wait(500);
		}			
		
		dispatcher.addPrologEventListener("magicmap:position(A,B,C)",listener);
		
		try {
			session.queryOnce("sync:deleteAll(magicmap:position(_,_,_))");
			session.queryOnce("sync:add(magicmap:position(111,222,333))");
			session.queryOnce("sync:add(magicmap:position(222,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			session.queryOnce("sync:delete(magicmap:position(222,333,444))");
			session.queryOnce("sync:add(magicmap:position(222,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			session.queryOnce("sync:delete(magicmap:position(222,333,444))");
			session.queryOnce("sync:add(magicmap:position(242,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			System.out.flush();
			System.err.flush();
			System.out.flush();
			System.err.flush();
		} finally {
			dispatcher.stop();
			session.dispose();
		}
		synchronized (monitor) {
			monitor.wait(500);
		}
		assertEquals(counter, 4);
		
		
	}

	public void testErrors() throws Exception {
		PrologEventListener nullListener = new PrologEventListener(){
			@Override
			public void update(PrologEvent e) {
			}
		};
		PrologEventDispatcher dispatcher = new PrologEventDispatcher(process);
		try {
			
			dispatcher.addPrologEventListener("aha(",nullListener);
			fail("expected prolog exception");
		} catch(PrologException e) {

			String msg = e.getLocalizedMessage();
			assertTrue("unexpected error message: "+msg,msg.startsWith("Peer reported an error:error(syntax_error"));
		}finally{
			dispatcher.stop();
		}
	}


	private PrologProcess init() throws PrologProcessException {
//		PrologProcessFactory factory= Factory.newInstance(FACTORY);
//		PrologProcess process = factory.create();
		
        process = Connector.newUninitializedPrologProcess();
//		process.setStandAloneServer(true);		
		process.start();
		return process;
	}
	
	public void testConnectionWorks() throws Throwable{
		process.getSession(PrologProcess.DEFAULT).queryOnce("threads");
	}
}


