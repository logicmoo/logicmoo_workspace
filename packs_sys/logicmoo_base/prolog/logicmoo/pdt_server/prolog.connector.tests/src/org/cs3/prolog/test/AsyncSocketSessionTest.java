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

package org.cs3.prolog.test;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.cterm.CCompound;
import org.cs3.prolog.connector.cterm.CEmptyList;
import org.cs3.prolog.connector.cterm.CTerm;
import org.cs3.prolog.connector.cterm.CTermUtil;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.AsyncPrologSessionEvent;
import org.cs3.prolog.connector.session.AsyncPrologSessionListener;
import org.cs3.prolog.connector.session.PrologSession;

public class AsyncSocketSessionTest extends TestCase {

	private PrologProcess process;

	private Recorder rec;

	private AsyncPrologSession session;

	@Override
	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		process = Connector.newUninitializedPrologProcess();

		process.start();
		rec = new Recorder();
		session = process.getAsyncSession();
		session.addBatchListener(rec);
	}

	@Override
	protected void tearDown() throws Exception {
		process.stop();
	}

	class Record {
		String method;

		AsyncPrologSessionEvent event;

		public Record(String method, AsyncPrologSessionEvent event) {
			this.method = method;
			this.event = event;

		}

		public boolean isSyntaxError() {
			// error(syntax_error(cannot_start_term), stream($stream(_), 9, 0,
			// 116))
			CTerm msg = CTermUtil.createCTerm(event.message);
			if (!(msg instanceof CCompound)) {
				return false;
			}
			CCompound c = (CCompound) msg;
			if (c.getArity() != 2 || !c.getFunctorValue().equals("error")) {
				return false;
			}
			CTerm arg = c.getArgument(0);
			if (arg.getArity() != 1
					|| !arg.getFunctorValue().equals("syntax_error")) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			StringBuffer sb = new StringBuffer();
			sb.append(method);
			sb.append('(');
			if (event.ticket instanceof String) {
				sb.append(event.ticket == null ? "null" : event.ticket
						.toString());
			} else {
				sb.append(event.ticket == null ? "null" : "dummy");
			}
			sb.append(',');
			sb.append(event.message == null ? "null" : hideStreamHandles(
					event.message, "$stream(_)"));
			sb.append(',');
			sb.append(event.getBindings() == null ? "null" : "("
					+ Util.prettyPrint(event.getBindings()) + ")");
			sb.append(')');
			return sb.toString();
		}

		private String hideStreamHandles(String string, String replace) {
			int i = -1;
			String search = "$stream(";
			StringBuffer sb = new StringBuffer();
			while ((i = string.indexOf(search, 0)) >= 0) {
				sb.append(string.substring(0, i));
				sb.append(replace);
				int j = string.indexOf(')', i + search.length());
				string = string.substring(j + 1);
			}
			sb.append(string);
			return sb.toString();

		}
	}
	
	
	class Recorder implements AsyncPrologSessionListener {
		public void clear() {
			records.clear();
		}

		public synchronized Record last() {
			return records.lastElement();
		}

		public Record get(int i) {
			return records.get(i);
		}

		@Override
		public String toString() {
			StringBuffer sb = new StringBuffer();
			boolean first = true;
			for (Iterator<Record> it = records.iterator(); it.hasNext();) {
				Record r = it.next();
				if (!first) {
					sb.append(", ");
				}
				sb.append(r.toString());
				first = false;
			}
			return sb.toString();
		}

		Vector<Record> records = new Vector<Record>();

		@Override
		public synchronized void joinComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("joinComplete", e));
			notifyAll();
		}

		@Override
		public synchronized void abortComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("abortComplete", e));
			notifyAll();
		}

		@Override
		public synchronized void goalSucceeded(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSucceeded", e));
			notifyAll();
		}

		@Override
		public synchronized void goalFailed(AsyncPrologSessionEvent e) {
			records.add(new Record("goalFailed", e));
			notifyAll();
		}

		@Override
		public synchronized void goalRaisedException(AsyncPrologSessionEvent e) {
			records.add(new Record("goalRaisedException", e));
			notifyAll();
		}

		@Override
		public synchronized void goalHasSolution(AsyncPrologSessionEvent e) {
			records.add(new Record("goalHasSolution", e));
			notifyAll();
		}

		@Override
		public synchronized void goalSkipped(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSkipped", e));
			notifyAll();
		}

		@Override
		public synchronized void goalCut(AsyncPrologSessionEvent e) {
			records.add(new Record("goalCut", e));
			notifyAll();
		}

		@Override
		public synchronized void batchComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("batchComplete", e));
			notifyAll();
		}

		public int size() {
			return records.size();
		}

		@Override
		public void batchError(AsyncPrologSessionEvent e) {}

	}

	public void test_syntaxError() throws Throwable {
		session.queryOnce("3", "member(a,[a,b,c)");
		session.dispose();
		assertTrue(rec.get(0).isSyntaxError());
	}

	public void test_syntaxError2() throws Throwable {
		session.queryOnce("1", "member(a,[a,b,c)");
		session.queryOnce("2", "member(a,[a,b,c)");
		session.dispose();
		assertEquals(3, rec.size());
		assertTrue(rec.get(0).isSyntaxError());
	}

	public void test_queryOnce_failure01() throws Throwable {
		session.queryOnce("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalFailed(4,null,null)", rec.get(0).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(1).toString());
		assertEquals(2, rec.size());
	}

	public void test_queryOnce_sequence01() throws Throwable {
		// PrologSession session=process.getSession();
		session.queryOnce("1", "member(A,[a,b,c])");
		session.queryOnce("2", "member(a,[a,b,c])");
		session.queryOnce("3", "member(a,[a,b,c)");
		session.queryOnce("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a))", rec.get(0).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(1).toString());
		assertEquals("goalHasSolution(2,null,())", rec.get(2).toString());
		assertEquals("goalSucceeded(2,null,null)", rec.get(3).toString());
		assertTrue(rec.get(4).isSyntaxError());
		assertEquals("goalFailed(4,null,null)", rec.get(5).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(6).toString());
	}

	public void test_queryAll_sequence01() throws Throwable {
		// PrologSession session=process.getSession();
		session.queryAll("1", "member(A,[a,b,c])");
		session.queryAll("2", "member(a,[a,b,c])");
		session.queryAll("3", "member(a,[a,b,c)");
		session.queryAll("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a))", rec.get(0).toString());
		assertEquals("goalHasSolution(1,null,(A-->b))", rec.get(1).toString());
		assertEquals("goalHasSolution(1,null,(A-->c))", rec.get(2).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(3).toString());
		assertEquals("goalHasSolution(2,null,())", rec.get(4).toString());
		assertEquals("goalSucceeded(2,null,null)", rec.get(5).toString());

		assertTrue(rec.get(6).isSyntaxError());

		assertEquals("goalFailed(4,null,null)", rec.get(7).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(8).toString());
	}

	public void testList() throws Throwable {
		session.queryOnce("1", "A=[0,1,2]");
		session.join();
		Record last = rec.get(0);
		AsyncPrologSessionEvent event = last.event;
		Map<String, Object> bindings = event.getBindings();
		Object object = bindings.get("A");
		assertNotNull(object);
		assertTrue(object instanceof List<?>);
		List<?> l = (List<?>) object;
		assertEquals(3, l.size());
		for (int i = 0; i < 3; i++) {
			Object o = l.get(i);
			assertEquals(("" + i), o);

		}
	}

	public void testPDT287_0() throws Exception {
		

		// atoms should be quoted.
		session.queryOnce("quoted", "atom_codes(A,[123,10])",PrologProcess.NONE);

		// terms should be canonical.
		session.queryOnce("canonical", "A=('B'+'C')",PrologProcess.NONE);

		// lists should be ignored
		session.queryOnce("ignore_lists", "A=[1,2]",PrologProcess.NONE);

		session.join();
		assertEquals("'{\\n'", rec.get(0).event.getBindings().get("A"));
		assertEquals("+('B', 'C')", rec.get(2).event.getBindings().get("A"));
		assertEquals("'.'(1, '.'(2, []))", rec.get(4).event.getBindings().get("A"));

	}

	public void testPDT287_1() throws Exception {
		
		session.queryOnce("unquoted", "atom_codes(A,[123,10])",PrologProcess.UNQUOTE_ATOMS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologProcess.UNQUOTE_ATOMS);
		session.queryOnce("ignore_lists", "A=[1,2]",PrologProcess.UNQUOTE_ATOMS);

		session.join();
		// atoms should be unquoted.
		assertEquals("{\n", rec.get(0).event.getBindings().get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.getBindings().get("A"));

		// lists should be ignored
		assertEquals("'.'(1, '.'(2, []))", rec.get(4).event.getBindings().get("A"));

	}

	public void testPDT291_nil() throws Exception{
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologProcess.PROCESS_LISTS);
		session.join();		
		assertTrue(rec.get(0).event.getBindings().get("A") instanceof List<?>);
		
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologProcess.NONE);
		session.join();
		assertTrue(rec.get(0).event.getBindings().get("A") instanceof String);
		
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologProcess.CTERMS);
		session.join();
		assertTrue(rec.get(0).event.getBindings().get("A") instanceof CEmptyList);
	}
	
	public void testPDT287_2() throws Exception {
		
		session.queryOnce("quoted", "atom_codes(A,[123,10])",PrologProcess.PROCESS_LISTS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologProcess.PROCESS_LISTS);
		session.queryOnce("process_lists", "A=[[1,2],'A']",PrologProcess.PROCESS_LISTS);
		session.join();
		
		// atoms should be quoted.
		assertEquals("'{\\n'", rec.get(0).event.getBindings().get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.getBindings().get("A"));

		// lists should be processed
		Object o = rec.get(4).event.getBindings().get("A");
		assertTrue(o instanceof List<?>);

		// list elements should be processed recursively
		List<?> l = (List<?>) o;
		assertTrue(l.get(0) instanceof List<?>);
		assertEquals("'A'", l.get(1));

	}

	public void testPDT287_3() throws Exception {
		
		session.queryOnce("unquoted", "atom_codes(A,[123,10])",PrologProcess.PROCESS_LISTS
				| PrologProcess.UNQUOTE_ATOMS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologProcess.PROCESS_LISTS
				| PrologProcess.UNQUOTE_ATOMS);
		session.queryOnce("process_lists", "A=[[1,2],'A']",PrologProcess.PROCESS_LISTS
				| PrologProcess.UNQUOTE_ATOMS);
		session.join();
		// atoms should be unquoted.
		assertEquals("{\n", rec.get(0).event.getBindings().get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.getBindings().get("A"));

		// lists should be processed
		Object o = rec.get(4).event.getBindings().get("A");
		assertTrue(o instanceof List<?>);

		// list elements should be processed recursively
		List<?> l = (List<?>) o;
		assertTrue(l.get(0) instanceof List<?>);
		assertEquals("A", l.get(1));

	}

	public void testPDT287_4() throws Exception {
		
		// Everything should be CTerms, no list Processing.
		session.queryOnce("bla", "A=[[1,2],'A']",PrologProcess.CTERMS);
		session.join();
		Object o = rec.get(0).event.getBindings().get("A");
		assertTrue(o instanceof CCompound);

	}

	public void testPDT287_illegal_session() throws Exception {
		// combination of CTERMS and UNQUOTE_ATOMS is illegal.
		try {
			process.getAsyncSession(PrologProcess.CTERMS
					| PrologProcess.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			process.getAsyncSession(PrologProcess.CTERMS
					| PrologProcess.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			process.getAsyncSession(PrologProcess.CTERMS
					| PrologProcess.UNQUOTE_ATOMS
					| PrologProcess.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
	
	public void testPDT287_illegal_query() throws Exception {
		// combination of CTERMS and UNQUOTE_ATOMS is illegal.
		try {
			session.queryOnce("bla","syntax error", PrologProcess.CTERMS
					| PrologProcess.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			session.queryOnce("bla","syntax error", PrologProcess.CTERMS
					| PrologProcess.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			session.queryOnce("bla","syntax error", PrologProcess.CTERMS
					| PrologProcess.UNQUOTE_ATOMS
					| PrologProcess.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
	
	public void test_longAtom() throws Throwable {
		// session.queryOnce("0", "guitracer");
		// session.queryOnce("0", "trace");
		// session.join();
		// rec.clear();
		StringBuffer sb = new StringBuffer();
		sb.append("atom(");
		for (int i = 0; i < 600000; i++) {
			sb.append('a');
		}
		sb.append(")");

		session.queryOnce("1", sb.toString());
		session.join();
		assertEquals("goalHasSolution(1,null,())", rec.get(0).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(1).toString());
		assertEquals("joinComplete(dummy,null,null)", rec.get(2).toString());

	}

	public void test_pending_during_queryall() throws Throwable {
		String alias = session.getProcessorThreadAlias();
		String ticket = "queryall";
		session
				.queryAll(
						ticket,
						"repeat,writeln(waiting),thread_get_message(test(M)),writeln(got(M)),(M==stop,!;true)");
		PrologSession syncSession = process.getSession();
		rec.clear();
		synchronized (rec) {
			syncSession.queryOnce("writeln('i am here'),thread_send_message('"
					+ alias + "',test(1))");
			rec.wait();
		}
		boolean pending1 = session.isPending(ticket);
		synchronized (rec) {
			syncSession.queryOnce("thread_send_message('" + alias
					+ "',test(stop))");
			rec.wait();
		}
		session.join();
		boolean pending2 = session.isPending(ticket);
		assertEquals("not pending during queryall", true, pending1);
		assertEquals("pending after queryall", false, pending2);
	}

	/*
	 * have one query with a blocking io call. queue a second one, which serves
	 * as a dummy. abort the batch. the processor cannot recieve the async abort
	 * request while the first query blocks. unlock the first query. the
	 * processor should now recieve the async abort request, it should skip the
	 * second goal.
	 * 
	 * Note: with the original implementation, cuts where reported after
	 * solutions, so the the first solution would be reported by the session.
	 * The current pifcom implementation checks for cut events first, so first
	 * solution is NOT reported. I figured that this change in semantics would
	 * not be of much relevants in practical applications, and it is much easier
	 * to realize on the server side.
	 */
	public void test_abort01() throws Throwable {
		session.queryOnce("1", "thread_self(Alias)");
		session.join();
		Record r = (Record) rec.records.get(0);
		final String alias = (String) r.event.getBindings().get("Alias");
		session.queryAll("2", "repeat,thread_get_message(test(M))");
		final PrologSession syncSession = process.getSession();

		synchronized (rec) {
			syncSession.queryOnce("thread_send_message('" + alias
					+ "',test(1))");
			rec.wait();
		}

		rec.clear();
		assertFalse(session.isIdle());
		session.queryOnce("3", "should_be_skipped");
		final Object lock = new Object();

		synchronized (lock) {
			Thread thread = new Thread() {
				@Override
				public void run() {

					// we need to make sure that test(2) (see below) is send
					// AFTER
					// the abort call, otherwise, abort will lock up forever.
					Debug.debug("enter 3");
					synchronized (lock) {
						Debug.debug("enter 5");
						try {

							Debug.debug("enter 6");
							syncSession.queryOnce("thread_send_message('"
									+ alias + "',test(2))");
						} catch (PrologException e) {
							Debug.report(e);

						} catch (PrologProcessException e) {
							Debug.report(e);
						}
					}

				}
			};
			Debug.debug("enter 0");
			thread.start();
			Debug.debug("enter 1");
			session.abort(lock);
			Debug.debug("enter 7");
		}

		session.dispose();
		assertEquals("goalCut(2,null,null), " + "goalSkipped(3,null,null), "
				+ "abortComplete(dummy,null,null), "
				+ "batchComplete(null,null,null)", rec.toString());

	}

	public void test_abort02() throws Exception {
		session.abort();
		session.dispose();

	}

	

	public void test_manyAsyncSessions() throws Throwable {
		int N = 30;
		AsyncPrologSession[] sessions = new AsyncPrologSession[N];
		for (int i = 0; i < N; i++) {
			sessions[i] = process.getAsyncSession();
		}
		try {
			process.getAsyncSession();

		} catch (PrologProcessException e) {
			e.printStackTrace();
		}
		for (int i = 0; i < N; i++) {
			sessions[i].dispose();
		}
	}
}


