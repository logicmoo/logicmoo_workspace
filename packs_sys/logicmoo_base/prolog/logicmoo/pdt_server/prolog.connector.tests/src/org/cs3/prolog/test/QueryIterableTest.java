/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.prolog.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.internal.session.socket.IterableQuery;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Testclass for the queryIterable(String) method.
 */
public class QueryIterableTest {

	private PrologProcess p;

	@Before
	public void setUp() throws Exception {
		p = Connector.newPrologProcess();
		String q = "numlist(1,100,L), member(X,L), assert(a(X))";
		p.queryAll(q);
		q = "assert(a(1,2)), assert(a(2,3)), assert(a(3,1)), assert((way(X,Y) :- a(X,Y))), assertz((way(X,Y) :- a(X,Z), way(Z,Y)))";
		p.queryAll(q);
	}
	
	@After
	public void tearDown() throws Exception {
		p.stop();
	}
	
	@Test
	public void testNumberOfResults() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("a(X)");
		int i=0;
		while(iter.hasNext()) {
			i++;
			iter.next();
		}
		iter.close();
		assertEquals(100, i);
		session.dispose();
	}
	
	@Test
	public void testBlockingOfOtherQueries() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("a(X)");
		if(iter.hasNext()) {
			iter.next();
		}
		try {
			session.queryOnce("a(X)");
			fail("Query should fail if another query is active.");
		} catch (PrologProcessException e) {
			// there should be an exception
		}
		
		try {
			session.queryAll("a(X)");
			fail("Query should fail if another query is active.");
		} catch (PrologProcessException e) {
			// there should be an exception
		}
		
		try {
			session.queryIterator("a(X)");
			fail("Query should fail if another query is active.");
		} catch (PrologProcessException e) {
			// there should be an exception
		}
		
		iter.close();
		session.dispose();
	}
	
	@Test
	public void testNonBlockingInDifferentSession() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("a(X)");
		if(iter.hasNext()) {
			iter.next();
		}
		
		PrologSession session2 = p.getSession();
		session2.queryOnce("a(X)");
		
		iter.close();
		session.dispose();
	}
	
	@Test
	public void testNonBlockingAfterClosing() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("a(X)");
		if(iter.hasNext()) {
			iter.next();
		}
		iter.close();
		session.queryOnce("a(X)");
		session.dispose();
	}
	
	@Test
	public void testExceptionAfterClosing() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("a(X)");
		if(iter.hasNext()) {
			iter.next();
		}
		iter.close();
		try {
			iter.hasNext();
			fail("hasNext() should fail if iterator was closed.");
		} catch (Exception e) {
			
		}
		try {
			iter.next();
			fail("next() should fail if iterator was closed.");
		} catch (Exception e) {
			
		}
		session.dispose();
	}
	
	@Test
	public void testNeverendingQuery() throws Throwable {
		PrologSession session = p.getSession();
		IterableQuery iter = session.queryIterator("way(A,B)");
		int i=0;
		while(iter.hasNext() && i<100) {
			i++;
			iter.next();
		}
		iter.close();
		assertEquals(100, i);
		session.dispose();
	}
	
	

}
