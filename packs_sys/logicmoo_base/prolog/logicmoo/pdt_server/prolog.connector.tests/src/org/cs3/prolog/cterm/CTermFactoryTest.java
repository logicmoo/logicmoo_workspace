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

package org.cs3.prolog.cterm;

import junit.framework.TestCase;

import org.cs3.prolog.connector.cterm.CAtom;
import org.cs3.prolog.connector.cterm.CCompound;
import org.cs3.prolog.connector.cterm.CInteger;
import org.cs3.prolog.connector.cterm.CString;
import org.cs3.prolog.connector.cterm.CTerm;
import org.cs3.prolog.connector.cterm.CTermFactory;


public class CTermFactoryTest extends TestCase {
	
	@Override
	protected void setUp() throws Exception {
		;
	}
	public void testUnquotedAtom00() throws Throwable {
		CTerm term = CTermFactory.createCTerm("hola");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","hola",atom.getFunctorValue());
		assertEquals("functor image","hola",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	public void testUnquotedAtom01() throws Throwable {
		CTerm term = CTermFactory.createCTerm("hol_a");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","hol_a",atom.getFunctorValue());
		assertEquals("functor image","hol_a",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testUnquotedAtom02() throws Throwable {
		CTerm term = CTermFactory.createCTerm("holT0_a");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","holT0_a",atom.getFunctorValue());
		assertEquals("functor image","holT0_a",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testUnquotedAtom03() throws Throwable {
		try{
			CTermFactory.createCTerm("23skido?");
			fail("atoms are not allowed to start with digits. there should be an exception!");
		}catch(Exception pe){			
			return;
		}
				
	}
	public void testUnquotedAtom04() throws Throwable {
		CTerm term = CTermFactory.createCTerm("s�ben");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","s�ben",atom.getFunctorValue());
		assertEquals("functor image","s�ben",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testQuotedAtom01() throws Throwable{
		CTerm term = CTermFactory.createCTerm("'Ecce, Corinna venit.'");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","Ecce, Corinna venit.",atom.getFunctorValue());
		assertEquals("functor image","'Ecce, Corinna venit.'",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());
	}
	
	public void testQuotedAtom02() throws Throwable{
		CTerm term = CTermFactory.createCTerm("'Ecce, \\'Corinna\\' venit.'");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor image","'Ecce, \\'Corinna\\' venit.'",atom.getFunctorImage());
		assertEquals("functor value","Ecce, 'Corinna' venit.",atom.getFunctorValue());
		
		assertEquals("arity",0,atom.getArity());
	}
	
	
	public void testQuotedString01() throws Throwable{
		CTerm term = CTermFactory.createCTerm("\"Ecce, \\\"Corinna\\\" venit.\"");		
		assertTrue("type is "+term.getClass().getName(),term instanceof CString);
		CString string= (CString)term;
		assertEquals("functor image","\"Ecce, \\\"Corinna\\\" venit.\"",string.getFunctorImage());
		assertEquals("functor value","Ecce, \"Corinna\" venit.",string.getFunctorValue());
		
		assertEquals("arity",0,string.getArity());
	}
	
	
	public void testInteger01() throws Throwable{
		CTerm term = CTermFactory.createCTerm("42");		
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;
		assertEquals("functor image","42",integer.getFunctorImage());
		assertEquals("functor value","42",integer.getFunctorValue());
		assertEquals(42,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testInteger02() throws Throwable{
		CCompound c = (CCompound) CTermFactory.createCTerm("worked(1)");
		CTerm term = c.getArgument(0);
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;		
		assertEquals(1,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testInteger03() throws Throwable{
		CCompound c = (CCompound) CTermFactory.createCTerm("worked(23)");
		CTerm term = c.getArgument(0);
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;		
		assertEquals(23,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testCompound01() throws Throwable{
		CTerm term = CTermFactory.createCTerm("aterm(annos,t)");		
		assertTrue("type: "+term.getClass().getName(),term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value","aterm",aterm.getFunctorValue());
		assertEquals("functor image","aterm",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","annos",annos.getFunctorValue());
		assertEquals("annos functor image","annos",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","t",t.getFunctorValue());
		assertEquals("t functor image","t",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
	
	public void testCompound02() throws Throwable{
		CTerm term = CTermFactory.createCTerm(";(a,b)");		
		assertTrue("type",term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value",";",aterm.getFunctorValue());
		assertEquals("functor image",";",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","a",annos.getFunctorValue());
		assertEquals("annos functor image","a",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","b",t.getFunctorValue());
		assertEquals("t functor image","b",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
	public void testCut00() throws Throwable{
		CTerm term = CTermFactory.createCTerm("','(!,b)");		
		assertTrue("type",term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value",",",aterm.getFunctorValue());
		assertEquals("functor image","','",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","!",annos.getFunctorValue());
		assertEquals("annos functor image","!",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","b",t.getFunctorValue());
		assertEquals("t functor image","b",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
}


