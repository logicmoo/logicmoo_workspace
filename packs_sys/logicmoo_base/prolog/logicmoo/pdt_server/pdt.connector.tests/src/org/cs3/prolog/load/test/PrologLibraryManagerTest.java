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

package org.cs3.prolog.load.test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.cs3.pdt.connector.load.PrologLibrary;
import org.cs3.pdt.connector.load.PrologLibraryManager;
import org.cs3.prolog.connector.common.Util;

/*
 * each test precedes a diagram spec that you can run through dot to
 * visualize the test graph.
 * 
 */

public class PrologLibraryManagerTest extends TestCase {
	
	/*
	 * For each test the libraries will be added to the known libraries
	 * in the order they appear in libs. After that they will be removed 
	 * in the same order.
	 * (first to last are added than first to last are removed)
	 * 
	 * After each change (add or remove) doTest checks whether the right
	 * libraries are resolved, broken or unresolved. To do this the
	 * given situation is compared to the 3 strings arrays "broken", "resolved" 
	 * and "unresolved" as followes:
	 * 
	 * The first element of the array shows the values for the situation after the
	 * first library was added, the second shows the values for the situation after
	 * the first and second libraries are added. When all are added the next element
	 * shows the situation after the first library was removed, the element after that
	 * shows the situation after the first and second libraries are removed and so on.
	 * The last element shows the situation after all libraries are removed (all empty).
	 * 
	 */
	
	
	/*
	digraph test01 {	
		A->B;
		A->C
	}
	*/

	public void test01() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","BC");
		DummyPrologLibrary b = new DummyPrologLibrary("B");
		DummyPrologLibrary c = new DummyPrologLibrary("C");
		PrologLibrary[] libs ={a,b,c};		
		String[] broken = {"A","A","","","",""};
		String[] unresolved = {"BC","C","","","",""};
		String[] resolved = {"A","AB","ABC","BC","C",""};
		
		doTest(libs,resolved,unresolved,broken);
	}
	

	/*
	digraph test02 {	
		A->B;
		A->B
	}
	*/

	public void test02() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","BB");
		DummyPrologLibrary b = new DummyPrologLibrary("B");		
		PrologLibrary[] libs ={a,b};		
		String[] broken = {"A","","",""};
		String[] unresolved = {"B","","",""};
		String[] resolved = {"A","AB","B",""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	
	/*
	digraph test03 {	
		A->C;
		B->C
	}
	*/	
	public void test03() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","C");
		DummyPrologLibrary b = new DummyPrologLibrary("B", "C");		
		DummyPrologLibrary c = new DummyPrologLibrary("C");
		PrologLibrary[] libs ={a,b,c};		
		String[] broken = {"A","AB","","","",""};
		String[] unresolved = {"C","C","","","",""};
		String[] resolved = {"A","AB","ABC","BC","C",""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	
	/*
	digraph test04 {	
		A->B;
		C->D
	}
	*/
	
	public void test04() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","B");
		DummyPrologLibrary b = new DummyPrologLibrary("B");		
		DummyPrologLibrary c = new DummyPrologLibrary("C","D");
		DummyPrologLibrary d = new DummyPrologLibrary("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","","C","","",
				           "", "", "", ""};
		String[] unresolved = {"B","","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test05 {	
		A->C;
		A->D;
		B->C;
		B->D;
	}
	*/

	
	public void test05() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","CD");
		DummyPrologLibrary b = new DummyPrologLibrary("B","CD");		
		DummyPrologLibrary c = new DummyPrologLibrary("C");
		DummyPrologLibrary d = new DummyPrologLibrary("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","AB","",
				           "", "", "", ""};
		String[] unresolved = {"CD","CD","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	/*
	digraph test06 {	
		A->B;
		A->C;
		B->D;
		C->D;
	}
	*/

	
	public void test06() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","BC");
		DummyPrologLibrary b = new DummyPrologLibrary("B","D");		
		DummyPrologLibrary c = new DummyPrologLibrary("C","D");
		DummyPrologLibrary d = new DummyPrologLibrary("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","ABC","",
				           "", "", "", ""};
		String[] unresolved = {"BC","CD","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	/*
	digraph test07 {	
		A->B;
		A->C;
		D->B;
		D->C;
	}
	*/

	
	public void test07() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","BC");
		DummyPrologLibrary b = new DummyPrologLibrary("B");		
		DummyPrologLibrary c = new DummyPrologLibrary("C");
		DummyPrologLibrary d = new DummyPrologLibrary("D","BC");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","A","","",
				           "", "D", "D", ""};
		String[] unresolved = {"BC","C","","",
				               "","B", "BC", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test08 {	
		A->B;
		B->A;
		A->C;
		C->D;
	}
	*/

	
	public void test08() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","BC");
		DummyPrologLibrary b = new DummyPrologLibrary("B","A");		
		DummyPrologLibrary c = new DummyPrologLibrary("C","D");
		DummyPrologLibrary d = new DummyPrologLibrary("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","A","AC","",
				           "B", "", "", ""};
		String[] unresolved = {"BC","C","D","",
				               "A","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		
		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test09 {	
		B->A;
		A->C;
		D->C;		
	}
	*/	
	public void test09() throws Throwable{
		DummyPrologLibrary a = new DummyPrologLibrary("A","C");
		DummyPrologLibrary b = new DummyPrologLibrary("B","A");		
		DummyPrologLibrary c = new DummyPrologLibrary("C");
		DummyPrologLibrary d = new DummyPrologLibrary("D","C");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","","",
				           "B", "", "D", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				               "BCD","CD", "D", ""};
		String[] unresolved = {"C","C","","",
				             "A","", "C", ""};
		
		doTest(libs,resolved,unresolved,broken);
	}
	
	
	private void doTest(PrologLibrary[] libs, String[] resolved, String[] unresolved, String[] broken) {
		Map<String, PrologLibrary> allLibs = getAllLibs(libs); 
		PrologLibraryManager mgr = new PrologLibraryManager();
		for (int i = 0; i < libs.length; i++) {
			PrologLibrary lib = libs[i];
			String msg = "after adding lib "+lib.getId();
			mgr.addLibrary(lib);
			checkResolved(msg,mgr,resolved[i],allLibs);
			checkUnresolved(msg,mgr,unresolved[i]);
			checkBroken(msg,mgr,broken[i]);	
		}
		for (int i = 0; i < libs.length; i++) {
			PrologLibrary lib = libs[i];
			mgr.removeLibrary(lib);
			String msg = "after removing lib "+lib.getId();
			checkResolved(msg,mgr,resolved[i+libs.length],allLibs);
			checkUnresolved(msg,mgr,unresolved[i+libs.length]);
			checkBroken(msg,mgr,broken[i+libs.length]);	
		}
		
	}

	private Map<String, PrologLibrary> getAllLibs(PrologLibrary[] libs){
		HashMap<String, PrologLibrary> map = new HashMap<String, PrologLibrary>();
		for (int i = 0; i < libs.length; i++) {
			map.put(libs[i].getId(),libs[i]);
		}
		return map;
	}
	
	

	

	private void checkBroken(String msg, PrologLibraryManager mgr, String string) {
		Set<Object> exp=new HashSet<Object>();
		for (int i = 0; i < string.length(); i++) {
			exp.add(String.valueOf(string.charAt(i)));
		}
		Set<String> act = mgr.getBrokenLibraries();
		assertEqualSet(msg+": broken libs do not match",exp,act);
	}

	private void checkUnresolved(String msg, PrologLibraryManager mgr, String string) {
		Set<Object> exp=new HashSet<Object>();
		for (int i = 0; i < string.length(); i++) {
			String key =String.valueOf(string.charAt(i)); 
			exp.add(key);
			assertNull("unresolved key "+key+" is actually resolveable",mgr.resolveLibrary(key));
		}
		Set<String> act = mgr.getUnresolvedDependencies();
		assertEqualSet(msg+": unresolved deps do not match",exp,act);
		
	}

	private void checkResolved(String msg,PrologLibraryManager mgr, String string, Map<String, PrologLibrary> allLibs) {
		HashMap<String, PrologLibrary> exp=new HashMap<String, PrologLibrary>();
		for (int i = 0; i < string.length(); i++) {
			String key = String.valueOf(string.charAt(i));
			exp.put(key,allLibs.get(key));
		}
		Set<String> negExp = new HashSet<String>();
		negExp.addAll(allLibs.keySet());
		negExp.removeAll(exp.keySet());
		Set<String> keys = exp.keySet();
		for (Iterator<String> it = keys.iterator(); it.hasNext();) {
			String key = it.next();
			assertSame(msg+": "+key+" is not resolved to same lib.",exp.get(key),mgr.resolveLibrary(key));
		}
			
		for (Iterator<String> it = negExp.iterator(); it.hasNext();) {
			String key = it.next();
			assertNull(msg+": "+key+" should not be resolvable.",mgr.resolveLibrary(key));
		}
		
	}
	
	private void assertEqualSet(String msg,Set<Object> expUsers, Set<String> actUsers) {
		String es="expected: \n"+Util.prettyPrint(expUsers);
		String as="but was: \n"+Util.prettyPrint(actUsers);			
		assertTrue(msg+": exp !>= act\n"+es+as,expUsers.containsAll(actUsers));
		assertTrue(msg+": act !>= exp\n"+es+as,actUsers.containsAll(expUsers));
	}
}


