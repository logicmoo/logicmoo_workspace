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

package org.cs3.prolog.load.test;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.List;

import junit.framework.ComparisonFailure;
import junit.framework.TestCase;

import org.cs3.pdt.connector.load.FileSearchPathConfigurator;
import org.cs3.pdt.connector.load.PrologLibrary;
import org.cs3.pdt.connector.load.PrologLibraryManager;

public class FileSearchPathConfiguratorTest extends TestCase {
	PrologLibraryManager mgr;
	
	DummyPrologLibrary a = new DummyPrologLibrary("a","");
	DummyPrologLibrary b = new DummyPrologLibrary("b", "a");
	DummyPrologLibrary c = new DummyPrologLibrary("c", "");
	DummyPrologLibrary d = new DummyPrologLibrary("d","ac");
	DummyPrologLibrary e = new DummyPrologLibrary("e","b");
	
	
	@Override
	public void setUp() {
		mgr = new PrologLibraryManager();
	}
	
	
	public void testGetRequiredLibs_NullMng() {
		String[] libs = {"a"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(null, libs);
			fail("an exception should occure");
		}catch(NullPointerException e) {
		}
	}

	
	public void testGetRequiredLibs_NullLibs() {
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, null);
			fail("an exception should occure");
		}catch(NullPointerException e) {
		}
	}
	
	
	public void testGetRequiredLibs_NoLibsNeeded() {
		String[] libs = {"a"};
		mgr.addLibrary(a);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={a};
		arrayEquals("",expected,result);
	}
	
	
	public void testGetRequiredLibs_OneNeededIsThere() {
		String[] libs = {"b"};
		mgr.addLibrary(a);
		mgr.addLibrary(b);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={b,a};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	
	public void testGetRequiredLibs_TwoNeededAreThere() {
		String[] libs = {"d"};
		mgr.addLibrary(a);
		mgr.addLibrary(c);
		mgr.addLibrary(d);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={d,a,c};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	
	public void testGetRequiredLibs_OneNeededDirectlyOneNeededIndirectlyBothAreThere() {
		String[] libs = {"e"};
		mgr.addLibrary(a);
		mgr.addLibrary(b);
		mgr.addLibrary(e);
		PrologLibrary[] result=FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
		PrologLibrary[] expected={e,b,a};
		List<PrologLibrary> resultList = Arrays.asList(result);
		List<PrologLibrary> expectedList = Arrays.asList(expected);
		assertEquals(expectedList.size(),resultList.size());
		assertTrue(resultList.containsAll(expectedList));
	}
	
	
	public void testGetRequiredLibs_LibNotThere() {
		String[] libs = {"a"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id a is unresolved", e.getMessage());
		}
	}

	
	public void testGetRequiredLibs_RequiredLibNotThere() {
		String[] libs = {"b"};
		mgr.addLibrary(b);
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id b is broken", e.getMessage());
		}
	}
	
	
	public void testGetRequiredLibs_WrongLibId() {
		String[] libs = {"x"};
		try {
			FileSearchPathConfigurator.getRequiredLibs(mgr, libs);
			fail("an exception should occure");
		} catch(IllegalArgumentException e) {
			assertEquals("library id x is unresolved", e.getMessage());
		}
	}

	/**
	 * Copied from Junit 4 Assert
	 * 
	 * to avoid the use of assertArrayEquals
	 * 
	 * Asserts that two object arrays are equal. If they are not, an
	 * {@link AssertionError} is thrown with the given message. If
	 * <code>expecteds</code> and <code>actuals</code> are <code>null</code>,
	 * they are considered equal.
	 * 
	 * @param message
	 *            the identifying message for the {@link AssertionError} (<code>null</code>
	 *            okay)
	 * @param expecteds
	 *            Object array or array of arrays (multi-dimensional array) with
	 *            expected values.
	 * @param actuals
	 *            Object array or array of arrays (multi-dimensional array) with
	 *            actual values
	 */

	private static void arrayEquals(String message, Object expecteds,Object actuals)  {
		if (expecteds == actuals)
			return;
		String header= message == null ? "" : message + ": ";
		if (expecteds == null)
			fail(header + "expected array was null");
		if (actuals == null)
			fail(header + "actual array was null");
		int actualsLength= Array.getLength(actuals);
		int expectedsLength= Array.getLength(expecteds);
		if (actualsLength != expectedsLength)
			fail(header + "array lengths differed, expected.length="
					+ expectedsLength + " actual.length=" + actualsLength);

		for (int i= 0; i < expectedsLength; i++) {
			Object expected= Array.get(expecteds, i);
			Object actual= Array.get(actuals, i);
			if (isArray(expected) && isArray(actual)) {
				try {
					arrayEquals(message, expected, actual);
				} catch (ComparisonFailure e) {
					throw e;
				}
			} else
				try {
					assertEquals(expected, actual);
				} catch (AssertionError e) {
					throw new ComparisonFailure(header, expected.toString(), actual.toString());
				}
		}
	}
	
	private static boolean isArray(Object expected) {
		return expected != null && expected.getClass().isArray();
	}

}


