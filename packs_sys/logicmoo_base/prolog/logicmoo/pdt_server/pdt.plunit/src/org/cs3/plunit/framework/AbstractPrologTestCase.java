/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.plunit.framework;


import static org.cs3.plunit.matcher.PrologResultsInAnyOrder.has;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.Before;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;

@SuppressWarnings("rawtypes")
public abstract class AbstractPrologTestCase {
	
	protected static boolean initPrologBeforEveryTest = true;
	
	protected static final String any="_";
	protected static final int UNKNOWN_LENGTH = -1; 
	private List<Map<String, Object>> lastResult;
	private List<HashMap> expectedResults = new ArrayList<HashMap>();

	protected static FactBaseUtil fba;
	
	public AbstractPrologTestCase() {
		fba = new FactBaseUtil(this.getClass());
	}
	
	@Before
	public void initializeProlog() throws Exception {
		initializationBeforeTest();
	}
	
	/**
	 * Override to avoid prolog initialization before every test.
	 * @throws Exception  
	 */
	protected void initializationBeforeTest() throws Exception {
		fba.initializeProlog();
	}

	@AfterClass
	public static void cleanupTemporaryFiles() throws Exception {
		if(fba!=null)
			fba.cleanUp();
		PrologFacade.removeTempServerDirectory();
	}
	
	protected static void consultJTFacts() throws Exception {
//		fba.consultJTFacts();
	}

	protected static void useTestData(String... names) throws Exception {
		fba.useTestData(names);
	}
	
	protected static void useMuseum(String... names) throws Exception {
		fba.useTestData(names);
	}
	
	protected static void consult(URI fileName) throws Exception {
		fba.consult(fileName);
	}
	
	public void consultResource(String... names) throws Exception {
		fba.consultResource(names);
	}
	

	
	protected static List<List<String>> allResultsOf(String functor, String... arguments) throws Exception {
		return PrologFacade.allResultsOf(functor, arguments);
	}

	/**
	 * Uses prolog "query once" and does not give more than exactly one answer
	 * @param functor
	 * @param arguments
	 * @return parsed collection of variable unifications
	 * @throws Exception
	 */
	protected static List<String> resultOf(String functor, String... arguments) throws Exception{
		return PrologFacade.resultOf(functor, arguments);
	}
	
	protected static boolean call(String predicate) throws PrologInterfaceException {
		return PrologFacade.queryOnce(predicate) != null;
	}
	
	protected static List<List<String>> metric(String name) throws Exception {
		return metric(name, null);
	}
	
	protected static List<List<String>> metric(String name, String element) throws Exception {
		if(element == null){
			element="Element";
		}
		return allResultsOf("metric", name,element,"Value");
	}

	public static String predicate(String functor, String... arguments){
		return PrologFacade.predicate(functor, arguments);
	}
	
	protected static String idOf(String name)throws PrologInterfaceException{
		List queryResult = PrologFacade.queryAll(predicate("id_of_named_element", "'"+name+"'","_","ID"));
		if (queryResult.size() > 0) {
			String id = (String)((Map) queryResult.get(0)).get("ID");
			assertThat("ID should not be null",id,Matchers.notNullValue());
			
			return id;
		}
		fail("ID not found for '" + name + "'");
		return null;
	}

	protected static String nameOf(String id)throws PrologInterfaceException{
		List queryResult = PrologFacade.queryAll(predicate("name_of_element", id,"NAME"));
		String name = null;
		if (queryResult.size() > 0) {
			name = (String)((Map) queryResult.get(0)).get("NAME");
		}
		if(name==null){
			name = id;
		}
		return name;
	}
	
	/**
	 * Returns the filename in the TestData's source folder
	 * @param filenameInSourcefolder
	 * @param sourceFolderName
	 * @return
	 * @throws PrologException
	 * @throws PrologProcessException
	 */
	protected static String fullFilename(String filenameInSourcefolder) throws PrologException, PrologInterfaceException {
		return fullFilename(filenameInSourcefolder,null);
	}
	
	private static String fullFilename(String filenameInSourcefolder, String sourceFolderName) throws PrologException, PrologInterfaceException {
		String result = "/";
		List queryResult = PrologFacade.queryAll("projectS(_, ProjectName, _, _, _)");
		if (queryResult.size() == 0) 
			fail("Project not found");
		
		result += ((Map) queryResult.get(0)).get("ProjectName");
		
		result += "/";
		
		if (sourceFolderName == null) {
			queryResult = PrologFacade.queryAll("sourceFolderS(_, _, SourceFolder)");
			if (queryResult.size() == 0) 
				fail("SourceFolder not found");
			
			result += ((Map) queryResult.get(0)).get("SourceFolder");
			
		} else {
			result += sourceFolderName;
		}
		
		if (filenameInSourcefolder == null)
			return result;
		
		if (! filenameInSourcefolder.startsWith("/"))
			result += "/";
		
		result += filenameInSourcefolder;
		
		return result;
	}
	
	public static String idOfSourceLocation(String fullFilename, int offset) throws PrologException, PrologInterfaceException {
		return idOfSourceLocation(fullFilename, offset, UNKNOWN_LENGTH);
	}
	
	/**
	 * Returns the ID of a specific source location.
	 * if length is negative, it is 
	 * @param fullFilename
	 * @param offset
	 * @param length
	 * @return
	 * @throws PrologProcessException 
	 * @throws PrologException 
	 */
	public static String idOfSourceLocation(String fullFilename, int offset, int length) throws PrologException, PrologInterfaceException {
		
		List queryResult = PrologFacade.queryAll("find_source_location(ID,'" + fullFilename +"',"+offset+","+(length < 0 ? "_" : ""+length ) + ")");
		
		if (queryResult.size() == 0)
			fail("ID not found");
		
		String id = (String)((Map) queryResult.get(0)).get("ID");
		assertThat("ID should not be null",id,Matchers.notNullValue());
		
		return id;
		
	}

	@Factory
	public static <T> Matcher<List<List<String>>> hasValue(Object o){
		return has(values(o));
	}

	@Factory
	public static <T> Matcher<List<List<String>>> hasValues(Object... o){
		return has(values(o));
	}

	public static String[] values(Object... objects){
		String[] strings=new String[objects.length];
		for(int i=0;i<objects.length;i++){
			strings[i]=objects[i].toString();
		}
		return strings;
	}
	
	public static Iterable<String> values(Iterable<Object> objects){
		Iterable<String> strings = Iterables.transform(objects, new Function<Object, String>(){
			@Override
			public String apply(Object from){
				return from.toString();
			}
		});
		return strings;
	}

	
	
	/**
	 * 
	 * This method can be used for debug to inspect the results of a prolog query
	 * @deprecated Works only with the old, yet deprecated methods.
	 */
	@Deprecated
	public void dumpResults() {
		String[] resultKeys = null;
		for (int j = 0; j < lastResult.size(); j++) {
			Map results = (HashMap) lastResult.get(j);
			
			// Write header
			if (resultKeys == null) {
				resultKeys = (String[]) results.keySet().toArray(new String[0]);
				
				for (String key: resultKeys) {
					System.out.print(key + "\t");
				}
				System.out.println();
				for (String key: resultKeys) {
					for (int i = 0; i < key.length(); i++)
						System.out.print("-");
					System.out.print("\t");
				}
				System.out.println();
			}
			
			for (String key: resultKeys) {
				System.out.print(results.get(key) + "\t");
			}
			System.out.println();				
		}
	}
	
	/**
	 * @deprecated by allResultsOf(query, args)
	 */
	@Deprecated
	protected List<Map<String, Object>> callPredicate(String query) throws PrologInterfaceException {
		lastResult = PrologFacade.queryAll(query);
		return lastResult;
	}

	/**
	 * *Deprecated*: Derived Facts are already included in the persistent factbase.
	 * @throws IOException
	 * @throws PrologProcessException
	 */
	@Deprecated
	public void createDerivedFacts() throws IOException, PrologInterfaceException {
		System.err.println("createDerivedFacts() is deprecated.\nDerived Facts are already included in the persited factbase.");
	}

	
	@Deprecated
	protected void assertResultSize(int size) {
		assertEquals(lastResult.size(),size);
	}
	

	@Deprecated
	protected void assertPredicateReturnsTrue(String query) throws PrologInterfaceException {
		callPredicate(query);
		assertThat("Predicate returned no results",lastResult.size(),is(greaterThan(0)));
	}

	
	@Deprecated
	protected void assertPredicateReturnsFalse(String query) throws PrologInterfaceException {
		callPredicate(query);
		assertThat("Predicate should not return results",lastResult,hasSize(0));
	}
	
	@Deprecated
	public void setExpectedResults(String key, String... values) {
		for (int i = 0; i < values.length; i++) {
			if (expectedResults.size() <= i) {
				expectedResults.add(new HashMap());
			}
			expectedResults.get(i).put(key, values[i]);
		}
	}

	@Deprecated
	public void assertExpectedResultsExist() {
		assertThat("Call setExpectedResults before asserting",expectedResults,hasSize(greaterThan(0)));
		assertThat("No results found",lastResult,hasSize(greaterThan(0)));

		
		for (int i = 0; i < expectedResults.size(); i++) {
			Map expected = expectedResults.get(i);
			
			boolean match = true;
			for (int j = 0; j < lastResult.size(); j++) {
				Map real = (HashMap) lastResult.get(j);
				
				match = true;
				for (int k = 0; k < expected.keySet().size(); k++) {
					String key = (String) expected.keySet().toArray()[k];
					if (!expected.get(key).equals(real.get(key))) {
						match = false;
						break;
					}
				}
				if (match == true) {
					break;
				}
			}
			assertThat("Couldn't find: " + expected.toString() + " in " + lastResult.toString(),match,is(true));
		}
	}
	
	@Deprecated
	public void assertExpectedResultsDoNotExist(){
		assertThat("Call setExpectedResults before asserting",expectedResults,hasSize(greaterThan(0)));
		assertThat("No results found",lastResult,hasSize(greaterThan(0)));
		
		for (int i = 0; i < expectedResults.size(); i++) {
			Map expected = expectedResults.get(i);
			
			boolean match = true;
			for (int j = 0; j < lastResult.size(); j++) {
				Map real = (HashMap) lastResult.get(j);
				
				match = true;
				for (int k = 0; k < expected.keySet().size(); k++) {
					String key = (String) expected.keySet().toArray()[k];
					if (expected.get(key).equals(real.get(key))) {
						match = false;
						break;
					}
				}
				if (match == true) {
					break;
				}
			}
			assertThat("Found Result: " + expected.toString() + " in " + lastResult.toString(),match,is(false));
			
		}
	}
	
	/**
	 * This method can be used while defining a test before refactoring
	 */
	@Deprecated
	public void dumpExpectedResultSettings() {
		Map<String,Object[]> resultData = new HashMap<String,Object[]>();
		String[] resultKeys = null;
		for (int j = 0; j < lastResult.size(); j++) {
			Map results = (HashMap) lastResult.get(j);
			
			// remember header
			if (resultKeys == null) {
				resultKeys = (String[]) results.keySet().toArray(new String[0]);
				
				for (String key: resultKeys) {
					resultData.put(key, new Object[lastResult.size()]);
				}
			
			}
			
			for (String key: resultKeys) {
				resultData.get(key)[j] = results.get(key);
			}				
		}

		assertThat(resultKeys,Matchers.notNullValue());
		for (String key: resultKeys) {
			System.out.print("setExpectedResults(\"" + key + "\"");
			for (int j = 0; j < lastResult.size(); j++) {
				System.out.print(",\""+ resultData.get(key)[j] + "\"");
			}
			System.out.println(");");			
		}
	}

	/**
	 * *deprecated* use idOf(String) instead
	 */
	@Deprecated
	public String getIdOfFullQualifiedName(String name) throws Exception {
		List queryResult = PrologFacade.queryAll("globalIds('"+name+"', ID)");
		if (queryResult.size() > 0) {
			String id = (String)((Map) queryResult.get(0)).get("ID");
			assertThat("ID should not be null",id,Matchers.notNullValue());
			return id;
		}
		fail("ID not found for '" + name + "'");
		return null;
		
	}
	
	/**
	 * *deprecated* use idOf(String) instead
	 */
	@Deprecated
	public String getIDOfComplexNamedElement(String FQN) throws Exception{
		return idOf(FQN);
	}
	
	/**
	 * 
	 * **Deprecated** use the hamcrest api instead.
	 *
	 */
	@Deprecated
	protected class ContextualPredicate {

		String predicate;
		String contextName = "Context";
		List<String> variablesToCheck;
		
		public ContextualPredicate(String predicate) {
			this.predicate = predicate;
			variablesToCheck = new ArrayList<String>();
		}

		public String getContextName() {
			return contextName;
		}
		public void setContextName(String contextName) {
			this.contextName = contextName;
		}
		
		public void setVariablesToCheck(String... names) {
			variablesToCheck.clear();
			for (String name: names) {
				variablesToCheck.add(name);
			}
		}

		public void assertResult(String escapedContextValue, String... values) throws PrologInterfaceException {
			assertThat(escapedContextValue,Matchers.notNullValue());
			
			callPredicate(contextName + " = " + escapedContextValue + ", " + predicate);
			
			int i = 0;
			for (String value: values) {
				assertThat("more variables returned than in checklist",i,is(Matchers.lessThan(variablesToCheck.size())));
						setExpectedResults(variablesToCheck.get(i), value);
				i++;
			}
			assertExpectedResultsExist();
		}
		
		public void assertResultNotIncluding(String escapedContextValue, String... values) throws PrologInterfaceException {
			assertThat(escapedContextValue,Matchers.notNullValue());
			
			callPredicate(contextName + " = " + escapedContextValue + ", " + predicate);
			
			int i = 0;
			for (String value: values) {
				assertThat("more variables returned than in checklist",i,is(Matchers.lessThan(variablesToCheck.size())));
				setExpectedResults(variablesToCheck.get(i), value);
				i++;
			}
			assertExpectedResultsDoNotExist();
			
		}
		public void assertResultSize(String escapedContextValue, int size) throws PrologInterfaceException {
			callPredicate(contextName + " = " + escapedContextValue + ", " + predicate);
			assertThat(lastResult,hasSize(size));
		}

		public void dumpResult() throws PrologInterfaceException {
			
			callPredicate(predicate);
			
			dumpResults();
		}
		
		
		public void dumpResult(String escapedContextValue) throws PrologInterfaceException {
			assertThat(escapedContextValue,Matchers.notNullValue());
			
			
			callPredicate(contextName + " = " + escapedContextValue + ", " + predicate);
			
			dumpResults();
		}
		
		//Helpers to write tests
		public void dumpExpectedResultSettings() throws PrologInterfaceException {	
			dumpExpectedResultSettings(null); 
					
		}
		public void dumpExpectedResultSettings(String escapedContextValue) throws PrologInterfaceException {
			if (escapedContextValue == null) {
				callPredicate(predicate);
			} else {
				callPredicate(contextName + " = " + escapedContextValue + ", " + predicate);
			}
			  
			String[] resultKeys = null;
			String currentContextName = "";
			int resultsForCurrentContext = 0;
			for (int j = 0; j < lastResult.size(); j++) {
				Map results = (HashMap) lastResult.get(j);
				
				// remember context
				if (currentContextName == null) {
					currentContextName = (String) results.get(contextName);
				} else if (! currentContextName.equals(results.get(contextName))) {
					resultsForCurrentContext++;				
					System.out.println("predicate.assertResultSize(\"" + currentContextName +"\"," + resultsForCurrentContext + ");");	
					currentContextName = (String) results.get(contextName);					
					resultsForCurrentContext = 0;
				} else {
					resultsForCurrentContext++;
				}
				
				// remember header
				if (resultKeys == null) {
					resultKeys = (String[]) results.keySet().toArray(new String[0]);
					
					System.out.print("predicate.setVariablesToCheck(");
					boolean first = true;
					for (String key: resultKeys) {
						if (contextName.equals(key))
							continue;
						
						if (first)
							first = false;
						else
							System.out.print(",");
						
						System.out.print("\""+key+"\"");
					}
					System.out.println(");");
				
				}
				
				System.out.print("predicate.assertResult(\"" + results.get(contextName)+"\"");
				for (String key: resultKeys) {
					if (contextName.equals(key))
						continue;
					System.out.print(",\""+ results.get(key) + "\"");

				}				
				System.out.println(");");	
				
				
			}
			if (currentContextName != null)
			   System.out.println("predicate.assertResultSize(\"" + currentContextName +"\"," + (resultsForCurrentContext +1) + ");");
			
		}
	}
	
}


