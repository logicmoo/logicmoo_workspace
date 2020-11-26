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

package org.cs3.plunit;


import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.plunit.framework.AbstractPrologTestCase;
import org.cs3.plunit.framework.PrologFacade;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;

@RunWith(LabelledParameterized.class)
public class PLUnitTest extends AbstractPrologTestCase{

	protected static final long TEST_TIMEOUT = 10000;
	private String testname;
	private String unit;

	static private String threadId;

	public PLUnitTest(String arg,String unit,String testname,String file, String line) {
		// First argument is used by the TestRunner to add a meaningful testname 
		this.testname = testname;
		this.unit= unit;
	}

	@SuppressWarnings("rawtypes")
	//@Parameters
	@Parameters(name="{2}:{4}")
	public static Collection data() throws Exception{
		
//		prepareTests();

		List<Map<String, Object>>  tests = PrologFacade.queryAll("junitadapter:unit_test(Unit,Test,File,Line)");
	
		if(tests.isEmpty())
			throw new Exception("No testcases found");
		
		Object[][] data=new String[tests.size()][5];
		for(int i=0;i<tests.size();i++){
			String unitname=tests.get(i).get("Unit").toString();
			String testname=tests.get(i).get("Test").toString();
			String file=tests.get(i).get("File").toString();
			String line=tests.get(i).get("Line").toString();
			data[i][0]=unitname+":"+testname;
			data[i][1]=unitname;
			data[i][2]=testname;
			data[i][3]=file;
			data[i][4]=line;
			
		}

		return Arrays.asList(data);
	}

	private Object monitor = new Object();
	private boolean success;
	
	@Test
	public void _() throws Exception{
		success = false;
//		 Map<String, Object> tid = PrologFacade.queryOnce("thread_self(ID)");
//		 threadId = (String)tid.get("ID");
	
		Thread t = new Thread() {
			public void run() {
				try {
					synchronized (monitor) {
						monitor.wait(TEST_TIMEOUT);
						if(success){
							return;
						}
						 PrologFacade.queryOnceNewSession("catch(thread_signal('"+threadId +"', abort),_,fail)");
						 fail("Timeout exceeded: " + TEST_TIMEOUT + " ms, aborted query.");
					}
				} catch (InterruptedException e) {
					e.printStackTrace();
				} catch (PrologInterfaceException e) {
					e.printStackTrace();
				}
			}
		};
		t.start();
		
		Map<String,Object> failed = PrologFacade.queryOnce(
				"forall(" + // ensure all choice points processed
				"run_tests("+unit+":"+testname+")," +
						"true)," +
				"junitadapter:test_failure(Kind,Msg,Line)");
		synchronized (monitor) {
			success = true;
			monitor.notify();
		}
		if(failed != null){
			fail(""+failed.get("Msg"));
		}
		
	}
	@AfterClass
	static public void resetFilesToTest() throws PrologInterfaceException{
		File file = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_current_unit_test_thread.txt");
		file.delete();
		PrologFacade.queryOnce("junitadapter:reset_file_to_test");
	}
	
	@BeforeClass
	static public void storeThreadId() throws PrologInterfaceException{
		 Map<String, Object> tid = PrologFacade.queryOnce("thread_self(ID)");
		 threadId = (String)tid.get("ID");
		 writeCurrentThreadIdToFile(threadId);
		 System.out.println("Thread id:_ " + threadId );
	}
	
	static public void writeCurrentThreadIdToFile(String threadId) {
		try {
			
			File portFile = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_console_active_port.txt");
			BufferedReader reader = new BufferedReader(new FileReader(portFile));
			int port = Integer.parseInt(reader.readLine());
			reader.close();

			File file = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_current_unit_test_thread.txt");
			FileWriter writer = new FileWriter(file,false);
			writer.write(""+port+"\n");
			writer.write(""+threadId+"\n");
			writer.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}


