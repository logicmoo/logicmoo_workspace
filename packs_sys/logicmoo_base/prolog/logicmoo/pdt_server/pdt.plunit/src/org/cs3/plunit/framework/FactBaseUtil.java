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

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;

import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.plunit.common.ITestConstants;

public class FactBaseUtil {
	
	private static final String CULTIVATE_BASIC = "cultivate.basic";
	private static final String CULTIVATE_MUSEUM = "cultivate.museum";
	private static final String PROLOG_LOAD_PL = "prolog" + File.separator + "load.pl";
	private static final String JT_FactbasePath = ITestConstants.FACTBASE_FOLDER + File.separator +"jt.qlf";
	
	private File projectPath;
	private File workspacePath;
	private File testProjectPath;
	
	private Class<?> testClass;
	
	//TODO this is a malicious coupling against a configurable eclipse project property!
	public FactBaseUtil(Class<?> testClass){
		this.testClass = testClass;
		
		String projectUnderTestPath  = PathUtil.getProjectPath(testClass);
	    projectPath = new File(projectUnderTestPath);
		testProjectPath = new File(projectPath.getParent()+File.separator+projectPath.getName()+".test");
		workspacePath = projectPath.getParentFile();
		
	}
	
	public void cleanUp(){
		testClass = null;
	}
	
	public void consult(URI fileName) throws PrologInterfaceException {
		if(fileName!=null){
			consult(new File(fileName));
		}else{
			consult((File)null);
		}
	}
	
	public void consult(File file) throws PrologInterfaceException {
		PrologFacade.consult(file);
	}
	
	public void consultResource(String... names) throws Exception {
		for (String name: names) {			
			String filePath = PathUtil.getBinPath(testClass) + "resources/" + name;
			consult(new File(filePath));
		}
	}

	public void useTestData(String... names) throws Exception {
		for (String factbaseName: names) {			
			consult(getFactbaseURI(factbaseName));
		}
	}

	public void initializeProlog() throws PrologInterfaceException,
			FileNotFoundException, IOException, Exception {
		// PrologFacade.restart();
		// TODO TR
	}
	

	
	
	
	

	public void consultMuseum() throws Exception {
		consultPluginPl(CULTIVATE_MUSEUM);
	}

	public void consultPluginPl(String pluginName) throws Exception {
		File cv = new File(getWorkspcaePath()
				+ File.separator+pluginName
				+ File.separator+"prolog"
				+ File.separator+"load.pl");
		
		if(cv.exists()){
			consult(cv);
		}else{
			//TODO: usefull?
			System.out.println("Warning: Plug-in "+pluginName+" pl wasn't consulted.");
		}
	}
	
	public String getWorkspcaePath(){
		return workspacePath.getAbsolutePath();
	}

	private String buildProjectPathWithLoadPl(String projectName) {
		return workspacePath.getAbsolutePath() + File.separator+ projectName+ File.separator+ PROLOG_LOAD_PL;
	}
	

	
	private URI  getFactbaseURI(String factbaseName) throws Exception {
		URI factbaseURI = tryPlainFactbasePath(factbaseName);
		if(factbaseURI != null)
	    	return factbaseURI;
		
		
	    factbaseURI = tryMavenStyleFactbasePath(factbaseName);
		if(factbaseURI != null)
	    	return factbaseURI;
		
		
		
		String path = System.getProperty("user.dir")+File.separator+ITestConstants.FACTBASE_FOLDER+File.separator+factbaseName+".qlf";
		File file = new File(path);
		if(!file.exists())
			 fail("Testdata factbase dosn't exist: "+ factbaseName + ".qlf (URL was "+file.toString()+")");
		
		return file.toURI();
	}

	private URI tryPlainFactbasePath(String factbaseName)	throws Exception{
		String factbasePath = testProjectPath.toString()+File.separator+ITestConstants.FACTBASE_FOLDER + File.separator + factbaseName + ".qlf";
		File file = new File(factbasePath);
		if(file.exists()) {
			return file.toURI();
		}
		return null;
	}


	private URI tryMavenStyleFactbasePath(String factbaseName)	throws Exception {
		String factbasePath = PathUtil.getPath(testClass) + "src/test/resources/" + ITestConstants.FACTBASE_FOLDER + File.separator + factbaseName + ".qlf";
		File file = new File(factbasePath);
		if(file.exists()) {
			return file.toURI();
		}
		return null;
	}


}


