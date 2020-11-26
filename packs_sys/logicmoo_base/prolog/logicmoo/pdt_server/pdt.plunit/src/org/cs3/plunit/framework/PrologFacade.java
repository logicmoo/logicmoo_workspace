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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class PrologFacade {

	private static String PATH_TO_CONSULT_SERVER_PL;
	private static PrologInterface process;
	private static File targetConsultServerDir;
	static {
		
		try {
			File portFile = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_console_active_port.txt");
			BufferedReader reader = new BufferedReader(new FileReader(portFile));
			int port = Integer.parseInt(reader.readLine());
			reader.close();
			process = AbstractPrologInterface.newInstance();
			process.setStandAloneServer(true);
			process.setHost("localhost");
//			process.setExecutable(Util.guessExecutableName());
//			process.setEnvironment(Util.guessEnvironmentVariables());
			process.setTimeout("15000");
			process.getClass().getMethod("setPort",int.class).invoke(process,port);
			process.getSession().dispose();
			
		} catch (Exception e) {
			//e.printStackTrace();
			System.out.println("Could not connect to running PDT Console.");
		}
	}





	
	public static boolean consult(File file) throws PrologInterfaceException {
		if(file!=null && file.exists()){
			String prologFileName = Util.prologFileName(file);
//			System.out.println("Consult: " + prologFileName);
			return queryOnce("consult('" + prologFileName + "')") != null;
		}else{
			//TODO: warning nessecary? use slf4j
			if(file!=null)
				System.out.println("File '"+file+"' wasn't consulted!");
			else
				System.out.println("File 'null' wasn't consulted!");
			return false;
		}
	}

	public static List<Map<String, Object>> queryAll(String query) throws PrologException,
			PrologInterfaceException {
		if(session == null)
			session = process.getSession(PrologInterface.LEGACY);
		List<Map<String, Object>> results = session.queryAll(query);
//		session.dispose();
		return results;
	}

	static PrologSession session = null;
	public static Map<String, Object> queryOnce(String query)
			throws PrologInterfaceException {
		if(session == null)
			session = process.getSession(PrologInterface.LEGACY);
		Map<String, Object> result = session.queryOnce(query);
//		session.dispose();
		return result;
	}


	public static Map<String, Object> queryOnceNewSession(String query)
			throws PrologInterfaceException {
		PrologSession session = process.getSession(PrologInterface.LEGACY);
		Map<String, Object> result = session.queryOnce(query);
		session.dispose();
		return result;
	}

	
	public static void removeTempServerDirectory() throws Exception{
		if(targetConsultServerDir==null){
			return;
		}
		File[] filelist=targetConsultServerDir.listFiles();
		if(filelist != null){
			for (File file : filelist) {
				file.delete();
			}
		}
		targetConsultServerDir.delete();

		targetConsultServerDir = null;
		PATH_TO_CONSULT_SERVER_PL = null;
	}

	private static boolean hasConsultServerPl(){
		return PATH_TO_CONSULT_SERVER_PL!=null;
	}
	
	/**
	 * Uses prolog "query once" and does not give more than exactly one answer
	 * @param functor
	 * @param arguments
	 * @return parsed collection of variable unifications
	 * @throws Exception
	 */
	public static List<String> resultOf(String functor, String... arguments) throws Exception{
		List<String> variables = new ArrayList<String>();
		for (String argument : arguments) {
			if (isVariable(argument)) {variables.add(argument);};
		}
		
		Map<String,Object> rawResult=PrologFacade.queryOnce(predicate(functor, arguments));

		// dsp, 22.10.10, we represent a Prolog fail as null
		if(rawResult == null) 
			return null;
//		if(rawResult == null) 
//			return new LinkedList<String>();
		
		List<String> line = new LinkedList<String>();
		for (String variable : variables) {
			// jn 2010-09-25: String could be as well List. Should be supported some time.
			line.add(rawResult.get(variable).toString());
		}
			
		return line;
	}
	
	public static List<List<String>> allResultsOf(String functor, String... arguments) throws Exception {
		
		List<String> variables = new ArrayList<String>();
		for (String argument : arguments) {
			if (isVariable(argument)) {variables.add(argument);};
		}
		
		String query = predicate(functor, arguments);
		
		return allResultsOf(query, variables);
	}

	private static List<List<String>> allResultsOf(String query, List<String> variables)
			throws PrologInterfaceException {
		List<List<String>> result = new ArrayList<List<String>>();
		
		List<Map<String, Object>> rawResult = PrologFacade.queryAll(query);
		if (rawResult == null) return null;
		
		for (Map<String, Object> unificationMap : rawResult) {
			List<String> line = new ArrayList<String>();
			for (String variable : variables) {
				// jn 2010-09-25: String could be as well List. Should be supported some time.
				line.add(unificationMap.get(variable).toString());
			}
			result.add(line);
		}
		return result;
	}
	
	public static String predicate(String functor, String... arguments){
		if (arguments.length == 0) return functor;
		String predicate= functor+"(";
		for (String argument : arguments) {
			predicate +=  argument+",";
		}
		predicate= predicate.substring(0,predicate.length()-1);
		return predicate + ")";
	}
	
	private static boolean isVariable(String argument) {
		if (argument == null) return false;
		argument = argument.trim();
		if (argument.isEmpty()) return false;
		char firstCharacter = argument.subSequence(0, 1).charAt(0);
		if (firstCharacter == '_') return argument.length() > 1;
		if (firstCharacter == '\'') return false;
		return Character.isUpperCase(firstCharacter);
	}
}




