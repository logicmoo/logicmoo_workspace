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

/*
 */
package org.cs3.prolog.connector.internal.session.socket;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.cterm.CTermUtil;
import org.cs3.prolog.connector.internal.process.AbstractPrologProcess;
import org.cs3.prolog.connector.internal.process.socket.SocketClient;
import org.cs3.prolog.connector.internal.process.socket.SocketCommunicationConstants;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

public class SocketSession implements PrologSession {
	
	private static final String RESULT_LIST = "ResultList";
	private SocketClient client;
	private boolean queryActive;
	private AbstractPrologProcess process;
	private int flags;
	
	public SocketSession(SocketClient client, AbstractPrologProcess process, int flags) {
		this.client = client;
		this.process = process;
		this.flags=flags;
	}
	
	@Override
	public void close() {
		dispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#dispose()
	 */
	@Override
	public void dispose() {
		if (isDisposed()) {
			return;
		}
		try {
			client.close();
		} catch (IOException e) {
			process.error(e);
		} finally {
			client = null;
		}
	}
	
	@Override
	public List<Map<String, Object>> queryAll(String query) throws PrologException, PrologProcessException {
		if (queryActive) {
			throw new PrologProcessException("Cannot start query while another query is active.");
		}
		return queryAllImpl(query);
	}

	@Override
	public IterableQuery queryIterator(String query) throws PrologException, PrologProcessException {
		if (queryActive) {
			throw new PrologProcessException("Cannot start query while another query is active.");
		}
		CTermUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (query.length() == 0) {
			return emptyIterator();
		}
		IterableQuery results;
		try {
			configureProtocol(flags);
			client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
			client.writeln(SocketCommunicationConstants.QUERY);
			client.readUntil(SocketCommunicationConstants.GIVE_TERM);
			normalizeQuery(query);
			results = readResultsIterator();
		} catch (IOException e) {
			throw process.error(e);
		} 
		return results;
	}

	private IterableQuery emptyIterator() {
		return new IterableQuery(this) {
			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public Map<String, Object> next() {
				return null;
			}

			@Override
			protected Map<String, Object> readFirstResult() throws IOException {
				return null;
			}

			@Override
			protected Map<String, Object> readMore() throws IOException {
				return null;
			}
		};
	}
	
	private List<Map<String, Object>> queryAllImpl(String query) throws PrologException, PrologProcessException {
		if ((flags & PrologProcess.CTERMS) == 0) {
			return queryAllAtOnce(query);
		} else {
			return queryAllDefault(query);
		}
	}
	
	private List<Map<String, Object>> queryAllDefault(String query) throws PrologException,
		PrologProcessException {
		CTermUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (query.length() == 0) {
			return generateEmptyResults();
		}
		Vector<Map<String, Object>> results;
		try {
			configureProtocol(flags);
			client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
			client.writeln(SocketCommunicationConstants.QUERY_ALL);
			client.readUntil(SocketCommunicationConstants.GIVE_TERM);
			normalizeQuery(query);
			results = readResults();
		} catch (IOException e) {
			throw process.error(e);
		} 
		return results;
	}
	
	private Vector<Map<String, Object>> readResults() throws IOException {
		Vector<Map<String, Object>> results = new Vector<Map<String, Object>>();
		Map<String, Object> result = read_solution(flags);
		while (result != null) {
			results.add(result);
			result = read_solution(flags);
		}
		return results;
	}

	private IterableQuery readResultsIterator() throws IOException {
		queryActive = true;
		return new IterableQuery(this) {
			@Override protected Map<String,Object> readFirstResult() throws IOException {
				return read_solution(flags);
			}
			
			@Override protected Map<String,Object> readMore() throws IOException {
				client.readUntil(SocketCommunicationConstants.MORE);
				client.writeln(SocketCommunicationConstants.YES);
				return read_solution(flags);
			}
		};
	}

	protected void closeIterator() throws IOException {
		queryActive = false;
		client.writeln(SocketCommunicationConstants.NO);
	}

	private List<Map<String, Object>> generateEmptyResults() {
		List<Map<String, Object>> l = new Vector<Map<String, Object>>();
		l.add(generateAnEmtpyResult());
		return l;
	}
	
	private List<Map<String, Object>> queryAllAtOnce(String query) throws PrologProcessException {
		int oldflags = flags;
		flags = flags | PrologProcess.PROCESS_LISTS;
		
		if (query.endsWith(".")) {
			query = query.substring(0, query.length()-1);
		}
		List<String> vars = getVariableNames(query);
		String newQuery = createFindallQuery(query, vars);
		Map<String, Object> result = queryOnce(newQuery);
		flags=oldflags;
		return transformResults(result, vars);
	}

	private List<Map<String, Object>> transformResults(Map<String, Object> result,  List<String> vars) {
		List<Map<String, Object>> newResult = new ArrayList<Map<String,Object>>();
		
		@SuppressWarnings("unchecked")
		List<List<Object>> listOfLists = (List<List<Object>>) result.get(RESULT_LIST);
		for (List<Object> list : listOfLists) {
			HashMap<String, Object> newMap = new HashMap<String, Object>();
			for (int i=0; i<vars.size(); i++) {
				newMap.put(vars.get(i), list.get(i));
			}
			newResult.add(newMap);
		}
		
		return newResult;
	}

	private String createFindallQuery(String query, List<String> vars) {
		StringBuffer buf = new StringBuffer();
		buf.append("findall([");
		boolean addComma = false;
		for (String v : vars) {
			if (addComma) {
				buf.append(", ");
			} else {
				addComma = true;
			}
			buf.append(v);
		}
		buf.append("], (");
		buf.append(query);
		buf.append("), " + RESULT_LIST + ")");
		return buf.toString();
	}

	private List<String> getVariableNames(String query) {
		try {
			String getVarQuery = "get_var_names(" + quoteQuery(query) + ",Vars)";
			Map<String, Object> result = queryOnce(getVarQuery);
			String varString = result.get("Vars").toString();
			if (varString.isEmpty()) {
				return new ArrayList<String>();
			} else {
				return Arrays.asList(Util.unquoteAtom(varString).split(","));
			}
		} catch (PrologProcessException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	private String quoteQuery(String query) {
		if (query.startsWith("'") && query.endsWith("'")) {
			return query;
		} else {
			StringBuilder buf = new StringBuilder("\'");
			for (int i = 0; i < query.length(); i++) {
				char c = query.charAt(i);
				switch (c) {
				case '\b':
					buf.append("\\b");
					continue;
				case '\t':
					buf.append("\\t");
					continue;
				case '\n':
					buf.append("\\n");
					continue;
				case '\f':
					buf.append("\\f");
					continue;
				case '\r':
					buf.append("\\r");
					continue;
				case '\"':
					buf.append("\\\"");
					continue;
				case '\'':
					buf.append("\\\'");
					continue;
				case '\\':
					buf.append("\\\\");
					continue;
				}
				buf.append(c);
			}
			buf.append('\'');
			return buf.toString();
		}
	}


	@Override
	public Map<String, Object> queryOnce(String query) throws PrologException, PrologProcessException {
		if (queryActive) {
			throw new PrologProcessException("Cannot start query while another query is active.");
		}
		CTermUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		Map<String, Object> solution;
		if (query.length() == 0) {
			solution = generateAnEmtpyResult();
		} else {
			try {
				configureProtocol(flags);
				client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
				client.writeln(SocketCommunicationConstants.QUERY);
				client.readUntil(SocketCommunicationConstants.GIVE_TERM);
				normalizeQuery(query);
				System.out.println("QUERY: " + query);
				solution = read_solution(flags);
				tryFinishReading();
			} catch (IOException e) {
				throw process.error(e);
			}
		}
		return solution;
	}

	private void tryFinishReading() throws PrologProcessException {
		try {
			finishReading();
		} catch (IOException e) {
			throw process.error(e);
		}
	}


	private void normalizeQuery(String query) throws IOException {
		query = query.trim();
		if (query.endsWith(".")) {
			client.writeln(query);
		} else {
			client.writeln(query + ".");
		}
	}

	private Map<String, Object> generateAnEmtpyResult() {
		return new HashMap<String, Object>();
	}
	
	private Map<String, Object> read_solution(int flags) throws IOException {
		HashMap<String, Object> result = new HashMap<String, Object>();
		while (true) {			
			String varname = (String) readValue(PrologProcess.UNQUOTE_ATOMS);
			if (varname == null) {
				return handleSpecialResults(result);
			} else {
				Object value = readVariableValue(flags, varname);
				result.put(varname, value);
			}
		}
	}

	private Object readVariableValue(int flags, String varname)
			throws IOException {
		Object value = readValue(flags);
		if (value == null) {
			throw new PrologException(
					"could not read value for variable " + varname);
		}
		return value;
	}

	private HashMap<String, Object> handleSpecialResults(HashMap<String, Object> result)
			throws IOException {
		String line = client.readln();
		if (line == null) {
			throw new IOException("There was no solution to read.");
		}
		if (line.startsWith(SocketCommunicationConstants.ERROR)) {
			throwPrologErrorToJavaVM(line);
		}
		if (SocketCommunicationConstants.END_OF_SOLUTION.equals(line)) {
			return result;
		}
		return null;
	}

	private void throwPrologErrorToJavaVM(String line) {
		int errorLength = SocketCommunicationConstants.ERROR.length();
		String errorSubstring = line.substring(errorLength);
		throw new PrologException(errorSubstring);
	}

	private Object readValue(int flags) throws IOException {
		return client.readValue(flags);
	}
	
	private void finishReading() throws IOException, PrologProcessException {
		while (true) {
			String line = client.readln();
			if (line == null) {
				throw process.error(new IllegalStateException(
						"There is nothing to read"));
			}
			if (SocketCommunicationConstants.MORE.equals(line)) {
				client.writeln(SocketCommunicationConstants.NO);
			}
			if (SocketCommunicationConstants.OK.equals(line)) {
				return;
			}
			if (line.startsWith(SocketCommunicationConstants.ERROR)) {
				throwPrologErrorToJavaVM(line);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#isDisposed()
	 */
	@Override
	public boolean isDisposed() {
		return client == null;
	}

	private void configureProtocol(int flags) {
		/*
		 * the only thing that needs to be configured on the server side, is
		 * whether or not lists are processed.
		 */
		boolean processLists = (flags & PrologProcess.PROCESS_LISTS) > 0;
		setProtocolOption("interprete_lists", Boolean.toString(processLists));
		
		boolean showUnboundResults = (flags & PrologProcess.UNBOUND_VARIABLES) > 0;
		setProtocolOption("unbound_variables", Boolean.toString(showUnboundResults));
	}

	public SocketClient getClient() {
		return client;
	}

	private void setProtocolOption(String id, String value) {
		if (queryActive) {
			throw new RuntimeException(
					"Cannot set protocol option while query is active.");
		}
		try {
			client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
			client.writeln(SocketCommunicationConstants.SET_OPTION);
			client.readUntil(SocketCommunicationConstants.GIVE_SYMBOL);
			client.writeln(id);
			client.readUntil(SocketCommunicationConstants.GIVE_TERM);
			client.writeln(value);
			client.readUntil(SocketCommunicationConstants.OK);
		} catch (IOException e) {
			throw new RuntimeException("IO Error while setting protocol option");
		} 
	}
}


