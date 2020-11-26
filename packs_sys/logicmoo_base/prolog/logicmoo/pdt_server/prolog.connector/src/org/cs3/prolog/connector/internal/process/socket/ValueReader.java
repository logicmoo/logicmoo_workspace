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

package org.cs3.prolog.connector.internal.process.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Stack;
import java.util.Vector;

import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.cterm.CTerm;
import org.cs3.prolog.connector.cterm.CTermFactory;
import org.cs3.prolog.connector.process.PrologProcess;

public class ValueReader {
	private static final char END_OF_VALUE_CHAR = '>';
	private static final char BEGIN_OF_VALUE_CHAR = '<';
	static final char END_OF_LIST_CHAR = '}';
	static final char BEGIN_OF_LIST_CHAR = '{';
	static final int EOF_CHAR = -1;
	private SocketClient socketClient;
	private BufferedReader reader;
	private boolean hasToContinue;
	private StringBuffer valueBuffer;
	private Stack<Vector<Object>> stack;
	
	ValueReader(SocketClient socketClient) {
		this.socketClient=socketClient;
	}

	public  Object readValue(int flags) throws IOException {
		Object value = null;
		value = doReading(flags);
		return value;
	}

	private Object doReading(int flags)
	throws IOException {
		Object value=null;
		prepeareEverythingForNextReading();
		char lastReadCharacter = readFirstInterestingCharacter(reader);
		while (hasToContinue) {
			value = analyseCharacter(flags,lastReadCharacter);
			if (hasToContinue) {
				lastReadCharacter = (char)reader.read();
			}
		}
		checkForUnexpectedEOF(lastReadCharacter);
		return value;
	}

	private void prepeareEverythingForNextReading() {
		reader = socketClient.getReader();
		valueBuffer = new StringBuffer();
		stack = new Stack<Vector<Object>>();
		hasToContinue=true;
	}

	private Object analyseCharacter(int flags, char lastReadCharacter) throws IOException {
		Object value = null;
		switch (lastReadCharacter) {
		case BEGIN_OF_VALUE_CHAR:
			clearValueBuffer();
			break;
		case BEGIN_OF_LIST_CHAR:
			stack.push(new Vector<Object>());
			break;
		case  END_OF_VALUE_CHAR:
			value = parseValue(flags);
			appendToTopmostListIfStackIsNotEmpty(value);
			break;
		case END_OF_LIST_CHAR:
			value = popTopmostList();
			appendToTopmostListIfStackIsNotEmpty(value);
			break;
		case (char)EOF_CHAR:
			hasToContinue=false;
			break;
		default:
			valueBuffer.append(lastReadCharacter);
		}
		return value;
	}

	private Object popTopmostList()
			throws IOException {
		Object value;
		if (stack.isEmpty()) {
			throw new IOException(
					"Read a closing curly bracket ('}') but there is no containing list!");
		}
		value = stack.pop();
		return value;
	}

	private Object parseValue(int flags) {
		Object value;
		String unparsedValue = Util.unescapeBuffer(valueBuffer);
		
		if (Util.flagsSet(flags,PrologProcess.CTERMS)) {
			CTerm ctermValue = CTermFactory.createCTerm(unparsedValue);
			value=ctermValue;
		} else{
			if(Util.flagsSet(flags, PrologProcess.UNQUOTE_ATOMS)) {
				value = Util.unquoteStringOrAtom(unparsedValue);
			} else {
				value=unparsedValue;
			}
		}
		return value;
	}

	private boolean invalidFirstCharacter(int firstCharacter) {
		return !(firstCharacter == BEGIN_OF_VALUE_CHAR || firstCharacter == BEGIN_OF_LIST_CHAR);
	}

	private char readFirstInterestingCharacter(BufferedReader reader)
	throws IOException {
		char firstInterestingCharacter = skipWhiteSpace(reader);
		checkForUnexpectedEOF(firstInterestingCharacter);
		if (invalidFirstCharacter(firstInterestingCharacter)) {
			reader.reset();
			hasToContinue=false;
		}
		return firstInterestingCharacter;
	}

	private void checkForUnexpectedEOF(int lastReadCharacter)
			throws IOException {
		if (lastReadCharacter == EOF_CHAR) {
			throw new IOException(
			"read EOF, while skipping whitespace before value.");
		}
	}

	private char skipWhiteSpace(BufferedReader reader) throws IOException {
		reader.mark(8); // if set to low, the socket connection may break. Be carefull!!!
		int readCharacter = reader.read();
		while (readCharacter != EOF_CHAR && Character.isWhitespace((char) readCharacter)) {
			reader.mark(1);
			readCharacter = reader.read();
		}
		return (char)readCharacter;
	}

	private void clearValueBuffer() {
		valueBuffer.setLength(0);
	}

	private void appendToTopmostListIfStackIsNotEmpty(Object value) {
		// if the stack is empty, we are finished.
		// Otherwise, the value becomes element of the list lying on top of
		// the stack.
		if (stack.isEmpty()) {
			hasToContinue=false;
		} else {
			Vector<Object> l = stack.peek();
			l.add(value);
		}
	}

}


