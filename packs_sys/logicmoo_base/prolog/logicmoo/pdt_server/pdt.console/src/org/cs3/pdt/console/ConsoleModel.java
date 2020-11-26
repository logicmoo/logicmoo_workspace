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

package org.cs3.pdt.console;

import java.io.IOException;

import org.cs3.pdt.console.internal.views.SingleCharModeException;

/**
 * Abstract model of a console.
 * 
 * A Console, as modeled by this interface, mainly consists of a buffer
 * containing a line of text that can be arbitrarily modified and finaly
 * comitted. In addition, a console produces output, which can be recieved by
 * registering an apropiate listener with this console. Note that the model by
 * itself does not support any buffering of the produced output.
 * 
 * As an (optional) feature, a console may support two different kinds of input
 * processing: the "normal" mode is described above, the alternative is the
 * so-called "single char" mode. When in single char mode, the underlying
 * streams expect character-wise unbuffered input via the putSingleChar(char)
 * method.
 */
public interface ConsoleModel {

	/**	 
	 * @return the current content of the linebuffer.
	 */
	abstract public String getLineBuffer();

	/**
	 * set the content of the line buffer. 
	 * @param line
	 */
	abstract public void setLineBuffer(String line);

	/**
	 * commit the current content of the linebuffer (i.e. "press enter").	 
	 * @throws SingleCharModeException 
	 */
	abstract public void commitLineBuffer() throws SingleCharModeException;

	/**
	 * Register a ConsoleModelListerner with this console.
	 * @param cml
	 */
	abstract public void addConsoleListener(ConsoleModelListener cml);

	/**
	 * remove a registered ConsoleModelLister from the list of registered Listeners.
	 * @param cml
	 */
	abstract public void removeConsoleListener(ConsoleModelListener cml);

	/**
	 * Send a single char to the underlying stream.
	 * Wether the input buffer is modified or not, is up to the respective implementation.
	 * <p><P>It is concidered a bad practice to call this method while the model is not in single
	 * char mode, although some implementations might allow it, this should generaly throw some kind
	 * of runtime exception.  
	 * @param c
	 * @throws SingleCharModeException 
	 */
	abstract public void putSingleChar(char c) throws SingleCharModeException;

	/**
	 * @return true if and only if the console is currently in single char mode.
	 */
	abstract public boolean isSingleCharMode();

	/**
	 * tell the model to connect to the underlying streams.
	 * @throws IOException 
	 */
	abstract public void connect() throws IOException;
	
	/**
	 * tell the model to disconnect from the underlying streams.
	 */
	abstract public void disconnect();
	
	/**
	 * @return true if and only if the console is connected to the underlying
	 * streams, i.e. it is fully operational.
	 */
	abstract public boolean isConnected();

	void registerQueryExpansion(QueryExpansion e);

	void unregisterQueryExpansion(QueryExpansion e);

}


