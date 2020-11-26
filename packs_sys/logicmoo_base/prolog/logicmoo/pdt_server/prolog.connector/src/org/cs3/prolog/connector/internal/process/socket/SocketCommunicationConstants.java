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

public interface SocketCommunicationConstants {

	public static final String GIVE_TERM = "GIVE_TERM";
	public static final String END_OF_SOLUTION = "END_OF_SOLUTION";
	public static final String MORE = "MORE?";
	public static final String SHUTDOWN = "SHUTDOWN";
	public static final String BYE = "BYE";
	public static final String GIVE_COMMAND = "GIVE_COMMAND";
	public static final String GIVE_SYMBOL = "GIVE_SYMBOL";
	public static final String QUERY = "QUERY";
	public static final String QUERY_ALL = "QUERY_ALL";
	public static final String GO_AHEAD = "GO_AHEAD";
	public static final String LINE_SEPARATOR = "\n";
	public static final String OK = "OK";
	public static final String ERROR = "ERROR: ";
	public final static String YES = "YES";
	public final static String NO = "NO";
	public static final String PING = "PING";
	public static final String PONG = "PONG";
	public static final String ENTER_BATCH = "ENTER_BATCH";
	public static final String ABORT_COMPLETE = "ABORT_COMPLETE: ";
	public static final String EOB_COMPLETE = "END_OF_BATCH_COMPLETE";
	public static final String JOIN_COMPLETE = "JOIN_COMPLETE: ";
	public static final String RESULTS_FOR_QUERY = "RESULTS_FOR_QUERY: ";
	public static final String SKIPPING_QUERY = "SKIPPING_QUERY: ";
	public static final String EOB = "end_of_batch.";
	public static final String CUT = "CUT";
	public static final String SET_OPTION = "SET_OPTION";

}


