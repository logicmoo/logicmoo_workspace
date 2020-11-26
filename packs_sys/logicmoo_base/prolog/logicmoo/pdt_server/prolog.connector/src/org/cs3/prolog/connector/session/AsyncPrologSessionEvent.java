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

package org.cs3.prolog.connector.session;

import java.util.EventObject;
import java.util.Map;

/**
 * An asynchronous Prolog session event consisting of a ticket and a query.
 * Depending on the context it may also contain a message or a map of bindings
 * representing a Prolog result.
 * 
 * An event is passed to an {@link AsyncPrologSessionListener} and can be
 * handled there.
 */
public class AsyncPrologSessionEvent extends EventObject {
	
	private static final long serialVersionUID = 1787537795491818031L;
	public String query=null;
	public Object ticket=null;
	public String message=null;
	private Map<String, Object> bindings = null;
	public Exception exception=null;
	public int id=-1;
	
	/**
	 * Creates an asynchronous Prolog session event for the given source.
	 * 
	 * @param source
	 */
	public AsyncPrologSessionEvent(Object source) {
		super(source);
	}

	/**
	 * Creates an asynchronous Prolog session event for the given source and
	 * ticket.
	 * 
	 * @param source
	 * @param ticket 
	 */
	public AsyncPrologSessionEvent(Object source, Object ticket) {
		super(source);
		this.ticket=ticket;
	}

	/**
	 * Creates an asynchronous Prolog session event for the given source, ticket
	 * and message.
	 * 
	 * @param source
	 * @param ticket
	 * @param message
	 */
	public AsyncPrologSessionEvent(Object source, Object ticket, String message) {
		super(source);
		this.ticket=ticket;
		this.message=message;
	}

	/**
	 * Creates an asynchronous Prolog session event for the given source, ticket and map of bindings.
	 * 
	 * @param source
	 * @param ticket 
	 * @param bindings 
	 */
	public AsyncPrologSessionEvent(Object source, Object ticket, Map<String, Object> bindings) {
		super(source);
		this.ticket=ticket;
		this.bindings=bindings;
	}

	/**
	 * @return the map of bindings;<br>null if bindings are not available 
	 */
	public Map<String, Object> getBindings() {
		return bindings;
	}

}


