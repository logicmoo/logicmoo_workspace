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

package org.cs3.pdt.common.metadata;

public class Goal extends PrologElement {

	private static final long serialVersionUID = 1L;
	private String termString;
	private int start;
	private int end;

	public Goal(String file, String module, String elementName, int arity, String termString) {
		this(file, 0, 0, 0, module, elementName, arity, termString);
	}

	public Goal(String file, int line, int start, int end, String module, String elementName, int arity, String termString) {
		super(file, line, module, elementName, arity);
		this.termString = termString;
		this.start = start;
		this.end = end;
	}
	
	public String getTermString() {
		if (termString != null) {
			return termString;
		} else {
			return getSignature();
		}
	}

	public int getStart() {
		return start;
	}

	public int getEnd() {
		return end;
	}

}


