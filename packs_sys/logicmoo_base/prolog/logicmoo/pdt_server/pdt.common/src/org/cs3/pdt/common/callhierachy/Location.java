/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.callhierachy;

import org.eclipse.core.resources.IFile;

public class Location implements Comparable<Location> {
	
	private String text;
	private IFile file;
	private int start;
	private int end;
	private int line;

	public Location(String text, IFile file, int start, int end, int line) {
		this.text = text;
		this.file = file;
		this.start = start;
		this.end = end;
		this.line = line;
	}

	public String getText() {
		return text;
	}

	public IFile getFile() {
		return file;
	}

	public int getStart() {
		return start;
	}

	public int getEnd() {
		return end;
	}
	public int getLine() {
		return line;
	}

	@Override
	public int compareTo(Location o) {
		int c = file.getName().compareTo(o.file.getName());
		if (c != 0) {
			return c;
		}
		return start - o.start;
	}

}
