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

package org.cs3.pdt.editor.internal.editors.breakpoints;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

public class MarkerBackup {

	private IFile file;
	private int lineNumber;
	private int offset;
	private String id;

	public MarkerBackup(IResource file, int lineNumber, String id, int offset) {
		this.file = (IFile) file;
		this.lineNumber = lineNumber;
		this.id = id;
		this.offset = offset;
	}

	/**
	 * @return the file
	 */
	public IFile getFile() {
		return file;
	}

	/**
	 * @return the current line number
	 */
	public int getLineNumber() {
		return lineNumber;
	}

	/**
	 * @return the offset
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}
	
	@Override
	public String toString() {
		return file.toString() + "[id: " + id + ", line: " + lineNumber + ", offset: " + offset + "]";
	}
}


