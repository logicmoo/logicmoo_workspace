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

import java.io.Serializable;

/**
 * a tupel describing a position within a sourcefile. <br>
 * This is just used as a means of using structured data as argument and return
 * value. <br>
 * Rows are counted starting from 1. <br>
 * Offsets are counted starting from 0. <br>
 * Last rows are always inclusive. <br>
 * End offsets are always exclusive.
 * 
 * Two instances of this class are considered equal, if 
 *  - they are both either streambased or line base,
 *  - they both are refer to a workspace or an absolute file
 *  - they both describe exactly the same "physical" character sequence.
 *  
 * 
 */
public final class SourceLocation implements Serializable, Comparable<SourceLocation> {
	private static final long serialVersionUID = 1L;

	/**
	 * The offset within the source file / source line. <br>
	 * If this is a line-based source location, this is the character position
	 * within the containing line. The first character in a line is character 0.
	 * <br>
	 * If this location is NOT row-based, this is the offset within the
	 * containing character stream. The first character in the file is at offset
	 * 0.
	 */
	private int offset = 0;

	/**
	 * The end offset of a range. (exclusive) <br>
	 * For line-based locations, this is the position of the first character
	 * that does NOT belong to the range within the last line that DOES belong
	 * to the range. <br>
	 * For stream-based locations, this is the offset of the first character
	 * that does NOT belong to the range.
	 */
	private int endOffset = 0;

	/**
	 * The absolute path to the file containing this location. <br>
	 * The should be interpreted as workspace path or as filesystem path,
	 * depending on the value of isWorkspacePath.
	 */
	public String file;

	/**
	 * Determines wether the path is workspace-relative.
	 */
	public boolean isWorkspacePath;

	private int line;

	private boolean isLineLocation = false;

	private String predicateName;

	private int arity;

	public SourceLocation(String file, boolean isWorkspacePath) {
		this.file = file;
		this.isWorkspacePath = isWorkspacePath;
	}

	public boolean isLineLocation() {
		return isLineLocation;
	}

	public int getLine() {
		return line;
	}

	/**
	 * 
	 * @param line
	 */
	public void setLine(int line) {
		offset=-1;
		isLineLocation = true;
		this.line = line;
	}

	@Override
	public String toString() {
		return file + "/" + getOffset();
	}

	@Override
	public int compareTo(SourceLocation arg0) {
		SourceLocation other = arg0;
		// workspace paths come before absolute paths.
		int c = (isWorkspacePath ? 0 : 1) - (other.isWorkspacePath ? 0 : 1);
		if (c != 0) {
			return c;
		}
		// lexicographic comparision of files
		c = file.compareTo(other.file);
		if (c != 0) {
			return c;
		}
		// if there is still no difference, both positions are
		// either row based or stream based.
		c = getOffset()-other.getOffset() ;
		if (c != 0) {
			return c;
		}
		c = getEndOffset()-other.getEndOffset();
		if (c != 0) {
			return c;
		}
		//both source locations describe the same "physical" character sequence.
		return 0;
	}

	@Override
	public boolean equals(Object obj) {		
		if(obj instanceof SourceLocation) {
			return compareTo((SourceLocation)obj)==0;
		}
		else return false;
	}
	
	@Override
	public int hashCode() {
		return file.hashCode()+getOffset()+getEndOffset();
	}

	public void setOffset(int offset) {
		isLineLocation=false;
		this.offset = offset;
	}

	public int getOffset() {
		return offset;
	}

	public void setPredicateName(String name) {
		this.predicateName=name;
		
	}

	public String getPredicateName() {
		return predicateName;
	}

	public void setArity(int arity) {
		this.arity =arity;
		
	}

	public int getArity() {
		return arity;
	}

	public void setEndOffset(int endOffset) {
		this.endOffset = endOffset;
	}

	public int getEndOffset() {
		return endOffset;
	}
}


