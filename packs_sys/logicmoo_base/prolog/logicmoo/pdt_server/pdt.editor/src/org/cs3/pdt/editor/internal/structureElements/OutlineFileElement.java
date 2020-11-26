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

package org.cs3.pdt.editor.internal.structureElements;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.common.metadata.PrologSourceLocation;
import org.cs3.pdt.common.structureElements.PrologTreeElement;

public class OutlineFileElement extends PrologSourceLocation implements PrologTreeElement {

	private String fullFileName;
	private String fileName;
	
	private List<OutlineClauseElement> clauses = new ArrayList<OutlineClauseElement>();
	private Object parent;
	private String label;
	
	public OutlineFileElement(Object parent, String file) {
		super(file, 1);
		fullFileName = file;
		fileName = getFileName(file);
		label = fileName + " (multifile)";
		this.parent = parent;
	}

	@Override
	public boolean hasChildren() {
		return !clauses.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return clauses.toArray();
	}

	@Override
	public String getLabel() {
		return label;
	}
	
	private String getFileName(String fullPathOfFile) {
		return new File(fullPathOfFile).getName();
	}

	public int numberOfClauses() {
		return clauses.size();
	}
	
	@Override
	public int hashCode() {
		return fullFileName.hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineFileElement)) {
			return false;
		} else {
			return fullFileName.equals(((OutlineFileElement) object).fullFileName); 
		}
	}

	public String getFileName() {
		return fileName;
	}
	
	@Override
	public Object getParent() {
		return parent;
	}

	public void addClause(PrologClause clause) {
		OutlineClauseElement clauseElement = new OutlineClauseElement(this, clause);
		clauses.add(clauseElement);
	}
	
	public int getFirstLine() {
		if (clauses.isEmpty()) {
			return getLine();
		} else {
			return clauses.get(0).getLine();
		}
	}
}


