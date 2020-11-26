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

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.cs3.prolog.connector.common.Util;



public class OutlineClauseElement implements PrologTreeElement{
	
	private static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
	private String label;
//	private int line;
//	private String type;
	private Object parent;
//	private String file;
	private PrologClause clause;
private String firstArgument;
	
//	public OutlineClauseElement(String label, String file, int line, String type, PrologTreeElement parent) {
//		this.label = label;
//		this.line = line;
//		this.type = type;
//		this.file = file;
//		this.parent = parent;
//	}
	
	public OutlineClauseElement(Object parent, PrologClause clause) {
		this.parent = parent;
		this.clause = clause;
		this.label = calculateOccuranceLabel();
	}

	public int getLine() {
		return clause.getLine();
	}
	
	public String getType() {
		return clause.getType();
	}
	
	@Override
	public Object getParent() {
		return parent;
	}
	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public Object[] getChildren() {
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public String getLabel() {
		return label;
	}
	
	public String getFile() {
		return clause.getOccuranceFile();
	}

	@Override
	public int hashCode() {
		return (clause.getOccuranceFile() + label).hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineClauseElement)) {
			return false;
		} else {
			OutlineClauseElement other = (OutlineClauseElement) object;
			return (getFile().equals(other.getFile())
					&& label.equals(other.label)
					&& getLine() == other.getLine());
		}
	}

	private String calculateOccuranceLabel() {
		firstArgument = PDTCommonUtil.getProperty("first_argument", clause.getProperties());
		if (firstArgument != null) {
			return clause.getLine() + ": " + Util.unquoteAtom(firstArgument);
		} else {
			StringBuffer occuranceLabel = new StringBuffer("Line: ");
			occuranceLabel.append(Integer.toString(clause.getLine()));
			occuranceLabel.append(" (");
			occuranceLabel.append(clause.getType());
			occuranceLabel.append(")");
			return occuranceLabel.toString();
		}
	}

	public String getFunctor() {
		return clause.getFunctor();
	}

	public int getArity() {
		return clause.getArity();
	}
	
	public String getFirstArgument() {
		return firstArgument;
	}
	
}


