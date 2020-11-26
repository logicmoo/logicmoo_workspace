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

package org.cs3.pdt.common.structureElements;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.search.ui.text.Match;

public class SearchMatchElement implements PrologSearchTreeElement {

	private ArrayList<PrologMatch> matches = new ArrayList<PrologMatch>();
	private Object parent;
	
	public SearchMatchElement() {
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}
	
	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public Object[] getChildren() {
		return new Object[0];
	}

	@Override
	public String getLabel() {
		if (matches.isEmpty()) {
			return null;
		}
		return matches.get(0).getLabel();
	}
	
	public StyledString getStyledString() {
		if (matches.isEmpty()) {
			return null;
		}
		StyledString str = new StyledString();
		boolean first = true;
		for (PrologMatch match : matches) {
			if (first) {
				first = false;
			} else {
				str.append(" | ");
			}
			str.append(match.getStyledString());
		}
		if (matches.size() > 1) {
			str.append(" (", StyledString.COUNTER_STYLER);
			str.append(Integer.toString(matches.size()), StyledString.COUNTER_STYLER);
			str.append(" matches)", StyledString.COUNTER_STYLER);
		}
		return str;
	}

	public void removeMatch(PrologMatch match) {
		matches.remove(match);
	}

	public void addMatch(PrologMatch match) {
		matches.add(match);
	}

	@Override
	public Object getParent() {
		return parent;
	}

	public IFile getFile() {
		if (matches.isEmpty()) {
			return null;
		}
		return matches.get(0).getFile();
	}

	@Override
	public int computeContainedMatches() {
		return matches.size();
	}

	public PrologMatch getMatch() {
		if (matches.isEmpty()) {
			return null;
		}
		return matches.get(0);
	}

	@Override
	public void collectContainedMatches(IFile file, ArrayList<Match> matches) {
		matches.addAll(this.matches);
	}

}


