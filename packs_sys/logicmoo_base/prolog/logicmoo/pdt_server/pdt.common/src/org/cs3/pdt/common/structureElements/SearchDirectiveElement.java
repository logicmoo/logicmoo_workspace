/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.structureElements;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.search.ui.text.Match;

public class SearchDirectiveElement implements PrologSearchTreeElement {

	private ArrayList<DirectiveMatch> matches = new ArrayList<>();
	private Object parent;
	
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
		for (DirectiveMatch match : matches) {
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

	public void removeMatch(DirectiveMatch match) {
		matches.remove(match);
	}

	public void addMatch(DirectiveMatch match) {
		matches.add(match);
	}

	public boolean hasMatches() {
		return !matches.isEmpty();
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

	@Override
	public void collectContainedMatches(IFile file, ArrayList<Match> matches) {
		matches.addAll(this.matches);
	}

}
