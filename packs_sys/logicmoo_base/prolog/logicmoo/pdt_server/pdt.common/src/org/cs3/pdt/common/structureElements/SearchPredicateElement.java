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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Vector;

import org.cs3.pdt.common.metadata.Predicate;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

/**
 * used in prolog searches to represent a predicate.
 */
public class SearchPredicateElement extends Predicate implements PrologSearchTreeElement {

	private static final long serialVersionUID = 8822257720982382862L;
	
	private LinkedHashMap<String, SearchMatchElement> matchesToSearchElements = new LinkedHashMap<String, SearchMatchElement>();
	private Object parent;

	private ArrayList<PredicateMatch> matches;
	
	public SearchPredicateElement(Object parent, String module, String predicateName, int arity, List<String> properties) {
		super(module,predicateName,arity, properties);
		this.parent = parent;
	}
	
	public SearchPredicateElement(Object parent, String module, String predicateName, int arity) {
		super(module, predicateName, arity, new Vector<String>());
		this.parent = parent;
	}
	
	@Override
	public boolean hasChildren() {
		return !matchesToSearchElements.values().isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return matchesToSearchElements.values().toArray();
	}
	
	@Override
	public String getLabel() {
		return getFunctor() + "/" + getArity();
	}
	
	public PrologMatch getFirstOccurrence() {
		if (matchesToSearchElements.isEmpty()) {
			return null;
		}
		PrologMatch firstMatch = null;
		int firstLine = Integer.MAX_VALUE;
		for (SearchMatchElement element : matchesToSearchElements.values()) {
			PrologMatch occurence = element.getMatch();
			int line = occurence.getLine();
			if (firstMatch == null) {
				firstMatch = occurence;
				firstLine = line;
			} else if (line < firstLine) {
				firstLine = line;
				firstMatch = occurence;
			}
		}
		return firstMatch;
	}
	
	public void removeMatch(PrologMatch match) {
		String signature = match.getSignature();
		SearchMatchElement searchMatchElement = matchesToSearchElements.get(signature);
		if (searchMatchElement != null) {
			searchMatchElement.removeMatch(match);
			if (searchMatchElement.computeContainedMatches() <= 0) {
				matchesToSearchElements.remove(signature);
			}
		}
	}

	public void addMatch(PrologMatch match) {
		SearchMatchElement searchMatchElement = matchesToSearchElements.get(match.getSignature());
		if (searchMatchElement == null) {
			searchMatchElement = (SearchMatchElement) match.getElement();
			searchMatchElement.setParent(this);
			matchesToSearchElements.put(match.getSignature(), searchMatchElement);
		}
		searchMatchElement.addMatch(match);
	}

	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public boolean equals(Object object) {
		return this == object;
	}

	public void addMatch(PredicateMatch match) {
		if (matches == null) {
			matches = new ArrayList<PredicateMatch>();
		}
		matches.add(match);
	}
	
	public void removeMatch(PredicateMatch match) {
		if (matches != null) {
			matches.remove(match);
		}
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}

	public boolean hasMatches() {
		return (matches != null && !matches.isEmpty());
	}

	@Override
	public int computeContainedMatches() {
		if (hasMatches()) {
			return matches.size();
		}
		int count = 0;
		for (SearchMatchElement element : matchesToSearchElements.values()) {
			count += element.computeContainedMatches();
		}
		return count;
	}

	@Override
	public void collectContainedMatches(IFile file, ArrayList<Match> matches) {
		for (SearchMatchElement element : matchesToSearchElements.values()) {
			element.collectContainedMatches(file, matches);
		}
	}
	
}


