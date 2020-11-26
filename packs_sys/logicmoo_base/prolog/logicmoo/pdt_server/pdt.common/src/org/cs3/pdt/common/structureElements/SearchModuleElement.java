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
import java.util.TreeMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.search.ui.text.Match;

public class SearchModuleElement implements PrologSearchTreeElement, Comparable<SearchModuleElement> {
	
	private String moduleAndVisibiltyLabel;
	private String label;
	private int visibilityCode;
	private String name;
	private Object parent;
	private IFile file;
	
	private LinkedHashMap<String, SearchDirectiveElement> directives = new LinkedHashMap<>();
	private TreeMap<String, SearchPredicateElement> predForSignature = new TreeMap<>();
	private ModuleMatch match;
	
	public SearchModuleElement(Object parent, String name, String visibility, IFile file) {
		this.parent = parent;
		this.name = name;
		this.file = file;
		
		StringBuilder buf = new StringBuilder(name);
		if (visibility != null && !visibility.isEmpty()) {
			buf.append(" (");
			buf.append(visibility);
			buf.append(')');
		}
		moduleAndVisibiltyLabel = buf.toString();
		if (file != null) {
			buf.append(" - ");
			buf.append(file.getName());
		}
		label = buf.toString();
		
		if ("invisible".equalsIgnoreCase(visibility)) {
			visibilityCode = 1; 
		} else if ("sub".equalsIgnoreCase(visibility) || "descendant".equalsIgnoreCase(visibility)) {
			visibilityCode = 2; 
		} else if ("local".equalsIgnoreCase(visibility)) {
			visibilityCode = 3; 
		} else if ("super".equalsIgnoreCase(visibility) || "inherited".equalsIgnoreCase(visibility)) {
			visibilityCode = 4; 
		}
	}
	
	@Override
	public String getLabel() {
		return label;
	}

	public StyledString getStyledString() {
		StyledString str = new StyledString(moduleAndVisibiltyLabel);
		if (file != null) {
			str.append(' ');
			str.append(file.getName(), StyledString.DECORATIONS_STYLER);
		}
		return str;
	}
	
	public String getName() {
		return name;
	}
	
	@Override
	public Object[] getChildren() {
		Object[] children = new Object[directives.size() + predForSignature.size()];
		Object[] directiveChildren = directives.values().toArray();
		Object[] predicateChildren = predForSignature.values().toArray();
		System.arraycopy(directiveChildren, 0, children, 0, directiveChildren.length);
		System.arraycopy(predicateChildren, 0, children, directiveChildren.length, predicateChildren.length);
		return children;
	}

	@Override
	public boolean hasChildren() {
		return !predForSignature.isEmpty() || !directives.isEmpty();
	}

	@Override
	public int compareTo(SearchModuleElement o) {
		int visibilityDifference = o.visibilityCode - this.visibilityCode;
		if (visibilityDifference != 0) {
			return visibilityDifference;
		}
		int nameDifference = name.compareTo(o.getName());
		if (nameDifference != 0) {
			return nameDifference;
		}
		if (file == null) {
			if (o.file == null) {
				return 0;
			} else {
				return 1;
			}
		} else {
			if (o.file == null) {
				return -1;
			} else {
				return (file.getName().compareTo(o.file.getName()));
			}
		}
	}

	public void removeMatch(PrologMatch match) {
		String signature = getSignatureForMatch(match);
		if (predForSignature.containsKey(signature)) {
			SearchPredicateElement predicateElement = predForSignature.get(signature);
			predicateElement.removeMatch(match);
			if (!predicateElement.hasChildren()) {
				predForSignature.remove(signature);
			}
		}
	}

	public void addMatch(PrologMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement searchPredicateElement = predForSignature.get(signature); 
		if (searchPredicateElement == null) {
			searchPredicateElement = new SearchPredicateElement(this, match.getModule(), match.getName(), match.getArity(), match.getProperties());
			predForSignature.put(signature, searchPredicateElement);
		}
		searchPredicateElement.addMatch(match);
	}
	
	private String getSignatureForMatch(PrologMatch match) {
		return match.getName() + match.getArity();
	}
	
	public void removeMatch(PredicateMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement searchPredicateElement = predForSignature.get(signature);
		searchPredicateElement.removeMatch(match);
		if (!searchPredicateElement.hasMatches()) {
			predForSignature.remove(signature);
		}
	}
	
	public void addMatch(PredicateMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement element = predForSignature.get(signature);
		if (element == null) {
			element = (SearchPredicateElement) match.getElement();
			element.setParent(this);
			predForSignature.put(signature, element);
		}
		element.addMatch(match);
	}

	private String getSignatureForMatch(PredicateMatch match) {
		return match.getName() + match.getArity();
	}

	public void removeMatch(DirectiveMatch directiveMatch) {
		String signature = directiveMatch.getSignature();
		SearchDirectiveElement searchDirectiveElement = directives.get(signature);
		searchDirectiveElement.removeMatch(directiveMatch);
		if (!searchDirectiveElement.hasMatches()) {
			directives.remove(signature);
		}
	}

	public void addMatch(DirectiveMatch directiveMatch) {
		String signature = directiveMatch.getSignature();
		SearchDirectiveElement searchDirectiveElement = directives.get(signature);
		if (searchDirectiveElement == null) {
			searchDirectiveElement = (SearchDirectiveElement) directiveMatch.getElement();
			searchDirectiveElement.setParent(this);
			directives.put(signature, searchDirectiveElement);
		}
		searchDirectiveElement.addMatch(directiveMatch);
	}
	
	@Override
	public Object getParent() {
		return parent;
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}
	
	public int getLine() {
		if (match == null) {
			return -1;
		} else {
			return match.getOffset();
		}
	}
	
	public IFile getFile() {
		return file;
	}

	public void setMatch(ModuleMatch match) {
		this.match = match;
	}
	
	public ModuleMatch getMatch() {
		return match;
	}

	@Override
	public int computeContainedMatches() {
		if (match != null) {
			return 1;
		}
		int count = 0;
		for (SearchDirectiveElement element : directives.values()) {
			count += element.computeContainedMatches();
		}
		for (SearchPredicateElement element : predForSignature.values()) {
			count += element.computeContainedMatches();
		}
		return count;
	}

	@Override
	public void collectContainedMatches(IFile file, ArrayList<Match> matches) {
		if (file.equals(this.file)) {
			for (SearchDirectiveElement element : directives.values()) {
				element.collectContainedMatches(file, matches);
			}
			for (SearchPredicateElement element : predForSignature.values()) {
				element.collectContainedMatches(file, matches);
			}
		}
	}

}



