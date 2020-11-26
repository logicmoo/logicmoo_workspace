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

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.ui.progress.IDeferredWorkbenchAdapter;

public class PredicateEdge implements Comparable<PredicateEdge>, IAdaptable {
	
	private Object parent;
	private Predicate source;
	private Predicate target;
	private int count;
	private int hash;
	
	private DeferredWorkbenchAdapter deferredWorkbenchAdapter;
	
	private StyledString styledString;

	public PredicateEdge(Object parent, Predicate source, Predicate target, int count, DeferredWorkbenchAdapter deferredWorkbenchAdapter) {
		this.parent = parent;
		this.source = source;
		this.target = target;
		this.count = count;
		this.deferredWorkbenchAdapter = deferredWorkbenchAdapter;
	}
	
	public Object getParent() {
		return parent;
	}

	public Predicate getSource() {
		return source;
	}

	public Predicate getTarget() {
		return target;
	}

	public int getCount() {
		return count;
	}
	
	public StyledString getStyledString() {
		if (styledString == null) {
			styledString = new StyledString();
			styledString.append(target.getStyledString());
			if (count > 1) {
				styledString.append(" (" + Integer.toString(count) + " matches)", StyledString.COUNTER_STYLER);
			}
		}
		return styledString;
	}
	
	public String getVisibility() {
		return target.getVisibility();
	}
	
	@Override
	public String toString() {
		return target.getLabel();
	}

	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		if (IDeferredWorkbenchAdapter.class.equals(adapter)) {
			return deferredWorkbenchAdapter;
		}
		return null;
	}

	@Override
	public int compareTo(PredicateEdge o) {
		return target.compareTo(o.target);
	}
	
	@Override
	public int hashCode() {
		if (hash == 0) {
			hash = source.hashCode() + target.hashCode();
		}
		return hash;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof PredicateEdge) {
			return source.equals(((PredicateEdge) obj).source)
					&& target.equals(((PredicateEdge) obj).target);
		}
		return false;
	}

}
