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

import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.progress.DeferredTreeContentManager;

public class CallTreeContentProvider implements ITreeContentProvider {

	private CallHierarchyView view;
	
	private DeferredTreeContentManager manager;

	
	CallTreeContentProvider(CallHierarchyView view) {
		this.view = view;
	}

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		manager = new DeferredTreeContentManager((AbstractTreeViewer) viewer, view.getSite());
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof Predicate[]) {
			return (Predicate[]) inputElement;
		}
		return null;
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		return manager.getChildren(parentElement);
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof PredicateEdge) {
			return ((PredicateEdge) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return manager.mayHaveChildren(element);
	}
	
}
