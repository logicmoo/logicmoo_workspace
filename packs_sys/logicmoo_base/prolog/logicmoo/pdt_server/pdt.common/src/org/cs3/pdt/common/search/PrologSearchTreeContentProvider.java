/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.search;

import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ITreeContentProvider;


public class PrologSearchTreeContentProvider extends PrologSearchContentProvider implements ITreeContentProvider {
	PrologSearchTreeContentProvider(PrologSearchResultPage page) {
		super(page);
	}

	@Override
	protected synchronized void initialize(PrologSearchResult result) {
		super.initialize(result);
	}

	@Override
	public Object getParent(Object child) {
		if (child==null || getSearchResult() == null){
			return null;
		} else if (child instanceof PrologTreeElement){
			return ((PrologTreeElement) child).getParent();
		} else if (child instanceof PrologMatch){
			return ((PrologMatch) child).getElement();
		} else {
			return null;
		}
	}

	

	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement==null||getSearchResult()==null){
			return new Object[0];
		} else if (parentElement instanceof PrologSearchResult){
			return ((PrologSearchResult) parentElement).getChildren();
		} else if (parentElement instanceof PrologTreeElement) {
			return ((PrologTreeElement) parentElement).getChildren();
		} else {
			return new Object[0];
		}
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element==null || getSearchResult() == null) {
			return false;
		} else if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).hasChildren();
		} else {
			return (element instanceof IFile) || (element instanceof PrologSearchResult);
		}
	}

	@Override
	public void clear() {
		initialize(getSearchResult());
		getPage().getViewer().refresh();
	}

	@Override
	public void elementsChanged(Object[] updatedElements) {
	}
}

