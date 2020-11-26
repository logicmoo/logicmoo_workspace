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

package org.cs3.pdt.common.search;

/*copied and adapted from the jdt source*/

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public abstract class PrologSearchContentProvider implements IStructuredContentProvider {
	protected final Object[] EMPTY_ARR= new Object[0];
	
	private PrologSearchResult fResult;
	private PrologSearchResultPage fPage;

	PrologSearchContentProvider(PrologSearchResultPage page) {
		fPage= page;
	}
	
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		initialize((PrologSearchResult) newInput);
		
	}
	
	protected void initialize(PrologSearchResult result) {
		fResult= result;
	}
	
	public abstract void elementsChanged(Object[] updatedElements);
	public abstract void clear();

	@Override
	public void dispose() {
		// nothing to do
	}

	PrologSearchResultPage getPage() {
		return fPage;
	}
	
	PrologSearchResult getSearchResult() {
		return fResult;
	}

}


