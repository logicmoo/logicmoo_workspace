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

package org.cs3.pdt.editor.internal.views.lightweightOutline;

import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

class OutlineContentProvider implements ITreeContentProvider {


	private static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];


	/**
	 * Creates a new Outline content provider.
	 *
	 * @param showInheritedMembers <code>true</code> iff inherited members are shown
	 */
	OutlineContentProvider() {

	}

	@Override
	public Object[] getChildren(Object element) {
		if(element instanceof PrologSourceFileModel) {
			return ((PrologSourceFileModel)element).getElements();
		}	
		if(element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).getChildren();
		}
		return null;
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

	}

	@Override
	public void dispose() {

	}

	@Override
	public Object[] getElements(Object element) {
		if (element instanceof PrologSourceFileModel) {
			if (((PrologSourceFileModel)element).hasChildren()) {
				return ((PrologSourceFileModel)element).getElements();
			}
		} else if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).getChildren();
		}
		return EMPTY_OBJECT_ARRAY;
	}


	@Override
	public Object getParent(Object element) {
		if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement) element).getParent();
		} else {
			return EMPTY_OBJECT_ARRAY;
		}
	}


	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).hasChildren();
		} else {
			return false;
		}
	}
}


