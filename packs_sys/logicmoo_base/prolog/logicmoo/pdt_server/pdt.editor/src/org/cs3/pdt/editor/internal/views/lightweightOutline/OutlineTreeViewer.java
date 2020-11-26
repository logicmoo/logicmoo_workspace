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

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.widgets.Tree;

class OutlineTreeViewer extends TreeViewer {

	OutlineTreeViewer(Tree tree) {
		super(tree);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Object[] getFilteredChildren(Object parent) {
		Object[] result = getRawChildren(parent);
		ViewerFilter[] filters = getFilters();
		if (filters != null) {
			for (int i= 0; i < filters.length; i++)
				result = filters[i].filter(this, parent, result);
		}
		return result;
	}
}


