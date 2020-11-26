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

import org.cs3.pdt.editor.internal.structureElements.OutlineClauseElement;
import org.cs3.pdt.editor.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.editor.internal.structureElements.OutlinePredicateElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

public class HideSystemPredicatesFilter extends ViewerFilter {

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof OutlineClauseElement) {
			return true;
		} else if (element instanceof OutlinePredicateElement) {
			OutlinePredicateElement p = (OutlinePredicateElement) element;
			return !p.getFunctor().startsWith("$");
		} else if (element instanceof OutlineModuleElement) {
			OutlineModuleElement m = (OutlineModuleElement) element;
			for (Object child: m.getChildren()) {
				if (select(viewer, element, child)) {
					return true;
				}
			}
			return false;
		} else {
			return true;
		}
	}

}


