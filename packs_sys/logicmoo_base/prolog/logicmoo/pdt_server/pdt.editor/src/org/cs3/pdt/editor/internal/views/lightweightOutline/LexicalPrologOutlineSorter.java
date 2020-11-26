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

class LexicalPrologOutlineSorter extends PositionalPrologOutlineSorter {

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if ((e1 instanceof OutlineModuleElement) && (e2 instanceof OutlineModuleElement)) {
			OutlineModuleElement m1 = (OutlineModuleElement) e1;
			OutlineModuleElement m2 = (OutlineModuleElement) e2;
			if (m1.fileEqualToEditorFile() && m2.fileEqualToEditorFile()) {
				return m1.getName().compareTo(m2.getName());
			}
		} else if ((e1 instanceof OutlinePredicateElement) && (e2 instanceof OutlinePredicateElement)) {
			return ((OutlinePredicateElement)e1).getFunctor().compareTo(((OutlinePredicateElement)e2).getFunctor());
		} else if ((e1 instanceof OutlineClauseElement) && (e2 instanceof OutlineClauseElement)) {
			OutlineClauseElement c1 = (OutlineClauseElement)e1;
			OutlineClauseElement c2 = (OutlineClauseElement)e2;
			if (c1.getFirstArgument() == null && c2.getFirstArgument() != null) {
				return 1;
			} else if (c1.getFirstArgument() != null && c2.getFirstArgument() == null) {
				return -1;
			} else if (c1.getFirstArgument() != null && c2.getFirstArgument() != null) {
				return c1.getFirstArgument().compareTo(c2.getFirstArgument());
			}
		}
		return super.compare(viewer, e1, e2);
	}
}

