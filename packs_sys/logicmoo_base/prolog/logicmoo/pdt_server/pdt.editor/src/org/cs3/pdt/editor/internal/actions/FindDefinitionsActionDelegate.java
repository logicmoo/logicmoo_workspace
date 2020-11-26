/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.actions;

import java.util.ResourceBundle;

import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.queries.ContextAwareDefinitionsSearchQuery;
import org.cs3.pdt.editor.PDT;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindDefinitionsActionDelegate extends SearchActionDelegate {
	public FindDefinitionsActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),FindDefinitionsActionDelegate.class.getName(), editor);
	}

	@Override
	protected ISearchQuery connectSearchQuery(Goal data) {
		ISearchQuery query = new ContextAwareDefinitionsSearchQuery(data);
		return query;
	}
	
}


