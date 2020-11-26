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

import org.cs3.pdt.common.callhierachy.CallHierarchyUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

public class OpenCallHierarchyActionDelegte extends TextEditorAction {
	
	public OpenCallHierarchyActionDelegte(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI), OpenCallHierarchyActionDelegte.class.getName(), editor);
	}
	
	@Override
	public void run() {
		PLEditor editor = (PLEditor) getTextEditor();
		try {
			Goal goal = editor.getSelectedPrologElement();
			if (goal == null) {
				UIUtils.displayMessageDialog(editor.getSite().getShell(), "Open Call Hierarchy", "Cannot locate a predicate at the specified location.");
				return;
			}
			String module = goal.getModule();
			if (module != null && !ParserUtils.isVarPrefix(module)) {
				CallHierarchyUtil.showCallHierarchyFor(editor.getSite().getWorkbenchWindow(), module, goal.getFunctor(), goal.getArity());
			} else {
				String prologFileName = goal.getFilePath();
				CallHierarchyUtil.showCallHierarchyFor(editor.getSite().getWorkbenchWindow(), goal.getFunctor(), goal.getArity(), prologFileName);
			}
		} catch (Exception e) {
			Debug.report(e);
		}
	}
}


