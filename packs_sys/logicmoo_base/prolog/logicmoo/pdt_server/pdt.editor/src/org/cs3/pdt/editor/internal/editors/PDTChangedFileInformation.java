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

package org.cs3.pdt.editor.internal.editors;

import org.cs3.pdt.common.PDTCommonUtil;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;

public class PDTChangedFileInformation implements ISelection{
	private IEditorInput editorInput;
	
	public PDTChangedFileInformation(IEditorInput input) {
		editorInput = input;
	}
	
	public String getPrologFileName() {
		return PDTCommonUtil.prologFileName(editorInput);
	}

	@Override
	public boolean isEmpty() {
		return false;
	}
	
}


