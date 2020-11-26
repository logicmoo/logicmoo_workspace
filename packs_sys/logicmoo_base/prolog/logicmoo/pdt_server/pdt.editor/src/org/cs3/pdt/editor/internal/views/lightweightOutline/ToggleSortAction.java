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

import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.PDTPlugin;
import org.cs3.pdt.editor.internal.ImageRepository;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;

public class ToggleSortAction extends Action {
	private TreeViewer treeViewer;

	public ToggleSortAction(TreeViewer treeViewer) {
		super("Sort", AS_CHECK_BOX);
		this.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SORT));
		this.treeViewer = treeViewer;
		String val = PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		if("true".equalsIgnoreCase(val)){
			setChecked(true);
			treeViewer.setSorter(new LexicalPrologOutlineSorter());	
		}
		else{
			setChecked(false);
			treeViewer.setSorter(new PositionalPrologOutlineSorter());
		}
	}

	@Override
	public void run() {
		if(isChecked()){
			treeViewer.setSorter(new LexicalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "true");
		}
		else{
			treeViewer.setSorter(new PositionalPrologOutlineSorter());
			PDTPlugin.getDefault().setPreferenceValue(PDT.PREF_OUTLINE_SORT, "false");
		}
	}
}


