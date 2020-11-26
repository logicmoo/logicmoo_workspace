/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.internal.externalproject;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;

public class DeepListFilesHandler extends AbstractHandler {
	
	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection currentSelectionChecked = HandlerUtil.getCurrentSelectionChecked(event);
		if (!(currentSelectionChecked instanceof IStructuredSelection)) {
			return null;
		}
		Object element = ((IStructuredSelection) currentSelectionChecked).getFirstElement();
		if (element instanceof IFile) {
			IFile file = (IFile) element;
			IContainer parent = file.getParent();
			if (parent instanceof IFolder) {
				new FolderKeeper().deepClearAndListAll((IFolder) parent);
			}
		} else if (element instanceof IFolder) {
			new FolderKeeper().deepClearAndListAll((IFolder) element);
		}
		
		return null;
	}
}
