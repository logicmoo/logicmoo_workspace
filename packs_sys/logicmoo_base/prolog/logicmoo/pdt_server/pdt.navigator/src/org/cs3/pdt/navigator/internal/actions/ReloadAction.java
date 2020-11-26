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

package org.cs3.pdt.navigator.internal.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class ReloadAction implements IObjectActionDelegate  {

	private IStructuredSelection selection;

	public ReloadAction() {
	}

	@Override
	public void run(IAction action) {
		List<IFile> filesToConsult = new ArrayList<IFile>();
		for (Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			Object obj = iter.next();
			
			if (obj instanceof IFile) {
				filesToConsult.add((IFile) obj);
			}
		}
		
		if (filesToConsult.size() == 1) {
			PDTConnectorPlugin.getDefault().getPrologProcessService().consultFile(filesToConsult.get(0));
//			new ConsultAction().consultWorkspaceFile(filesToConsult.get(0));
		} else {
			PDTConnectorPlugin.getDefault().getPrologProcessService().consultFiles(filesToConsult);
//			new ConsultAction().consultWorkspaceFiles(filesToConsult);
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			this.selection = (IStructuredSelection) selection;
		}

	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

}


