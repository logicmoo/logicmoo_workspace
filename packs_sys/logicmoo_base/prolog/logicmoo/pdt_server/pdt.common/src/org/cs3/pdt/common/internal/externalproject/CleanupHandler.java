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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;

public class CleanupHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection currentSelectionChecked = HandlerUtil.getCurrentSelectionChecked(event);
		if (!(currentSelectionChecked instanceof IStructuredSelection)) {
			return null;
		}
		Object element = ((IStructuredSelection) currentSelectionChecked).getFirstElement();
		if (element instanceof IProject) {
			IProject project = (IProject) element;
			try {
				project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
				new FolderKeeper().clearProject(project);
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}
		return null;
	}

}
