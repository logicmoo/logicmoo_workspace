/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.graphicalviews.focusview;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.progress.UIJob;


public abstract class ViewCoordinatorBase implements IPartListener, ConsultListener  {
	
	protected ViewBase focusView;
	protected ViewBase.FocusViewControl currentFocusView;

	public ViewCoordinatorBase(ViewBase focusView) {
		this.focusView = focusView;

		
		focusView.getSite().getWorkbenchWindow().getPartService().addPartListener(this);
		
		PDTConnectorPlugin.getDefault().getPrologProcessService()
			.registerConsultListener(this);
	}
	
	public abstract void swichFocusView(String path);

	protected abstract boolean isCurrentFocusViewActualFor(String path);
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		if (focusView.getViewContainer().isDisposed()) {
			return;
		}
		if (part instanceof IEditorPart) {
			IEditorPart editorPart = (IEditorPart) part;
			final String fileName = PDTCommonUtil.prologFileName(editorPart.getEditorInput());
			if (!fileName.endsWith(".pl") && !fileName.endsWith(".pro") && !fileName.endsWith(".lgt") && !fileName.endsWith(".logtalk")) {
				return;
			}
			if (currentFocusView == null 
					|| !isCurrentFocusViewActualFor(fileName)) {
				
				new UIJob("Update View") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						
						swichFocusView(fileName);
						
						if (currentFocusView.isEmpty()){
							focusView.setStatusText("[Please activate prolog console, set focus on file and press F9 to load graph]");
						}
						
						return Status.OK_STATUS;
					}
				}.schedule();
			}
		}
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
	}
	
	@Override
	public void beforeConsult(PrologProcess process, List<IFile> files,
			IProgressMonitor monitor) throws PrologProcessException { }

	@Override
	public void afterConsult(PrologProcess process, List<IFile> files,
			List<String> allConsultedFiles, IProgressMonitor monitor)
			throws PrologProcessException {
		
		refreshCurrentView();
	}
	
	protected void refreshCurrentView() {
		if (currentFocusView != null) {
			currentFocusView.reload();
		}
	}
	
	public void dispose() {
		focusView.getSite().getWorkbenchWindow().getPartService().removePartListener(this);
		
		PDTConnectorPlugin.getDefault().getPrologProcessService().unRegisterConsultListener(this);
	}
}
