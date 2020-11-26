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

import java.io.IOException;
import java.util.HashMap;

import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.graphicalviews.focusview.LogtalkView.DiagramType;
import org.cs3.pdt.graphicalviews.focusview.LogtalkView.InputType;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;

public class LogtalkViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	final LogtalkView focusView;
	
	private String lastPath;
	
	public LogtalkViewCoordinator(ViewBase focusView) {
		super(focusView);
		this.focusView = (LogtalkView) focusView;
	}
	
	@Override
	public void swichFocusView(String path) {
		lastPath = path;
		try {
			if (path == null && focusView.getInputType() == InputType.PROJECT) {
				return;
			}
			String projectName = null;
			if (path != null) {
				IFile fileForLocation = FileUtils.findFileForLocation(path);
				if (fileForLocation != null) {
					projectName= fileForLocation.getProject().getName();
				}
			}
		
			String signature = getSignature(projectName, focusView.getDiagramType(), focusView.getInputType(), focusView.getCurrentLibrary());
			currentFocusView = views.get(signature);
			
			if (currentFocusView == null) {
				PDTGraphView pdtGraphView = new PDTGraphView(focusView);
				GraphProcessLoaderBase loader = focusView.createGraphProcessLoader(pdtGraphView);
				loader.setCurrentPath(path);
				
				currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);
	
				refreshCurrentView();
				
				views.put(signature, currentFocusView);
			}
	
			currentFocusView.recalculateMode();
			focusView.setCurrentFocusView(currentFocusView);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void diagramSettingsChanged() {
		new UIJob("Update View") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				swichFocusView(lastPath);
				return Status.OK_STATUS;
			}
		}.schedule();
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphProcessLoader)currentFocusView.getProcessLoader()).containsFilePath(path);
	}
	
	private String getSignature(String projectName, DiagramType diagramType, InputType inputType, String library) {
		String signature;
		if (inputType == InputType.PROJECT) {
			signature = diagramType.getLabel() + "/" + projectName;
		} else {
			signature = inputType.getLabel() + "/" + diagramType.getLabel() + "/" + library;
		}
		return signature;
	}
}
