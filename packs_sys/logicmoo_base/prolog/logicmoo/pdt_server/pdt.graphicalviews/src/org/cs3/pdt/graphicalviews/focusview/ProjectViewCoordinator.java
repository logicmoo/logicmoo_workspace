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

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.eclipse.core.resources.IProject;

public class ProjectViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public ProjectViewCoordinator(ViewBase focusView)
	{
		super(focusView);
		
		PDTConnectorPlugin.getDefault().getPrologProcessService()
			.registerConsultListener(this);
	}
	
		
	public void swichFocusView(String path) {
		try {
			IProject project = FileUtils.findFileForLocation(path).getProject();
		
			currentFocusView = views.get(project.getName());
			
			if (currentFocusView == null) {
				PDTGraphView pdtGraphView = new PDTGraphView(focusView);
				GraphProcessLoaderBase loader = focusView.createGraphProcessLoader(pdtGraphView);
				loader.setCurrentPath(path);
				
				currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);
	
				refreshCurrentView();
				
				views.put(project.getName(), currentFocusView);
			}
	
			currentFocusView.recalculateMode();
			focusView.setCurrentFocusView(currentFocusView);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphProcessLoader)currentFocusView.getProcessLoader()).containsFilePath(path);
	}
}
