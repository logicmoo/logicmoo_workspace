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

package org.cs3.pdt.graphicalviews.internal.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.progress.UIJob;

public abstract class ToolBarAction extends Action {
	private String name;
	private String toolTipText;
	private ImageDescriptor image;
	
	public ToolBarAction(String name, ImageDescriptor image) {
		this(name, name, image);
	}
	
	public ToolBarAction(String name, String toolTipText, ImageDescriptor image) {
		this.name = name;
		this.toolTipText = toolTipText;
		this.image = image;
	}
	
	@Override
	public void run(){
			new UIJob("Hierarchical layout")
			{
				@Override
				public IStatus runInUIThread(IProgressMonitor monitor) {
					performAction();
					return Status.OK_STATUS;
			}
		}.schedule();
	}
	
	public abstract void performAction();
	
	@Override
	public ImageDescriptor getImageDescriptor() {
		return image;
	}
	
	@Override
	public String getText() {
		return name;
	}
	
	@Override
	public String getToolTipText() {
		return toolTipText;
	}
}


