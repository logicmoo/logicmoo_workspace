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

package org.cs3.pdt.connector;

import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;

public abstract class AbstractEditorTracker extends AbstractPrologContextTracker implements IPartListener2
{

	public AbstractEditorTracker()
	{
		super();
	}

	public AbstractEditorTracker(String id, String label)
	{
		super(id, label);
	}

	@Override
	public void init(IWorkbench workbench) {
		workbench.getActiveWorkbenchWindow().getPartService().addPartListener(this);
		fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef) {
		if(partRef instanceof IEditorReference){
			if(getCurrentPrologProcess()!=null){
				fireContextChanged();
			}			
		}
	}
	
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {		
		check(partRef);		
	}	

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		check(partRef);	
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	

}


