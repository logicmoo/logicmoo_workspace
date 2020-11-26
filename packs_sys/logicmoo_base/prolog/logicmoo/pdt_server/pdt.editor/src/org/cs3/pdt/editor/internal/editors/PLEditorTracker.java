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

package org.cs3.pdt.editor.internal.editors;

import org.cs3.pdt.connector.AbstractPrologContextTracker;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;


public class PLEditorTracker extends AbstractPrologContextTracker implements IPartListener2 {

	

	

	@Override
	public PrologProcess getCurrentPrologProcess(){
		return null;
	}

	@Override
	public void init(IWorkbench workbench) {
		IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
		IPartService partService = activeWorkbenchWindow.getPartService();
		partService.addPartListener(this);
		//FIXME: TRHO: Do not fire context changed on startup. Results in an unavailable prolog interface although another tracker was successful! 
		//fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef)  {
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
		;//check(partRef);
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		;//check(partRef);
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


