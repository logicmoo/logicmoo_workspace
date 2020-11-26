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

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;

public class ToggleEntryPointAction implements IActionDelegate {

	public ToggleEntryPointAction() {
	}

	@Override
	public void run(IAction action) {
		if (selectedFiles != null) {
			
			PrologProcess process = PDTCommonUtil.getActivePrologProcess();
			if (isSelectionChecked()) {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, false, process);
				}
			} else {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, true, process);
				}
			}
			PDTCommonPlugin.getDefault().notifyDecorators();
			
		}
		
	}

	Set<IFile> selectedFiles;
	
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		selectedFiles = new HashSet<IFile>();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection selections = (IStructuredSelection) selection;

			for (Iterator<?> iter = selections.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFile) {
					selectedFiles.add((IFile) obj);
				}
			}
			action.setChecked( isSelectionChecked() );
		}
		
	}

	private boolean isSelectionChecked() {
		boolean result = false;
		if (selectedFiles != null && selectedFiles.size() > 0) {
			result = true;
			for (IFile f : selectedFiles) {
				if (!isEntryPoint(f)) {
					result = false;
					break;
				}
			}
		}
		return result;
	}
	
	private boolean isEntryPoint(IFile file) {
		return PDTCommonPlugin.getDefault().isEntryPoint(file);
	}
	
	private void setEntryPoint(IFile file, boolean b, PrologProcess process) {
		if (b) {
			PDTCommonPlugin.getDefault().addEntryPoint(file);
		} else {
			PDTCommonPlugin.getDefault().removeEntryPoint(file);
		}
		
		if (process != null) {
			try {
				String prologFileName = QueryUtils.prologFileNameQuoted(file.getLocation().toFile().getCanonicalFile());
				
				if (b) {
					process.queryOnce(bT(PDTCommonPredicates.ADD_ENTRY_POINT, prologFileName));
				} else {
					process.queryOnce(bT(PDTCommonPredicates.REMOVE_ENTRY_POINTS, prologFileName));
				}
			} catch (IOException e) {
				Debug.report(e);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
			
			
		}
	}


}


