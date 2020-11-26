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

package org.cs3.pdt.editor.internal.editors;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.PrologProcessStartListener;
import org.cs3.pdt.connector.service.ActivePrologProcessListener;
import org.cs3.pdt.editor.PDTPredicates;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologEvent;
import org.cs3.prolog.connector.process.PrologEventDispatcher;
import org.cs3.prolog.connector.process.PrologEventListener;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;

public class CurrentProcessListener implements PrologEventListener, ActivePrologProcessListener, PrologProcessStartListener {

	@Override
	public void update(PrologEvent e) {
		if (e.getSubject().equals(PDTPredicates.PDT_EDIT_HOOK)) {
			openFileInEditor(e.getData());
		}
	}

	public void openFileInEditor(String event) {
		if (event.startsWith("'")) {
			event = event.substring(1);
		}
		if (event.endsWith("'")) {
			event = event.substring(0, event.length() - 1);
		}
		int index = event.lastIndexOf(" ");
		final String fileName = event.substring(0, index);
		final int lineNumber = Integer.parseInt(event.substring(index + 1));
		Debug.debug("Try to open PLEditor for " + fileName + " (line " + lineNumber + ")");
		Runnable r = new Runnable() {
			@Override
			public void run() {
				IEditorPart editorPart = PDTCommonUtil.openInEditor(fileName);
				if (editorPart != null && editorPart instanceof PLEditor){
					((PLEditor) editorPart).gotoLine(lineNumber);
				}
			}
		};
		Display.getDefault().syncExec(r);
	}

	PrologProcess currentProcess;
	private PrologEventDispatcher currentDispatcher;
	
	private void addProcessListener() {
		if (currentProcess != null) {
			Debug.debug("add edit registry listener for process " + currentProcess.toString());
			currentDispatcher = new PrologEventDispatcher(currentProcess);
			try {
				currentDispatcher.addPrologEventListener(PDTPredicates.PDT_EDIT_HOOK, this);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
			
		}
	}
	private void removeProcessListener() {
		if (currentProcess != null && currentDispatcher != null) {
			Debug.debug("remove edit registry listener for process " + currentProcess.toString());
			try {
				currentDispatcher.removePrologEventListener(PDTPredicates.PDT_EDIT_HOOK, this);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
		}
	}

	private void updateEntryPoints() {
		try {
			PDTCommonUtil.updateEntryPointsInProcess(currentProcess);
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
	}

	@Override
	public void activePrologProcessChanged(PrologProcess process) {
		if (currentProcess == process) {
			return;
		}
		
		removeProcessListener();
		
		currentProcess = process;
		
		addProcessListener();
		
		updateEntryPoints();
	}

	@Override
	public void prologProcessStarted(PrologProcess process) {
		if (process.equals(currentProcess)) {
			updateEntryPoints();
		}
	}

}


