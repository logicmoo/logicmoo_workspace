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

package org.cs3.pdt.console.internal;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.connector.service.PDTReloadExecutor;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.views.SingleCharModeException;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

public class ConsoleReloadExecutor implements PDTReloadExecutor {
	
	@Override
	public int getPriority() {
		return 1000;
	}
	
	@Override
	public boolean executePDTReload(PrologProcess process, String query, IProgressMonitor monitor) throws PrologProcessException {
		monitor.beginTask("", 1);
		
		try {
			PrologConsole activePrologConsole = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
			if (activePrologConsole == null) {
			}
			PrologProcess activeConsoleProcess = activePrologConsole.getPrologProcess();
			if (activeConsoleProcess == null || !activeConsoleProcess.equals(process)) {
				return false;
			}
			return executeQueryOnConsole(activePrologConsole, query);
		} finally {
			monitor.done();
		}
	}
	
	private boolean executeQueryOnConsole(PrologConsole activePrologConsole, String query) {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				IWorkbenchPage activePage = UIUtils.getActivePage();
				if (activePage != null) {
					try {
						boolean focusToConsole = !PDTCommonPlugin.getDefault().getPreferenceStore().getBoolean("console.no.focus");
						if (focusToConsole) {
							activePage.showView(PDTConsole.CONSOLE_VIEW_ID);
						} else {
							activePage.showView(PDTConsole.CONSOLE_VIEW_ID, null, IWorkbenchPage.VIEW_VISIBLE);
							PDTCommonPlugin.getDefault().getPreferenceStore().setValue("console.no.focus", false);
						}
					} catch (PartInitException e) {
						Debug.report(e);
					}
				}
			}
		});
		activePrologConsole.ensureConnectionForCurrentPrologProcess();
		
		ConsoleModel model = activePrologConsole.getModel();
		model.setLineBuffer(" ");
		try {
			model.commitLineBuffer();
		} catch (SingleCharModeException e) {
			final Shell shell = UIUtils.getActiveShell();
			shell.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					UIUtils.displayMessageDialog(shell, "Prolog Console", "Consult operation has been performed in the background, without Console feedback, since the Prolog Console is waiting for user input. \nIf you are not interested in more results terminate the query by typing <return>.");
				}
			});
			return false;
		}
		if (query.endsWith(".")) {
			model.setLineBuffer(query);
		} else {
			model.setLineBuffer(query + ".");
		}
		model.commitLineBuffer();
		return true;
	}

}


