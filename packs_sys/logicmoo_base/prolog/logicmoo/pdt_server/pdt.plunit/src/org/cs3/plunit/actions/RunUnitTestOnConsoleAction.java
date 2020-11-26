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

package org.cs3.plunit.actions;

import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jdt.junit.model.ITestCaseElement;
import org.eclipse.jdt.junit.model.ITestSuiteElement;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class RunUnitTestOnConsoleAction  implements IObjectActionDelegate {
	

	private String unitName;
	private String testName;



	@Override
	public void run(IAction action) {
//		String[] fileNum= args[1].split(";");
//		fileName = fileNum[0];
		PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		ConsoleModel model = console.getModel();
		model.setLineBuffer(" ");
		model.commitLineBuffer();
		model.setLineBuffer("run_tests(" + unitName +":"+ testName + ").");
		model.commitLineBuffer();

	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		
		if(!(selection instanceof ITreeSelection)){
			return;
		}
		ITreeSelection tree = (ITreeSelection)selection;
		
		ITestCaseElement tc = (ITestCaseElement)tree.getFirstElement();
		unitName = ((ITestSuiteElement)tc.getParentContainer()).getSuiteTypeName();
		testName = tc.getTestMethodName().substring(2,tc.getTestMethodName().length()-1).split(":")[0];
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub

	}

}


