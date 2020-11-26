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

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.jdt.junit.model.ITestCaseElement;
import org.eclipse.jdt.junit.model.ITestSuiteElement;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;

public class OpenJUnitWrapperAction implements IObjectActionDelegate {

	private String fileName;
	private int lineNumber;



	@Override
	public void run(IAction action) {
		try {
			PDTCommonUtil.selectInEditor(lineNumber, fileName, false);
		} catch (PartInitException e) {
			Debug.report(e);
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		
		if(!(selection instanceof ITreeSelection)){
			return;
		}
		ITreeSelection tree = (ITreeSelection)selection;
		
		ITestCaseElement tc = (ITestCaseElement)tree.getFirstElement();
		fileName = ((ITestSuiteElement)tc.getParentContainer().getParentContainer()).getSuiteTypeName();
		String[] args= tc.getTestMethodName().substring(6,tc.getTestMethodName().length()-1).split(":");
//		String[] fileNum= args[1].split(";");
//		fileName = fileNum[0];
		if(tc.getFailureTrace()!=null && tc.getFailureTrace().getTrace().startsWith("java.lang.AssertionError: Failed assertion in line ")){
			String trace = tc.getFailureTrace().getTrace();
			trace = trace.substring("java.lang.AssertionError: Failed Assertion in Line ".length(),trace.length());
			StringBuffer buf = new StringBuffer();
			int i = 0;
			while(trace.charAt(i) >='0' && trace.charAt(i) <='9' ){
				buf.append(trace.charAt(i));
				i++;
			}
			lineNumber = Integer.parseInt(buf.toString());
						
		} else

		lineNumber = Integer.parseInt(args[1]);
		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub

	}

}


