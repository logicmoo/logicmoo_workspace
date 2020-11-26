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

package org.cs3.pdt.graphicalviews.internal.ui;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class PredicatesListDialog extends Dialog {

	String predicates;
	private org.eclipse.swt.widgets.List list;

	public PredicatesListDialog(Shell parentShell, String predicates) {
		super(parentShell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		this.predicates = predicates;
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		
		String[] predicateList = predicates.split("[\\[,\\]]");
		
		list = new org.eclipse.swt.widgets.List(composite, SWT.V_SCROLL | SWT.BORDER);
		for (String p : predicateList) {
			if (p.length() > 0)
				list.add(p);
		}
		list.setSelection(0);
		
		GridData gridData = new GridData();
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.horizontalAlignment = GridData.FILL;
	    gridData.grabExcessVerticalSpace = true;
	    gridData.verticalAlignment = GridData.FILL;
	    
	    list.setLayoutData(gridData);

		return composite;
	}
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
	}
	
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Predicates");
	}
	
	@Override
	protected Point getInitialSize() {
		return new Point(400, 300);
	}
}
