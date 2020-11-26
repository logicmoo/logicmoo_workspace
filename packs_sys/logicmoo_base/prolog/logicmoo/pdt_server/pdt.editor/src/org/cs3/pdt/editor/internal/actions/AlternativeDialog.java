/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.editor.internal.actions;

import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PartInitException;

class AlternativeDialog extends Dialog {

	private List<Map<String, Object>> alternatives;
	private org.eclipse.swt.widgets.List list;
	private String description;

	protected AlternativeDialog(Shell parentShell, List<Map<String, Object>> alternatives, String description) {
		super(parentShell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		this.alternatives = alternatives;
		this.description = description;
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		
		Label label = new Label(composite, SWT.WRAP);
		label.setText(description);
		
	    GridData gridData = new GridData();
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.horizontalAlignment = GridData.FILL;
	    gridData.heightHint = convertHeightInCharsToPixels(3);
	    
	    label.setLayoutData(gridData);
		
		list = new org.eclipse.swt.widgets.List(composite, SWT.BORDER);
		for (Map<String, Object> alternative : alternatives) {
			list.add(getTextForPred(alternative));
		}
		list.setSelection(0);
		list.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				if (list.getSelectionIndex() >= 0) {
					AlternativeDialog.this.okPressed();
				}
			}
		});
		
	    gridData = new GridData();
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.horizontalAlignment = GridData.FILL;
	    gridData.grabExcessVerticalSpace = true;
	    gridData.verticalAlignment = GridData.FILL;
	    
	    list.setLayoutData(gridData);

		return composite;
	}
	
	private String getTextForPred(Map<String, Object> predicate) {
		return (String) predicate.get("TargetLabel");
	}
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
	}
	
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(FindPredicateActionDelegate.NAME);
	}
	
	@Override
	protected Point getInitialSize() {
		return new Point(400, 300);
	}
	
	@Override
	protected void okPressed() {
		int selection = list.getSelectionIndex();
		if (selection >= 0) {
			Map<String, Object> predicate = alternatives.get(selection);
			if (!"-1".equals(predicate.get("TargetLine"))) {
				try {
					PDTCommonUtil.selectInEditor(Integer.parseInt(predicate.get("TargetLine").toString()), predicate.get("TargetFile").toString(), true);
				} catch (PartInitException e) {
					Debug.report(e);
				} catch (NumberFormatException e) {
					Debug.report(e);
				}
			}
		}
		super.okPressed();
	}

}