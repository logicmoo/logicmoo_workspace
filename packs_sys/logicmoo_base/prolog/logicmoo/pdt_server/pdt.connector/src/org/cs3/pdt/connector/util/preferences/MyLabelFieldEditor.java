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

package org.cs3.pdt.connector.util.preferences;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

public class MyLabelFieldEditor extends FieldEditor implements FieldEditorForStructuredPreferencePage {

	private Text field;
	private Composite parent;
	
	public MyLabelFieldEditor(Composite parent, String name) {
		super("", name, parent);
		this.parent = parent;
	}

	@Override
	protected void adjustForNumColumns(int numColumns) {
        GridData gd = (GridData) field.getLayoutData();
        gd.horizontalSpan = numColumns - 1;
        gd.grabExcessHorizontalSpace = (gd.horizontalSpan == 1);
	}

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
        getLabelControl(parent);
        field = new Text(parent, SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
        field.setFont(parent.getFont());
        String text = getLabelText();
        if (text != null) {
        	field.setText(text);
		}
        field.addDisposeListener(new DisposeListener() {
            @Override
			public void widgetDisposed(DisposeEvent event) {
            	field = null;
            }
        });
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        field.setLayoutData(gd);
	}

	@Override
	protected void doLoad() {
	}

	@Override
	protected void doLoadDefault() {
	}

	@Override
	protected void doStore() {
	}

	@Override
	public int getNumberOfControls() {
		return 2;
	}

	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}

	@Override
	public Composite getParent() {
		return parent;
	}
	
	public void setText(String text) {
		field.setText(text);
	}

}


