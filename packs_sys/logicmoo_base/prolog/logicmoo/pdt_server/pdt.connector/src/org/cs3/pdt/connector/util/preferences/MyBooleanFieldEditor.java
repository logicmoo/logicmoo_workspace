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

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyBooleanFieldEditor extends BooleanFieldEditor implements FieldEditorForStructuredPreferencePage{
	
	public MyBooleanFieldEditor(String name, String labelText, int style, Composite parent) {
		super(name, labelText, style, parent);
    	this.parent = parent;
	}
	
	public MyBooleanFieldEditor(String name, String label, Composite parent) {
		super(name, label, parent);
    	this.parent = parent;
	}
	
	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}
	
    private Composite parent;
	
	@Override
	public Composite getParent() {
		return parent;
	}
	
	public void setBooleanValue(boolean value) {
		getChangeControl(parent).setSelection(value);
	}
	
}


