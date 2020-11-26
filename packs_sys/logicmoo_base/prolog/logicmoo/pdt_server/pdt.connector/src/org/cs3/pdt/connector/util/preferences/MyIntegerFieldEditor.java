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

import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class MyIntegerFieldEditor extends IntegerFieldEditor implements FieldEditorForStructuredPreferencePage {

    private Composite parent;

	public MyIntegerFieldEditor(String name, String labelText, Composite parent) {
    	super(name, labelText, parent);
    	this.parent = parent;
    }
    
    public MyIntegerFieldEditor(String name, String labelText, Composite parent, int textLimit) {
    	super(name, labelText, parent, textLimit);
    	this.parent = parent;
    }

	@Override
	public void adjustColumns(int numColumns) {
		adjustForNumColumns(numColumns);
	}

	@Override
	public Composite getParent() {
		return parent;
	}
	
	public void setIntValue(int value) {
		getTextControl().setText(Integer.toString(value));
	}

}


