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

import java.util.ArrayList;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

class EditorGroup {
	
	private Composite parent;
	private ArrayList<FieldEditor> editors = new ArrayList<FieldEditor>();
	private int maxColumns = 0;
	
	EditorGroup(Composite parent) {
		if (parent != null) {
			GridLayout layout = new GridLayout();
			parent.setLayout(layout);
			parent.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		}
		this.parent = parent;
	}
	
	void addEditor(FieldEditor e) {
		if (!editors.contains(e)) {
			editors.add(e);
			maxColumns = Math.max(maxColumns, e.getNumberOfControls());
		}
	}
	
	void adjustGroupAndEditors() {
		if (parent != null) {
			parent.setLayout(new GridLayout(maxColumns, false));
		}
		for (FieldEditor e : editors) {
			if (e instanceof FieldEditorForStructuredPreferencePage) {
				((FieldEditorForStructuredPreferencePage) e).adjustColumns(maxColumns);
			}
		}
	}
	
	int getMaxColumns() {
		return maxColumns;
	}
	
}


