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

import java.util.HashMap;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

public abstract class StructuredFieldEditorPreferencePage extends FieldEditorPreferencePage {

	public StructuredFieldEditorPreferencePage() {
		super();
	}

    protected StructuredFieldEditorPreferencePage(int style) {
        super(style);
    }

    protected StructuredFieldEditorPreferencePage(String title, int style) {
        super(title, style);
    }

    protected StructuredFieldEditorPreferencePage(String title, ImageDescriptor image, int style) {
        super(title, image, style);
    }
    
    private HashMap<Composite, EditorGroup> editorGroups = new HashMap<Composite, EditorGroup>();
    
    @Override
    protected void adjustGridLayout() {
    	for (EditorGroup eg : editorGroups.values()) {
    		eg.adjustGroupAndEditors();
    	}
    }
    
    protected void adjustLayoutForElement(Composite element) {
    	Composite parent = element.getParent();
    	EditorGroup editorGroup = editorGroups.get(parent);
    	if (editorGroup != null) {
    		Object layoutData = element.getLayoutData();
    		if (layoutData instanceof GridData) {
    			((GridData) layoutData).horizontalSpan = editorGroup.getMaxColumns();
    			((GridData) layoutData).horizontalAlignment = SWT.FILL;
    		} else {
    			element.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false, editorGroup.getMaxColumns(), 1));
    		}
    	}
    }

    @Override
    protected void addField(FieldEditor editor) {
    	super.addField(editor);
    	if (editor instanceof FieldEditorForStructuredPreferencePage) {
    		Composite parent = ((FieldEditorForStructuredPreferencePage) editor).getParent();
    		EditorGroup editorGroup = editorGroups.get(parent);
			if (editorGroup == null) {
				editorGroup = new EditorGroup(parent);
				editorGroups.put(parent, editorGroup);
			}
			editorGroup.addEditor(editor);
    	}
    }
}


