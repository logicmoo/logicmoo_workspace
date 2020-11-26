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

package org.cs3.pdt.graphicalviews.preferences;

import java.util.LinkedList;
import java.util.List;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.cs3.pdt.graphicalviews.main.PreferencesUpdateListener;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public abstract class PreferencePageBase 
		extends FieldEditorPreferencePage 
		implements IWorkbenchPreferencePage, PreferencesUpdateListener {

	private List<FieldEditor> editors = new LinkedList<FieldEditor>();
	private GridData parentData;
	
	protected static GridLayout groupLayout;
	protected static GridData cellData;
	protected static GridData defaultAligmentData;
	
	public PreferencePageBase() {
		super(GRID);
		setPreferenceStore(PluginActivator.getDefault().getPreferenceStore());
		PluginActivator.getDefault().addPreferencesUpdateListener(this);
		
		parentData = new GridData(GridData.FILL_HORIZONTAL);
        parentData.horizontalSpan = 2;
        parentData.verticalIndent = 5;
        
        defaultAligmentData = new GridData();
        defaultAligmentData.horizontalSpan = 2;
        defaultAligmentData.horizontalIndent = 9;
        defaultAligmentData.verticalIndent = 5;

		groupLayout = new GridLayout();
		groupLayout.marginWidth = 10;
        groupLayout.marginHeight = 10;
        groupLayout.numColumns = 2;
        
        cellData = new GridData();
        cellData.widthHint = 200;
        cellData.heightHint = 32;
	}
	
	@Override
	public void init(IWorkbench workbench) {
	}
	
	@Override
	public boolean performOk() {
		boolean res = super.performOk();
		if (res) {
			PluginActivator.getDefault().preferencesUpdated();
		}
		return res;
	}
	
	@Override
	protected void addField(FieldEditor editor) {
		editors.add(editor);
		super.addField(editor);
	}
	
	@Override
	public void preferencesUpdated() {
		for (FieldEditor e : editors) {
			if (e != null) {
				e.load();
			}
		}
	}
	
	@Override
	public void dispose() {
		PluginActivator.getDefault().removePreferencesUpdateListener(this);
		super.dispose();
	}
	
	protected Group createGroup(String title, Layout groupLayout) {
		
		Group predicateBgColor = new Group(getFieldEditorParent(), SWT.NONE);
		predicateBgColor.setText(title);
		predicateBgColor.setLayout(groupLayout);
		predicateBgColor.setLayoutData(parentData);
		return predicateBgColor;
	}
	
	public Composite wrap(Composite parent) {
		return wrap(parent, null);
	}
	
	public Composite wrap(Composite parent, Object layoutData) {
		
		Composite w = new Composite(parent, SWT.NONE);
		if (layoutData == null) {
			layoutData = defaultAligmentData;
		}
		w.setLayoutData(layoutData);
		return w;
	}
}


