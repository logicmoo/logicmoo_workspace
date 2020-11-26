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

package org.cs3.pdt.graphicalviews.preferences.controls;

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.REFRESH_MODE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.REFRESH_MODE_AUTOMATIC;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.REFRESH_MODE_MANUAL;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

public class RefreshModeFieldEditor extends FieldEditor {

	Button rbManual;
	Button rbAutomatic;
	String value;
	
	public RefreshModeFieldEditor(Composite parent) {
		init(REFRESH_MODE, "Refresh Mode");
		createControl(parent);
	}
	
	@Override
	public int getNumberOfControls() {
		return 1;
	}
	
	@Override
	protected void adjustForNumColumns(int numColumns) { }

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
//		Control control = getLabelControl(parent);
		//GridData gdInfo = new GridData();
        //gdInfo.horizontalSpan = numColumns;
        //gdInfo.horizontalIndent = 9;
        //gdInfo.verticalIndent = 5;
        //control.setLayoutData(gdInfo);
        
        //Label text = new Label(parent, SWT.NONE);
        //text.setText("Specify when to redraw the Focus View.");
        //text.setLayoutData(gdInfo);
        
//		control = getRadioBoxControl(parent);
		//GridData gdRadio = new GridData(GridData.FILL_HORIZONTAL);
        //gdRadio.horizontalSpan = numColumns;
        //gdRadio.verticalIndent = 10;
        //control.setLayoutData(gdRadio);
        
        //Label note = new Label(parent, SWT.NONE);
        //FontData fontData = note.getFont().getFontData()[0];
        //Font font = new Font(parent.getDisplay(), new FontData(fontData.getName(), fontData.getHeight(), SWT.ITALIC));
        //note.setFont(font);
        //note.setText("NOTE: Refreshing undoes any manual layout of the graph!");
        //note.setLayoutData(gdInfo);
	}
	
	protected Composite getRadioBoxControl(Composite parent) {
		
		Group radioBox = new Group(parent, SWT.NONE);
		
		//radioBox.setText("Refresh Mode");
		
		GridLayout layout = new GridLayout();
        layout.marginWidth = 10;
        layout.marginHeight = 10;
        layout.horizontalSpacing = HORIZONTAL_GAP;
        layout.numColumns = 1;
        radioBox.setLayout(layout);
		
        rbManual = createRadioButton(radioBox, "&Using Refresh Button", REFRESH_MODE_MANUAL);
        rbAutomatic = createRadioButton(radioBox, "&Automatically (when saving file)", REFRESH_MODE_AUTOMATIC);
		
		return radioBox;
	}

	public Button createRadioButton(Composite parent, final String text, final String data) {
		Button radio = new Button(parent, SWT.RADIO | SWT.LEFT);
	    radio.setText(text);
	    radio.setData(data);
	    radio.setFont(parent.getFont());
	    radio.addSelectionListener(new SelectionAdapter() {
	        @Override
			public void widgetSelected(SelectionEvent event) {
	            String oldValue = value;
	            value = (String) event.widget.getData();
	            setPresentsDefaultValue(false);
	            fireValueChanged(REFRESH_MODE, oldValue, value);
	        }
	    });
	    
		return radio;
	}


	@Override
	protected void doLoad() {
		value = getPreferenceStore().getString(REFRESH_MODE);
		updateRadioButtons(value);
	}

	@Override
	protected void doLoadDefault() {
		value = null;
		updateRadioButtons(getPreferenceStore().getDefaultString(REFRESH_MODE));
	}

	private void updateRadioButtons(String refreshMode) {
		if (rbManual == null 
				|| rbAutomatic == null 
				|| refreshMode == null) {
			return;
		}
		
		rbManual.setSelection(REFRESH_MODE_MANUAL.equals(refreshMode));
		rbAutomatic.setSelection(REFRESH_MODE_AUTOMATIC.equals(refreshMode));
	}

	@Override
	protected void doStore() {
		IPreferenceStore prefs = getPreferenceStore();
		
		if (value == null) {
            prefs.setToDefault(REFRESH_MODE);
        }
		else {
			prefs.setValue(REFRESH_MODE, value);
		}
	}

}


