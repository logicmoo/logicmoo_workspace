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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_INDIVIDUAL;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_MAXIMUM;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_MEDIAN;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_NUMBER_OF_LINES;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class NodeSizeRadioGroupFieldEditor extends FieldEditor {
	
	String widthMode;
	String numberOfLines;
	String width;
	
	Button[] rbWidthMode;
	Text txtWidth;
	Combo cmbNumberOfLines;
	
	public NodeSizeRadioGroupFieldEditor(Composite parent) {
		
		init(NODE_SIZE, "Predicate Node Size");
		createControl(parent);
    }
	
	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getRadioBoxControl(parent);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = 2;
		gd.verticalIndent = 5;
        control.setLayoutData(gd);
    }

	protected Composite getRadioBoxControl(Composite parent) {
		
		Group group = new Group(parent, SWT.NONE);
		
		group.setText(getLabelText());
		
		GridLayout layout = new GridLayout();
        layout.horizontalSpacing = HORIZONTAL_GAP;
        layout.numColumns = 3;
        group.setLayout(layout);
        
        createHeightCombo(group);
        
        GridData gd = new GridData();
        gd.horizontalSpan = 3;
        
        Label lblWidth = new Label(group, SWT.NONE);
        lblWidth.setText("Width");
        lblWidth.setLayoutData(gd);
		
        rbWidthMode = new Button[4];
        
		rbWidthMode[0] = createRadioButton(group, "&Fixed", NODE_SIZE_FIXED);
		
        txtWidth = createTextBox(group);
        
		rbWidthMode[1] = createRadioButton(group, "M&edian", NODE_SIZE_MEDIAN);
		rbWidthMode[2] = createRadioButton(group, "Ma&ximum", NODE_SIZE_MAXIMUM);
		rbWidthMode[3] = createRadioButton(group, "&Individual", NODE_SIZE_INDIVIDUAL);
        
        rbWidthMode[1].setLayoutData(gd);
        rbWidthMode[2].setLayoutData(gd);
        rbWidthMode[3].setLayoutData(gd);
		
		return group;
	}

	protected void createHeightCombo(Group group) {
		
		new Label(group, SWT.NONE).setText("Height");
		
		cmbNumberOfLines = new Combo(group, SWT.READ_ONLY);
        cmbNumberOfLines.add("1");
        cmbNumberOfLines.add("2");
        cmbNumberOfLines.add("3");
        cmbNumberOfLines.add("4");
        
        cmbNumberOfLines.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				String oldValue = numberOfLines;
				numberOfLines = cmbNumberOfLines.getText();
                setPresentsDefaultValue(false);
                fireValueChanged(NODE_SIZE_NUMBER_OF_LINES, oldValue, numberOfLines);
			}
        });
        
        new Label(group, SWT.NONE).setText("lines");
	}

	public Button createRadioButton(Composite parent, final String text, final String data) {
		Button radio = new Button(parent, SWT.RADIO | SWT.LEFT);
        radio.setText(text);
        radio.setData(data);
        radio.setFont(parent.getFont());
        radio.addSelectionListener(new SelectionAdapter() {
            @Override
			public void widgetSelected(SelectionEvent event) {
                String oldValue = widthMode;
                widthMode = (String) event.widget.getData();
                setPresentsDefaultValue(false);
                fireValueChanged(NODE_SIZE, oldValue, widthMode);
            }
        });
        
		return radio;
	}
	
	private Text createTextBox(final Composite parent) {
		
		GridData gd = new GridData();
		gd.widthHint = 34;
		
		final Text txt = new Text(parent, SWT.BORDER);
		txt.setLayoutData(gd);
		
		Label l = new Label(parent, SWT.NONE);
		l.setText("pixels");
		
		Point s = txt.getSize();
		s.y = 50;
		txt.setSize(s);
		
		txt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				String oldValue = width;
				width = (String)e.data;
				
				if (checkValues()) {
					setPresentsDefaultValue(false);
	                fireValueChanged(NODE_SIZE_FIXED_WIDTH, oldValue, width);
				}
			}
		});
		
		rbWidthMode[0].addSelectionListener(new SelectionAdapter() {
            @Override
			public void widgetSelected(SelectionEvent event) {
                txt.setEnabled(rbWidthMode[0].getSelection());
            }
        });
		
		return txt;
	}
	
	@Override
	public boolean isValid() {
		return checkValues();
	}
	
	private boolean checkValues() {
		if (width == null) {
			return true;
		}
		try {
			Integer.parseInt(width);
		}
		catch (NumberFormatException ex) {
			showErrorMessage("Wrong number format");
			return false;
		}
		
		clearErrorMessage();
		return true;
	}
	
	@Override
	protected void doLoad() {
		IPreferenceStore prefs = getPreferenceStore();
		
		updateRadioGroupValue(prefs.getString(NODE_SIZE));
		updateSizeTextBoxes(
				prefs.getString(NODE_SIZE_FIXED_WIDTH), 
				prefs.getString(NODE_SIZE_NUMBER_OF_LINES));
	}

	@Override
	protected void doLoadDefault() {
		IPreferenceStore prefs = getPreferenceStore();
		
		widthMode = null;
		numberOfLines = null;
		width = null;
		
		updateRadioGroupValue(getPreferenceStore().getString(NODE_SIZE));
		updateSizeTextBoxes(
				prefs.getDefaultString(NODE_SIZE_FIXED_WIDTH), 
				prefs.getDefaultString(NODE_SIZE_NUMBER_OF_LINES));
	}
	
	@Override
	protected void doStore() {
		if (!isValid()) 
			return;
		
		IPreferenceStore prefs = getPreferenceStore();
		store(prefs, NODE_SIZE, widthMode);
		store(prefs, NODE_SIZE_FIXED_WIDTH, width);
		store(prefs, NODE_SIZE_NUMBER_OF_LINES, numberOfLines);
	}
	
	private static void store(IPreferenceStore prefs, String name, String value) {
		if (value == null) {
            prefs.setToDefault(name);
        }
		else {
			prefs.setValue(name, value);
		}
	}
	
	private void updateRadioGroupValue(String selectedValue) {
        widthMode = selectedValue;
        if (rbWidthMode == null) {
			return;
		}

        if (widthMode != null) {
            for (int i = 0; i < rbWidthMode.length; i++) {
                Button radio = rbWidthMode[i];
                radio.setSelection(widthMode.equals(radio.getData()));
            }
        }
    }

	private void updateSizeTextBoxes(final String width, final String numberOfLines) {
		if (txtWidth == null || cmbNumberOfLines == null) {
			return;
		}
		
		txtWidth.setText(width);
		txtWidth.setEnabled(rbWidthMode[0].getSelection());
		
		cmbNumberOfLines.setText(numberOfLines);
	}

	@Override
	protected void adjustForNumColumns(int numColumns) { }

	@Override
	public int getNumberOfControls() {
		return 1;
	}
}


