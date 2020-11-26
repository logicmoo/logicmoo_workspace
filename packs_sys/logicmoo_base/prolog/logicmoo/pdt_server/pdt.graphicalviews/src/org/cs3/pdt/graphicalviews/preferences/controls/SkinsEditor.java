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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.BASE_TEMPLATE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.BASE_TEMPLATE_DEFAULT;

import java.io.FileNotFoundException;
import java.util.Hashtable;
import java.util.Set;

import org.cs3.pdt.graphicalviews.preferences.PreferenceInitializer;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.progress.UIJob;

public class SkinsEditor extends FieldEditor {

	private Combo templatesCombo;
	private Text newTemplateName;
	
	public SkinsEditor(Composite parent) {
		createControl(parent);
		
		new RowLayout(SWT.HORIZONTAL);
    }
	
	protected Composite wrap(Composite parent) {
		Composite row = new Composite(parent, SWT.NONE);
		RowLayout layout = new RowLayout(SWT.HORIZONTAL);
		layout.spacing = 10;
		layout.marginTop = 5;
		layout.marginLeft = 9;
		row.setLayout(layout);
		return row;
	}
	
	@Override
	protected void adjustForNumColumns(int numColumns) { }

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		
		new Label(wrap(parent), SWT.NONE).setText("A \"skin\" is a set of Focus View preferences.");
		
		GridData fullRowgd = new GridData(GridData.FILL_HORIZONTAL);
		fullRowgd.horizontalSpan = numColumns;
		
		Composite row = wrap(parent);
        
		new Label(row, SWT.NONE).setText("Current skin:  ");
		templatesCombo = new Combo(row, SWT.READ_ONLY);
        createRemoveAllTemplatesButton(row);
        
        //createImportButton(parent).setLayoutData(rdBtns);
        
//        new Label(container, SWT.NONE).setText("Save current preferences as template:");
//        newTemplateName = new Text(container, SWT.BORDER);
//        newTemplateName.setLayoutData(gd);
//        createSavePreferencesSetButton(container).setLayoutData(gdBtns);
        
        Group importExportButtonsContainer = new Group(parent, SWT.NONE);
        importExportButtonsContainer.setText("Save skins");
        importExportButtonsContainer.setLayoutData(fullRowgd);
        
        GridLayout bottomLayout = new GridLayout();
        bottomLayout.horizontalSpacing = 10;
        bottomLayout.marginWidth = 10;
        bottomLayout.marginHeight = 10;
        bottomLayout.numColumns = 2;

        importExportButtonsContainer.setLayout(bottomLayout);
        
        createSaveCurrentTemplateToFileButton(importExportButtonsContainer);
        createLoadTemplateFromFileButton(importExportButtonsContainer);
	}

	private Button createRemoveAllTemplatesButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText(" Delete current skin ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK | SWT.CANCEL);
				mb.setText("WARNING");
				mb.setMessage("All base templates except default will be removed!");
				if (mb.open() == SWT.OK) {
					PreferenceInitializer.removeAllTemplates(getPreferenceStore());
					updateUI();
				}
			}
		});

		return button;
	}

	private void createLoadTemplateFromFileButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText(" Load template from file ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fd = new FileDialog(getPage().getShell(), SWT.OPEN);
		        fd.setText("Open");
		        String path = fd.open();
		        if (path != null) {
		        	try {
						PreferenceInitializer.loadPreferencesFromFile(getPreferenceStore(), path);
						clearErrorMessage();
						showInfo("Template loaded and applied");
					} catch (Exception ex) {
						showMessage(ex.getMessage());
					}
		        }
			}
		});
	}

	private void createSaveCurrentTemplateToFileButton(Composite container) {
		Button button = new Button(container ,SWT.PUSH);
		
		button.setText(" Save template to file ");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fd = new FileDialog(getPage().getShell(), SWT.SAVE);
		        fd.setText("Save");
		        String path = fd.open();
		        if (path != null) {
		        	try {
						PreferenceInitializer.saveCurrentPreferencesToFile(getPreferenceStore(), path);
						clearErrorMessage();
						showInfo("Template saved");
					} catch (FileNotFoundException ex) {
						showErrorMessage(ex.getMessage());
					}
	        	}
			}
		});
	}

	protected Button createImportButton(Composite container) {
		Button button = new Button(container, SWT.PUSH);
		
		button.setText("Apply");
		
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK | SWT.CANCEL);
				mb.setText("WARNING");
				mb.setMessage("Current focusview preferences will be overriden!");
				if (mb.open() == SWT.OK) {
					PreferenceInitializer.applyTemplate(getPreferenceStore(), templatesCombo.getItem(templatesCombo.getSelectionIndex()));
					updateUI();
				}
			}
		});
		
		return button;
	}
	
	@SuppressWarnings("unused")
	private Button createSavePreferencesSetButton(Composite container) {
		Button button = new Button(container ,SWT.PUSH);
		
		button.setText("Save");
		
		button.addSelectionListener(new SelectionAdapter() {
			
			@Override
			@SuppressWarnings("deprecation")
			public void widgetSelected(SelectionEvent e) {
				try {
					getPreferencePage().performOk();
					PreferenceInitializer.saveCurrentTemplate(getPreferenceStore(), newTemplateName.getText());
					clearErrorMessage();
					updateUI();
					clearErrorMessage();
				}
				catch (IllegalArgumentException ex) {
					showErrorMessage(ex.getMessage());
				}
			}
		});
		return button;
	}
	
	protected void updateUI() {
		new UIJob("Updating...") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				newTemplateName.setText("");
				loadItems();
				return Status.OK_STATUS;
			}
		
		}.schedule();
	}

	@Override
	protected void doLoad() {
		loadItems();	
	}
	
	@Override
	protected void doLoadDefault() {
		loadItems();
	}

	private void loadItems() {
		if (templatesCombo == null)
			return;
		
		templatesCombo.removeAll();
		
		templatesCombo.add(BASE_TEMPLATE_DEFAULT);
		templatesCombo.select(0);
		
		String val = getPreferenceStore().getString(BASE_TEMPLATE);

		Hashtable<String,String[][]> storedPrefs = PreferenceInitializer.getTemplates(getPreferenceStore());
		if (storedPrefs != null) {
			Set<String> keys = storedPrefs.keySet();
			
			int i = 1;
			for (String k : keys) {
				templatesCombo.add(k);
				
				if (k.equals(val)) {
					templatesCombo.select(i);
				}
				i++;
			}
		}
	}

	@Override
	protected void doStore() { }

	@Override
	public int getNumberOfControls() {
		return 1;
	}
	
	private void showInfo(String string) {
		MessageBox mb = new MessageBox(getPage().getShell(), SWT.APPLICATION_MODAL | SWT.OK);
		mb.setText("Info");
		mb.setMessage(string);
		mb.open();
	}
}


