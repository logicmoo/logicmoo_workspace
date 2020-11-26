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

package org.cs3.pdt.connector.internal.preferences;

import java.util.List;

import org.cs3.pdt.connector.PDTConnector;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class PreferencePage extends org.eclipse.jface.preference.PreferencePage implements IWorkbenchPreferencePage {

	private Composite configurationSelector;

	private Table configurationList;

	private Button newConfiguration;
	private Button editConfiguration;
	private Button deleteConfiguration;


	public PreferencePage() {
		setPreferenceStore(PDTConnectorPlugin.getDefault().getPreferenceStore());
		setDescription("Select a default Prolog process configurations. There are predefined configurations which can be edited but not deleted. Self defined configurations can be deleted.");
	}

	@Override
	protected Control createContents(Composite parent) {
		configurationSelector = createConfigurationSelector(parent);
		fillConfigurationList();
		String configurationId = getPreferenceStore().getString(PDTConnector.PREF_CONFIGURATION);
		selectConfiguration(configurationId);
		for (TableItem item : configurationList.getItems()) {
			if (configurationId.equals(item.getText())) {
				configurationList.setSelection(item);
				break;
			}
		}
		return configurationSelector;
	}

	@Override
	public boolean performOk() {
		boolean result = super.performOk();
		String selectedConfiguration = getSelectedConfiguration();
		if (selectedConfiguration == null) {
			Debug.warning("Selected configuration is null!");
		} else {
			getPreferenceStore().setValue(PDTConnector.PREF_CONFIGURATION, selectedConfiguration);
		}
		return result;
	}
	
	@Override
	protected void performDefaults() {
		super.performDefaults();
		selectConfiguration(getPreferenceStore().getDefaultString(PDTConnector.PREF_CONFIGURATION));
	}

	private Composite createConfigurationSelector(Composite parent) {
		Composite container = new Composite(parent, SWT.NONE);
		container.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		container.setLayout(new GridLayout(3, false));
		
		Label label = new Label(container, SWT.LEFT);
		label.setText("Configuration");
		label.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		
		configurationList = new Table(container, SWT.CHECK | SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION);
		configurationList.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		configurationList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
	            if (event.detail == SWT.CHECK) {
	            	TableItem item = (TableItem) event.item;
	                boolean checked = item.getChecked();
	                if (checked) {
	                	for (TableItem otherItem : configurationList.getItems()) {
	                		if (!(otherItem.equals(item))) {
	                			otherItem.setChecked(false);
	                		}
	                	}
	                } else {
	                	item.setChecked(true);
	                }
	            } else {
	            	TableItem item = (TableItem) event.item;
	            	String configuration = item.getText();
	            	deleteConfiguration.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurations().contains(configuration));
	            }
			}
		});
		
		Composite buttonContainer = new Composite(container, SWT.NONE);
		buttonContainer.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));
		buttonContainer.setLayout(new GridLayout(1, false));
		
		newConfiguration = createButton(buttonContainer, "New...");
		newConfiguration.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				NewConfigurationDialog dialog = new NewConfigurationDialog(newConfiguration.getShell(), PreferenceConfiguration.getInstance().getConfigurations(), PreferenceConfiguration.getInstance().getDefaultConfigurations());
				int result = dialog.open();
				if (result == Dialog.OK && dialog.getConfiguration() != null && !dialog.getConfiguration().isEmpty()) {
					String newConfiguration = dialog.getConfiguration();
					PreferenceConfiguration.getInstance().addConfiguration(newConfiguration, dialog.getDefaultConfiguration());
					fillConfigurationList();
					selectConfiguration(newConfiguration);
				}
			}
		});
		editConfiguration = createButton(buttonContainer, "Edit...");
		editConfiguration.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				if (configurationList.getSelectionIndex() == -1) {
					return;
				}
				TableItem item = configurationList.getItem(configurationList.getSelectionIndex());
				String configuration = item.getText();
				new EditConfigurationDialog(getShell(), PreferenceConfiguration.getInstance().getPreferenceStore(configuration), configuration).open();
			}
		});
		deleteConfiguration = createButton(buttonContainer, "Delete");
		deleteConfiguration.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				if (configurationList.getSelectionIndex() == -1) {
					return;
				}
				TableItem item = configurationList.getItem(configurationList.getSelectionIndex());
				String configuration = item.getText();
				boolean answer = MessageDialog.openQuestion(deleteConfiguration.getShell(), "Delete configuration", "Do you want to delete the configuration \"" + configuration + "\"?");
				if (answer) {
					String defaultId = PreferenceConfiguration.getInstance().getDefaultConfiguration(configuration);
					PreferenceConfiguration.getInstance().deleteConfiguration(configuration);
					fillConfigurationList();
					selectConfiguration(defaultId);
					PDTConnectorPlugin.getDefault().getPreferenceStore().setValue(PDTConnector.PREF_CONFIGURATION, defaultId);
				}
			}
		});
		return container;
	}
	
	private Button createButton(Composite parent, String label) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText(label);
		
		GridData data = new GridData(SWT.FILL, SWT.CENTER, false, false);
		int widthHint = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		Point minSize = button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
		data.widthHint = Math.max(widthHint, minSize.x);
		button.setLayoutData(data);
		return button;
	}
	
	private void fillConfigurationList() {
		configurationList.removeAll();
		for (String configId : PreferenceConfiguration.getInstance().getConfigurations()) {
			TableItem item = new TableItem(configurationList, SWT.NONE);
			item.setText(configId);
		}
	}
	
	private void selectConfiguration(String configurationId) {
		for (TableItem item : configurationList.getItems()) {
			item.setChecked(configurationId.equals(item.getText()));
		}
		deleteConfiguration.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurations().contains(configurationId));
	}
	
	private String getSelectedConfiguration() {
		for (TableItem item : configurationList.getItems()) {
			if (item.getChecked()) {
				return item.getText();
			}
		}
		return null;
	}
	
	private static class NewConfigurationDialog extends Dialog {

		private Text text;
		private List<String> configurations;
		private List<String> defaultConfigurations;
		private Combo combo;
		
		private String defaultConfiguration;
		private String configuration;

		private static final char[] ILLEGAL_CHARACTERS = { '/', '\n', '\r', '\t', '\0', '\f', '`', '?', '*', '\\', '<', '>', '|', '\"', ':', '\'', ';' };
		
		protected NewConfigurationDialog(Shell parentShell, List<String> configurations, List<String> defaultConfigurations) {
			super(parentShell);
			this.configurations = configurations;
			this.defaultConfigurations = defaultConfigurations;
		}
		
		@Override
		protected Control createDialogArea(Composite parent) {
			Composite composite = (Composite) super.createDialogArea(parent);
			composite.setLayout(new GridLayout(2, false));
			
			Label label1 = new Label(composite, SWT.NONE);
			label1.setText("Name");
			label1.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			
			text = new Text(composite, SWT.SINGLE | SWT.BORDER);
			text.setTextLimit(50);
			GridData textLayoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
			textLayoutData.widthHint = convertWidthInCharsToPixels(50);
			text.setLayoutData(textLayoutData);
			text.addVerifyListener(new VerifyListener() {
				@Override
				public void verifyText(VerifyEvent e) {
					for (char c : e.text.toCharArray()) {
						if (isIllegalCharacter(c)) {
							e.doit = false;
							return;
						}
					}
				}
				
				private boolean isIllegalCharacter(char c) {
					for (char illegal : ILLEGAL_CHARACTERS) {
						if (illegal == c) {
							return true;
						}
					}
					return false;
				}
			});
			
			Label label2 = new Label(composite, SWT.NONE);
			label2.setText("Inherit defaults from");
			label2.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			
			combo = new Combo(composite, SWT.READ_ONLY);
			for (String defaultConfiguration : defaultConfigurations) {
				combo.add(defaultConfiguration);
			}
			combo.setText(defaultConfigurations.get(0));
			combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			
			return composite;
		}
		
		@Override
		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText("New Configuration");
		}
		
		protected void okPressed() {
			if (text.getText().isEmpty()) {
				MessageDialog.openWarning(getShell(), "New Configuration", "Configuration must not be empty.");
				return;
			} else if (configurations.contains(text.getText())) {
				MessageDialog.openWarning(getShell(), "New Configuration", "Configuration malready exists.");
				return;
			}
			configuration = text.getText();
			defaultConfiguration = combo.getText();
			super.okPressed();
		}
		
		public String getConfiguration() {
			return configuration;
		}
		
		public String getDefaultConfiguration() {
			return defaultConfiguration;
		}

	}

	@Override
	public void init(IWorkbench workbench) {
	}
	
}


