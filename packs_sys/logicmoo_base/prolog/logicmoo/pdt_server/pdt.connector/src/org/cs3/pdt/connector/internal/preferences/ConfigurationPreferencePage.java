/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.connector.internal.preferences;

import java.util.ArrayList;
import java.util.Set;

import org.cs3.pdt.connector.PDTConnector;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.util.EclipsePreferenceProvider;
import org.cs3.pdt.connector.util.preferences.MyBooleanFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyDirectoryFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyFileFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyIntegerFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyLabelFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyStringFieldEditor;
import org.cs3.pdt.connector.util.preferences.StructuredFieldEditorPreferencePage;
import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ProcessUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Group;

public class ConfigurationPreferencePage extends StructuredFieldEditorPreferencePage {

	private boolean preferencesChanged = false;

	private MyFileFieldEditor executable;
	private MyStringFieldEditor invocation;
	private MyStringFieldEditor commandLineArguments;
	private MyStringFieldEditor startupFiles;
	private MyLabelFieldEditor executeablePreviewLabel;
	private MyStringFieldEditor extraEnvironmentVariables;
	private MyDirectoryFieldEditor serverLogDir;
	private MyIntegerFieldEditor timeoutFieldEditor;
	private MyBooleanFieldEditor hidePrologWindow;

	private ArrayList<FieldEditor> editors = new ArrayList<FieldEditor>();

	private String configurationId;

	public ConfigurationPreferencePage(String configurationId) {
		super(GRID);
		this.configurationId = configurationId;
		setDescription(escape("Edit the " + configurationId + " Prolog process configuration."));
		setTitle(escape(configurationId));
	}
	
	private String escape(String s) {
		return s.replace("&", "&&");
	}

	@Override
	public void createFieldEditors() {
		Group executableGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		executableGroup.setText("Executable");

		invocation = new MyStringFieldEditor(Connector.PREF_INVOCATION, "OS invocation", executableGroup);
		addField(invocation);

		// eg. xpce or /usr/bin/xpce
		executable = new MyFileFieldEditor(Connector.PREF_EXECUTABLE, "Prolog executable", executableGroup);
		executable.getLabelControl(executableGroup).setToolTipText("Don't enter quotes, they will be added automatically.");
		addField(executable);

		commandLineArguments = new MyStringFieldEditor(Connector.PREF_COMMAND_LINE_ARGUMENTS, "Command line arguments", executableGroup);
		commandLineArguments.getLabelControl(executableGroup).setToolTipText("See SWI-Prolog manual for a list of possible command line arguments.");
		addField(commandLineArguments);

		startupFiles = new MyStringFieldEditor(Connector.PREF_ADDITIONAL_STARTUP, "Additional startup files", executableGroup) {
			@Override
			protected boolean doCheckState() {
				String value = getStringValue();
				String[] files = value.split(",");
				for (String file : files) {
					if (file.contains(" ")) {
						if (!(file.startsWith("\"") && file.endsWith("\""))
								&& !(file.startsWith("'") && file.endsWith("'"))) {
							return false;
						}
					}
				}
				return true;
			}
		};
		startupFiles.setErrorMessage("File paths containing white spaces must be enclosed in double quotes. To enter multiple files, separate them by a comma.");
		startupFiles.getLabelControl(executableGroup).setToolTipText("Can be multiple files, seperated by commas.\nAdd quotes if needed!\n\nExample: \"c:/my files/dummy.pl\" dummy2.pl");

		addField(startupFiles);

		executeablePreviewLabel = new MyLabelFieldEditor(executableGroup, "Executable preview");
		addField(executeablePreviewLabel);

		extraEnvironmentVariables = new MyStringFieldEditor(Connector.PREF_ENVIRONMENT, "Extra environment variables", getFieldEditorParent());
		addField(extraEnvironmentVariables);

		serverLogDir = new MyDirectoryFieldEditor(Connector.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		addField(serverLogDir);

		timeoutFieldEditor = new MyIntegerFieldEditor(Connector.PREF_TIMEOUT, "Connect Timeout", getFieldEditorParent());
		timeoutFieldEditor.getTextControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		timeoutFieldEditor.getLabelControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		addField(timeoutFieldEditor);

		// The host the process server is listening on
		StringFieldEditor host = new MyStringFieldEditor(Connector.PREF_HOST, "Server host", getFieldEditorParent());
		host.setEnabled(false, getFieldEditorParent());
		addField(host);

		// The port the process server is listening on
		IntegerFieldEditor port = new MyIntegerFieldEditor(Connector.PREF_PORT, "Server port", getFieldEditorParent());
		port.setEnabled(false, getFieldEditorParent());
		addField(port);

		hidePrologWindow = new MyBooleanFieldEditor(Connector.PREF_HIDE_PLWIN, "Hide prolog process window (Windows only)", getFieldEditorParent());
		addField(hidePrologWindow);

		adjustLayoutForElement(executableGroup);
	}

	@Override
	protected void initialize() {
		super.initialize();
		updateExecuteablePreviewLabelText();
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		super.propertyChange(event);

		String prefName = ((FieldEditor) event.getSource()).getPreferenceName();
		if (prefName.equals(Connector.PREF_INVOCATION)
				|| prefName.equals(Connector.PREF_EXECUTABLE)
				|| prefName.equals(Connector.PREF_ADDITIONAL_STARTUP)
				|| prefName.equals(Connector.PREF_COMMAND_LINE_ARGUMENTS)) {

			updateExecuteablePreviewLabelText();
		}
		preferencesChanged = true;
	}

	@Override
	public void addField(FieldEditor editor) {
		editors.add(editor);
		super.addField(editor);
	}

	@Override
	public boolean performOk() {
		boolean result = super.performOk();
		try {
			((PreferenceStore) getPreferenceStore()).save();
		} catch (Exception e) {
			Debug.report(e);
		}
		if (preferencesChanged) {
			updatePrologProcessExecutables();
		}
		return result;
	}

	private void updateExecuteablePreviewLabelText() {
		String newExecutable = ProcessUtils.createExecutable(invocation.getStringValue(), executable.getStringValue(), commandLineArguments.getStringValue(), startupFiles.getStringValue()) + " -g [$ConnectorInitFile]";
		executeablePreviewLabel.setText(newExecutable);
	}

	private void updatePrologProcessExecutables() {
		PrologProcessRegistry registry = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();
		Set<String> subscriptionIds = registry.getAllSubscriptionIDs();
		for (String id : subscriptionIds) {
			PrologProcess process = registry.getPrologProcess(registry.getSubscription(id).getProcessKey());
			if (process != null && configurationId.equals(process.getAttribute(PDTConnector.CONFIGURATION_ATTRIBUTE))) { // Sinan & Günter, 24.9.2010
				process.initOptions(new EclipsePreferenceProvider(PDTConnectorPlugin.getDefault(), configurationId));
			}
		}
	}

	@Override
	protected void performApply() {
		performOk();
		preferencesChanged = false;
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
	}

}
