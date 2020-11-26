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

package org.cs3.pdt.common.internal.preferences;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.connector.util.preferences.MyRadioGroupFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class PreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PDTCommonPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
	
//		// When i open a file in the prolog editor that does not belong to
//		// a prolog project, ask if i want to add the prolog nature.
//		addField(new RadioGroupFieldEditor(PDT.PREF_ADD_NATURE_ON_OPEN, "Automatically add Prolog Nature when opening pl files", 4,
//				new String[][] { { "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
//						{ "ask", MessageDialogWithToggle.PROMPT } }, getFieldEditorParent(), true));
//
//		// When i consult a prolog file, but the active console view is not
//		// connected to the default runtime
//		// of the respective prolog project, should i switch to the default
//		// runtime first?
//		addField(new RadioGroupFieldEditor(PDT.PREF_SWITCH_TO_DEFAULT_PROCESS, "Switch to default runtime before consulting", 4,
//				new String[][] { { "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
//						{ "ask", MessageDialogWithToggle.PROMPT } }, getFieldEditorParent(), true));
//		
		
		// Determines the verbosity of the debug log file.			
		RadioGroupFieldEditor rgfe_level = new RadioGroupFieldEditor(PDTCommon.PREF_DEBUG_LEVEL, "Debug Level", 5, new String[][] {
				{ "none", "NONE" }, { "error", "ERROR" }, { "warning", "WARNING" }, { "info", "INFO" }, { "debug", "DEBUG" } },
				getFieldEditorParent(), true);
		addField(rgfe_level);

		RadioGroupFieldEditor rgfe_output = new RadioGroupFieldEditor(PDTCommon.PREF_DEBUG_OUTPUT_TO, "Debug Output to", 3, new String[][] {
				{ "logfile", "LOGFILE" }, { "console", "CONSOLE" } }, getFieldEditorParent(), true);
		addField(rgfe_output);

		// A file to which debug output of the PDT will be writen
		addField(new DirectoryFieldEditor(PDTCommon.PREF_CLIENT_LOG_FILE_DIR, "Log file location", getFieldEditorParent()));
		
		IntegerFieldEditor fileSizeLimit = new IntegerFieldEditor(PDTCommon.PREF_FILE_SIZE_LIMIT, "Warn before opening files larger than (in kB)", getFieldEditorParent(), 6);
		String toolTip = "Show a warning before opening Prolog files which are larger than the specified limit.\nThe value 0 disables the warning.";
		fileSizeLimit.getLabelControl(getFieldEditorParent()).setToolTipText(toolTip);
		fileSizeLimit.getTextControl(getFieldEditorParent()).setToolTipText(toolTip);
		addField(fileSizeLimit);

		RadioGroupFieldEditor rgfeReconsult = new MyRadioGroupFieldEditor(PDTCommon.PREF_RECONSULT_ON_RESTART, "Handling consulted files on restart", 3, new String[][] {
				{ "no reconsulting", PDTCommon.RECONSULT_NONE }, { "reconsult entry points", PDTCommon.RECONSULT_ENTRY }, { "reconsult all files", PDTCommon.RECONSULT_ALL }},
				getFieldEditorParent());
		addField(rgfeReconsult);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

}


