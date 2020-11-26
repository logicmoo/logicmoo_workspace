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

package org.cs3.pdt.console.internal.preferences;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.connector.util.preferences.MyColorFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyFileFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyFontFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyIntegerFieldEditor;
import org.cs3.pdt.connector.util.preferences.StructuredFieldEditorPreferencePage;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
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

public class PreferencePageMain extends StructuredFieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private ColorFieldEditor cfe_error;
	private ColorFieldEditor cfe_warn;
	private ColorFieldEditor cfe_info;
	private ColorFieldEditor cfe_dbg;
	
	public PreferencePageMain() {
		super(GRID);
		setPreferenceStore(PrologConsolePlugin.getDefault().getPreferenceStore());
	}
		
	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		Group fontGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		fontGroup.setText("Console font");
		addField(new MyFontFieldEditor(PDTConsole.PREF_CONSOLE_FONT, "Console font:", fontGroup));
		addField(new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_NORMAL, "Console font color:", fontGroup));
		
		Group colourGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		colourGroup.setText("Colour output line starting with ...");
		
		cfe_error = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_ERROR, "!!! or ERROR", colourGroup);
		cfe_warn = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_WARNING, "*** or WARNING", colourGroup);
		cfe_info = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_INFO, "~~~ or INFO", colourGroup);
		cfe_dbg = new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_DEBUG, "??? or DEBUG", colourGroup);
		
		addField(cfe_error);
		addField(cfe_warn);
		addField(cfe_info);
		addField(cfe_dbg);
		
		Group backgroundGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		backgroundGroup.setText("Console background colors");
		addField(new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_BACKGROUND_NORMAL, "No running query", backgroundGroup));
		addField(new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_BACKGROUND_SINGLE_CHAR_MODE, "Running query, waiting for user input", backgroundGroup));
		addField(new MyColorFieldEditor(PDTConsole.PREF_CONSOLE_COLOR_BACKGROUND_DISABLED, "No Prolog Process connected to the Console", backgroundGroup));
		
		addField(new MyIntegerFieldEditor(PDTConsole.PREF_CONSOLE_HISTORY_SIZE, "History size", getFieldEditorParent(), 4));
		
		adjustLayoutForElement(fontGroup);
		adjustLayoutForElement(colourGroup);
		adjustLayoutForElement(backgroundGroup);
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
	
	
	
	class FileFieldEditorWithEnsureFileExists extends MyFileFieldEditor {

		public FileFieldEditorWithEnsureFileExists(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
		}
		
		@Override
		protected boolean checkState() {
			return ensureFileExists();// && super.checkState();
		}
		
		private boolean ensureFileExists() {
			
			String msg = null;
			String value = getTextControl().getText();
			
			if (value == null) {
				msg = "History File must not be null";
			}
			if (value.length() == 0) {
				msg = "History File must not be empty";
			}
			File f = new File(value);
			if (!f.isAbsolute()) {
				msg = "History File must be an absolute path";
			}
			if (f.isDirectory()) {
				msg = "History File exists, but is a directory";
			}
			if (!f.exists()) {
				try {
					if (!f.createNewFile()) {

					}
				} catch (IOException e) {
					Debug.report(e);
					msg = "could not create History File";
				}

			}
			if (!f.canWrite()) {
				msg = "History File exists, but is not writable";
			}
			
			
			if (msg != null) { // error
		        showErrorMessage(msg);
		        return false;
			}

	        // OK!
	        clearErrorMessage();
	        return true;
		}

	}
	
	


}


