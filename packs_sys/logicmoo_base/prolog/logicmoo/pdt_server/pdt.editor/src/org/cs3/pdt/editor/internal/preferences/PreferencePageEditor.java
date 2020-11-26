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

package org.cs3.pdt.editor.internal.preferences;

import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.PDTPlugin;
import org.eclipse.jface.preference.BooleanFieldEditor;
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

public class PreferencePageEditor extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public PreferencePageEditor() {
		super(GRID);
		setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		addField(new BooleanFieldEditor(PDT.PREF_EXTERNAL_FILE_SAVE_WARNING, "Ask before saving external files", getFieldEditorParent()));

		addField(new BooleanFieldEditor(PDT.PREF_AUTO_COMPLETE_ARGLIST, "Create arglist in auto completion", getFieldEditorParent()));

		addField(new BooleanFieldEditor(PDT.PREF_CONSULT_ON_SAVE, "Consult file on save", getFieldEditorParent()));

		addField(new BooleanFieldEditor(PDT.PREF_OUTLINESHOW_MULTIFILE, "Show multifile contributions in the Outline", getFieldEditorParent()));

		addField(new RadioGroupFieldEditor(PDT.PREF_OUTLINE_SHOW_ALL_CLAUSES, "Number of clauses per predicate listed in the Outline", 2, new String[][]{{"1", "false"}, {"All", "true"}}, getFieldEditorParent(), true));
		
		addField(new IntegerFieldEditor(PDT.PREF_OUTLINE_FIRST_ARGUMENT_VARIABLE_FILE_SIZE, "Do not compute the variable name of the first argument of clauses in files larger than (in kB)", getFieldEditorParent()));

//		// A comma separated list of filter ids that should be activated at startup
//		StringFieldEditor sfe = new StringFieldEditor(PDT.PREF_OUTLINE_FILTERS, "Default active Filters for the Prolog Outline",
//				getFieldEditorParent());
//		addField(sfe);
//
//		BooleanFieldEditor bfe = new BooleanFieldEditor(PDT.PREF_OUTLINE_SORT,
//				"Whether the Prolog Outline is to be sorted lexicographical", getFieldEditorParent());
//		bfe.setEnabled(false, getFieldEditorParent());
//		addField(bfe);

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


