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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING_BRACKET;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING_MIDDLE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING_POSTFIX;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING_PREFIX;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.SHOW_TOOLTIPS;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.cs3.pdt.graphicalviews.preferences.controls.NodeSizeRadioGroupFieldEditor;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class PredicateLayoutPreferences
	extends PreferencePageBase {
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		
		addField(new NodeSizeRadioGroupFieldEditor(getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(
				NAME_CROPPING, "Name cropping", 1,
				new String[][] { 
					{ "&Bracket:   abc ... xyz", NAME_CROPPING_BRACKET },
					{ "P&refix:      abcdefg ...", NAME_CROPPING_PREFIX }, 
					{ "&Middle:   ... klmno ...", NAME_CROPPING_MIDDLE },
					{ "P&ostfix:    ... tuvwxyz", NAME_CROPPING_POSTFIX }
				}, getFieldEditorParent(), true));
		
		addField(new BooleanFieldEditor(SHOW_TOOLTIPS, "&Show full name in tooltip", wrap(getFieldEditorParent())));
	}
	
	@Override
	public void init(IWorkbench workbench) {
	}

	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static String getNameCroppingConfiguration() {
		return getCurrentPreferences().getString(PreferenceConstants.NAME_CROPPING);
	}
	
	public static String getNodeSizePreference() {
		return getCurrentPreferences().getString(PreferenceConstants.NODE_SIZE);
	}
	
	public static int getNumberOfLines() {
		return getCurrentPreferences().getInt(PreferenceConstants.NODE_SIZE_NUMBER_OF_LINES);
	}
	
	public static String getLayoutPreference() {
		return getCurrentPreferences().getString(PreferenceConstants.LAYOUT);
	}
	
	public static void setLayoutPreference(String value) {
		getCurrentPreferences().setValue(PreferenceConstants.LAYOUT, value);
	}
	
	public static boolean isShowToolTip() {
		return getCurrentPreferences().getBoolean(SHOW_TOOLTIPS);
	}
}


