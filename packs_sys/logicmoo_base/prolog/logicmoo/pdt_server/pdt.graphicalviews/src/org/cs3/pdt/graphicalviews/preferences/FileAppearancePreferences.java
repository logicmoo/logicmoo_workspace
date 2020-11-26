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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_MODULE_FILE_BACKGROUND_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_MODULE_HEADER_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_NONMODULE_HEADER_COLOR;

import java.awt.Color;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Group;
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
public class FileAppearancePreferences
	extends PreferencePageBase {
	
	private GridLayout groupLayout;
	private GridData cellData;
	
	public FileAppearancePreferences() {
		groupLayout = new GridLayout();
		groupLayout.marginWidth = 10;
        groupLayout.marginHeight = 10;
        groupLayout.numColumns = 2;
        
        cellData = new GridData();
        cellData.widthHint = 200;
        cellData.heightHint = 32;
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		
		addField(new ColorFieldEditor(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR, "&File Background Color    ", wrap(getFieldEditorParent())));
		
		Group fileHeaderColor = createGroup("File Header Background Color", groupLayout);
		
		addField(new ColorFieldEditor(APPEARANCE_MODULE_HEADER_COLOR, "&Module File  ", wrap(fileHeaderColor, cellData)));
		addField(new ColorFieldEditor(APPEARANCE_NONMODULE_HEADER_COLOR, "&Non Module File  ", wrap(fileHeaderColor, cellData)));
	}

	@Override
	public void init(IWorkbench workbench) {
	}
	
	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static Color getColor(String preferenceName) {
		String value = getCurrentPreferences().getString(preferenceName);
		RGB color = StringConverter.asRGB(value);
		return new Color(color.red, color.green, color.blue);
	}
	
	public static Color getFileHeaderColor() {
		return getColor(APPEARANCE_NONMODULE_HEADER_COLOR);
	}
	
	public static Color getModuleHeaderColor() {
		return getColor(APPEARANCE_MODULE_HEADER_COLOR);
	}
	
	public static Color getModuleFileBackgroundColor() {
		return getColor(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR);
	}
}


