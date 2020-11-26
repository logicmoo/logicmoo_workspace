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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_DASHED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_DASHED_DOTTED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_DOTTED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_SOLID;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_EXPORTED_PREDICATE_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_PREDICATE_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR;

import java.awt.Color;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;

import y.view.LineType;

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

public class PredicateAppearancePreferences
	extends PreferencePageBase {

	private static final LineType[] s_lineTypes = {
		LineType.LINE_2,
		LineType.DASHED_2,
		LineType.DOTTED_2,
		LineType.DASHED_DOTTED_3
	};
	
	private GridLayout groupLayout;
	private GridData cellData;
	
	public PredicateAppearancePreferences() {
		groupLayout = new GridLayout();
		groupLayout.marginWidth = 10;
        groupLayout.marginHeight = 10;
        groupLayout.numColumns = 2;
        
        cellData = new GridData();
        cellData.widthHint = 267;
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
		
		String[][] lineTypes = new String[][] { 
				{ "Solid      ", Integer.toString(APPEARANCE_BORDER_STYLE_SOLID) },
				{ "Dashed     ", Integer.toString(APPEARANCE_BORDER_STYLE_DASHED) },
				{ "Dotted     ", Integer.toString(APPEARANCE_BORDER_STYLE_DOTTED) },
				{ "Dashed Dotted", Integer.toString(APPEARANCE_BORDER_STYLE_DASHED_DOTTED) }
			};
		
		Group predicateBgColor = createGroup("Predicate Node Background Color", groupLayout);
		
		addField(new ColorFieldEditor(APPEARANCE_EXPORTED_PREDICATE_COLOR, "&Exported          ", wrap(predicateBgColor, cellData)));
		addField(new ColorFieldEditor(APPEARANCE_PREDICATE_COLOR, "&Not Exported        ", wrap(predicateBgColor, cellData)));
		
		Group predicateBorderColor = createGroup("Predicate Node Border Color", groupLayout);
		
		addField(new ColorFieldEditor(APPEARANCE_BORDER_COLOR, "&Used Predicate ", wrap(predicateBorderColor, cellData)));
		addField(new ColorFieldEditor(APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR, "Unuse&d Predicate  ", wrap(predicateBorderColor, cellData)));
		
		Group predicateBorderStyle = createGroup("Predicate Node Border Style", groupLayout);
		
		addField(new ComboFieldEditor(APPEARANCE_BORDER_STYLE, "&Static Predicate", lineTypes, wrap(predicateBorderStyle, cellData)));
		addField(new ComboFieldEditor(APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE, "D&ynamic Predicate", lineTypes, wrap(predicateBorderStyle, cellData)));
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
	
	public static Color getPredicateColor() {
		return getColor(APPEARANCE_PREDICATE_COLOR);
	}
	
	public static Color getExportedPredicateColor() {
		return getColor(APPEARANCE_EXPORTED_PREDICATE_COLOR);
	}
	
	public static Color getBorderColor() {
		return getColor(APPEARANCE_BORDER_COLOR);
	}
	
	public static Color getUnusedPredicateBorderColor() {
		return getColor(APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR);
	}
	
	public static LineType getBorderStyle() {
		int i = getCurrentPreferences().getInt(APPEARANCE_BORDER_STYLE);
		return s_lineTypes[i];
	}
	
	public static LineType getDynamicPredicateBorderStyle() {
		int i = getCurrentPreferences().getInt(APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE);
		return s_lineTypes[i];
	}
}


