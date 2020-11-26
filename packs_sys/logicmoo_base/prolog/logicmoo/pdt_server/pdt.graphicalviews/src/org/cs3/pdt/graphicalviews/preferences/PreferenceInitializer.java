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
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_DASHED_DOTTED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_BORDER_STYLE_SOLID;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_EXPORTED_PREDICATE_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_LINE_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_MODULE_FILE_BACKGROUND_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_MODULE_HEADER_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_NONMODULE_HEADER_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_PREDICATE_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.BASE_TEMPLATE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.BASE_TEMPLATES_STORAGE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.BASE_TEMPLATE_DEFAULT;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.LAYOUT;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.LAYOUT_COMPACTNESS;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.LAYOUT_HIERARCHY;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NAME_CROPPING_BRACKET;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_NUMBER_OF_LINES;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.REFRESH_MODE;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.REFRESH_MODE_MANUAL;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.SHOW_TOOLTIPS;

import java.awt.Color;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.Hashtable;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.cs3.pdt.graphicalviews.utils.ColorUtils;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	public static final String[][] defaultPreferences = new String[][] {
			{ BASE_TEMPLATE, BASE_TEMPLATE_DEFAULT },
			{ REFRESH_MODE, REFRESH_MODE_MANUAL },
			{ SHOW_TOOLTIPS, Boolean.toString(true) },
			{ NAME_CROPPING, NAME_CROPPING_BRACKET },
			{ NODE_SIZE, NODE_SIZE_FIXED },
			{ NODE_SIZE_NUMBER_OF_LINES, "2" },
			{ NODE_SIZE_FIXED_WIDTH, "100" },
			{ LAYOUT, LAYOUT_HIERARCHY },
			{ LAYOUT_COMPACTNESS, "500" },
			{ APPEARANCE_NONMODULE_HEADER_COLOR, ColorUtils.getColorString(Color.WHITE) },
			{ APPEARANCE_MODULE_HEADER_COLOR, ColorUtils.getColorString(new Color(203, 215, 226)) },
			{ APPEARANCE_MODULE_FILE_BACKGROUND_COLOR, ColorUtils.getColorString(new Color(240, 240, 240)) },
			{ APPEARANCE_PREDICATE_COLOR, ColorUtils.getColorString(Color.YELLOW) },
			{ APPEARANCE_EXPORTED_PREDICATE_COLOR, ColorUtils.getColorString(Color.GREEN) },
			{ APPEARANCE_BORDER_COLOR, ColorUtils.getColorString(Color.BLACK) },
			{ APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR, ColorUtils.getColorString(Color.RED) },
			{ APPEARANCE_BORDER_STYLE, Integer.toString(APPEARANCE_BORDER_STYLE_SOLID) },
			{ APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE, Integer.toString(APPEARANCE_BORDER_STYLE_DASHED_DOTTED) },
			{ APPEARANCE_LINE_COLOR, ColorUtils.getColorString(Color.DARK_GRAY) }
	};
	
	@SuppressWarnings("unchecked")
	public static Hashtable<String, String[][]> getTemplates(IPreferenceStore store) {
		
		Hashtable<String, String[][]> templates = null;
		try {
			String storage = store.getString(BASE_TEMPLATES_STORAGE);
			
			if (storage.length() == 0) {
				return null;
			}
			
			ObjectInputStream stream = new ObjectInputStream(new ByteArrayInputStream(storage.getBytes()));
			
			templates = (Hashtable<String, String[][]>)stream.readObject();
		}
		catch (Exception e) {
			return null;
		}
		return templates;
	}

	@Override
	public void initializeDefaultPreferences() {
		
		IPreferenceStore store = PluginActivator.getDefault().getPreferenceStore();
		
		String baseTemplate = store.getString(BASE_TEMPLATE);
		if (baseTemplate.length() == 0) {
			baseTemplate = BASE_TEMPLATE_DEFAULT;
		}
			
		Hashtable<String,String[][]> templates = getTemplates(store);
		String[][] preferences;
		if (templates == null
				|| (preferences = templates.get(baseTemplate)) == null) {
			preferences = defaultPreferences;
		}
		for (String[] p : preferences) {
			store.setDefault(p[0], p[1]);
		}
	}

	public static void saveCurrentTemplate(IPreferenceStore store, String name) throws IllegalArgumentException {
		
		store.setValue(BASE_TEMPLATE, name);
		
		Hashtable<String,String[][]> templates = getTemplates(store);
		
		if (templates == null)
			templates = new Hashtable<String,String[][]>();
		
		if (name.length() == 0 || templates.containsKey(name)) {
			throw new IllegalArgumentException("Template name must be non-empty and unique");
		}
		
		String[][] newPreferences = getCurrentPreferences(store);
		
		templates.put(name, newPreferences);
		
		ByteArrayOutputStream mem = new ByteArrayOutputStream(4096);
		writeObjectToStream(templates, mem);
		store.setValue(BASE_TEMPLATES_STORAGE, mem.toString());
	}

	public static void applyTemplate(IPreferenceStore store, String name) {
		
		Hashtable<String,String[][]> templates = getTemplates(store);
		
		String[][] preferences;
		if (templates == null 
				|| (preferences = templates.get(name)) == null) {
			preferences = defaultPreferences;
		}
		
		applyPreferences(store, preferences);
	}

	protected static void applyPreferences(IPreferenceStore store, String[][] preferences) {
		for (String[] p : preferences) {
			store.setDefault(p[0], p[1]);
			store.setToDefault(p[0]);
		}
		PluginActivator.getDefault().preferencesUpdated();
	}

	public static void removeAllTemplates(IPreferenceStore store) {
		store.setValue(BASE_TEMPLATES_STORAGE, "");
		store.setValue(BASE_TEMPLATE, BASE_TEMPLATE_DEFAULT);
	}

	public static void saveCurrentPreferencesToFile(IPreferenceStore store, String path) throws FileNotFoundException {
		
		String[][] newPreferences = getCurrentPreferences(store);
		
		writeObjectToStream(newPreferences, new FileOutputStream(path));
	}

	protected static String[][] getCurrentPreferences(IPreferenceStore store) {
		String[][] newPreferences = new String[defaultPreferences.length][2];
		for (int i = 0; i < defaultPreferences.length; i++) {
			newPreferences[i][0] = defaultPreferences[i][0];
			newPreferences[i][1] = store.getString(defaultPreferences[i][0]);
		}
		return newPreferences;
	}

	protected static void writeObjectToStream(Object dataObject, OutputStream stream) {
		ObjectOutputStream objStream;
		try {
			objStream = new ObjectOutputStream(stream);
			objStream.writeObject(dataObject);
			stream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void loadPreferencesFromFile(IPreferenceStore store, String path) throws IOException, ClassNotFoundException {
		
		FileInputStream stream = new FileInputStream(path);
		ObjectInputStream objStream = new ObjectInputStream(stream);
		String[][] preferences = (String[][])objStream.readObject();
		objStream.close();
		
		for (String[] p : preferences) {
			store.setValue(p[0], p[1]);
		}
		PluginActivator.getDefault().preferencesUpdated();
	}

}


