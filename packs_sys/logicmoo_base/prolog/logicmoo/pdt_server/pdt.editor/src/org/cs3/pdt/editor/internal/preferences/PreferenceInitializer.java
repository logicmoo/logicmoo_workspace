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
import org.cs3.pdt.editor.internal.editors.PDTColors;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = PDTPlugin.getDefault().getPreferenceStore();
		
		// Editor preferences
		store.setDefault(PDT.PREF_EXTERNAL_FILE_SAVE_WARNING, true);
		store.setDefault(PDT.PREF_AUTO_COMPLETE_ARGLIST, true);
		store.setDefault(PDT.PREF_CONSULT_ON_SAVE, true);
		store.setDefault(PDT.PREF_OUTLINESHOW_MULTIFILE, true);
		store.setDefault(PDT.PREF_OUTLINE_SHOW_ALL_CLAUSES, "true");
		store.setDefault(PDT.PREF_OUTLINE_FIRST_ARGUMENT_VARIABLE_FILE_SIZE, 100);
		
		// Editor Color preferences
		initializeDefaultPreferences_FontAndColor(store);		
	}

	private void initializeDefaultPreferences_FontAndColor(IPreferenceStore store){			
		PreferenceConverter.setDefault(store, PDTColors.PREF_BACKGROUND, PDTColors.BACKGROUND);
		PreferenceConverter.setDefault(store, PDTColors.PREF_BACKGROUND_EXTERNAL_FILES, PDTColors.BACKGROUND_EXTERN);
		PreferenceConverter.setDefault(store, PDTColors.PREF_DEFAULT, PDTColors.DEFAULT);
		PreferenceConverter.setDefault(store, PDTColors.PREF_STRING, PDTColors.STRING);
		PreferenceConverter.setDefault(store, PDTColors.PREF_COMMENT, PDTColors.COMMENT);		
		PreferenceConverter.setDefault(store, PDTColors.PREF_VARIABLE, PDTColors.VARIABLE);
		PreferenceConverter.setDefault(store, PDTColors.PREF_UNDEFINED, PDTColors.UNDEFINED);
		PreferenceConverter.setDefault(store, PDTColors.PREF_BUILTIN, PDTColors.BUILTIN);
		PreferenceConverter.setDefault(store, PDTColors.PREF_DYNAMIC, PDTColors.DYNAMIC);
		PreferenceConverter.setDefault(store, PDTColors.PREF_TRANSPARENT, PDTColors.TRANSPARENT);
		PreferenceConverter.setDefault(store, PDTColors.PREF_META, PDTColors.META);
	}

	
}


