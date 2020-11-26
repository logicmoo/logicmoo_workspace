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

import java.io.IOException;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

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
		IPreferenceStore store = PDTCommonPlugin.getDefault().getPreferenceStore();
		
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			Debug.report(e);
			Debug.error("Could not find plugin installation dir.");
		}

		store.setDefault(PDTCommon.PREF_DEBUG_LEVEL, "WARNING");
		store.setDefault(PDTCommon.PREF_DEBUG_OUTPUT_TO,"LOGFILE");
		store.setDefault(PDTCommon.PREF_CLIENT_LOG_FILE_DIR, location);
		store.setDefault(PDTCommon.PREF_FILE_SIZE_LIMIT, 5000l);

		store.setDefault(PDTCommon.PREF_RECONSULT_ON_RESTART, PDTCommon.RECONSULT_ENTRY);
	}

	private String getLocation() throws IOException {
		return PDTCommonPlugin.getDefault().getStateLocation().toOSString();
	}

}


