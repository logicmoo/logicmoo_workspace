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

package org.cs3.pdt.connector.internal.preferences;

import org.cs3.pdt.connector.PDTConnector;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	protected PDTConnectorPlugin plugin;
	protected IPreferenceStore store;
	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		plugin = PDTConnectorPlugin.getDefault();
		store = plugin.getPreferenceStore();

		store.setDefault(PDTConnector.PREF_CONFIGURATION, PDTConnector.CONFIGURATION_SWI);
		PreferenceConfiguration.initializeDefaultPreferences(store);
		
		PreferenceConfiguration.initWithSWIPreferences(store);
	}

	
}


