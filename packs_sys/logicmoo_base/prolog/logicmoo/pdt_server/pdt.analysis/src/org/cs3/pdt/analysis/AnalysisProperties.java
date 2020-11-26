/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.analysis;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;

public class AnalysisProperties {
	
	public static Set<String> getEnabledAnalyses(String factbase) {
		HashSet<String> result = new HashSet<>();
		String[] array = getSettings().getArray(factbase);
		if (array != null) {
			Collections.addAll(result, array);
		}
		return result;
	}
	
	public static void setEnabledAnalyses(String factbase, Collection<String> analyses) {
		getSettings().put(factbase, analyses.toArray(new String[analyses.size()]));
	}

	private static final String ANALYSIS_SECTION = "analyses";
	
	private static IDialogSettings section;

	private static IDialogSettings getSettings() {
		if (section == null) {
			IDialogSettings dialogSettings = PDTAnalysisPlugin.getDefault().getDialogSettings();
			section = dialogSettings.getSection(ANALYSIS_SECTION);
			if (section == null) {
				section = dialogSettings.addNewSection(ANALYSIS_SECTION);
			}
		}
		return section;
	}

}
