package org.cs3.pdt.graphicalviews.preferences;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.eclipse.jface.preference.IPreferenceStore;

public class PredicateVisibilityPreferences {

	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static boolean showPDTPredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.VISIBILITY_PDT_PREDICATES);
	}
	public static void setShowPDTPredicates(boolean value) {
		getCurrentPreferences().setValue(PreferenceConstants.VISIBILITY_PDT_PREDICATES, value);
	}

	public static boolean showSWIPredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.VISIBILITY_SWI_PREDICATES);
	}
	public static void setShowSWIPredicates(boolean value) {
		getCurrentPreferences().setValue(PreferenceConstants.VISIBILITY_SWI_PREDICATES, value);
	}

}
