/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.editors;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cs3.pdt.editor.PDTPlugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class ColorManager {
	
	private IPreferenceStore store;
	
	protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>(10);
	
	public ColorManager() {
		store = PDTPlugin.getDefault().getPreferenceStore();
	}

	public void dispose() {
		Iterator<Color> e = fColorTable.values().iterator();
		while (e.hasNext())
			 e.next().dispose();
	}
	
	public Color getColor(RGB rgb) {
		Color color = fColorTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			fColorTable.put(rgb, color);
		}
		return color;
	}

	public RGB getBackgroundColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_BACKGROUND);
	}
	
	public RGB getExternBackgroundColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_BACKGROUND_EXTERNAL_FILES);
	}

	public RGB getDefaultColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_DEFAULT);  
	}

	public RGB getStringColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_STRING);
	}

	public RGB getCommentColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_COMMENT);	
	}

	public RGB getVariableColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_VARIABLE);
	}

	public RGB getUndefinedColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_UNDEFINED);
	}

	public RGB getKeywordColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_BUILTIN);
	}

	public RGB getDynamicColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_DYNAMIC);
	}

	public RGB getTransparentColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_TRANSPARENT);
	}
	
	public RGB getMetaColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_META);
	}
}


