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

package org.cs3.pdt.graphicalviews.main;

import java.util.LinkedList;
import java.util.List;

import org.cs3.pdt.graphicalviews.model.labels.NodeLabelConfigurationsInitializer;
import org.cs3.prolog.connector.common.Util;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class PluginActivator extends AbstractUIPlugin {
	
	// The plug-in ID
	public static final String PLUGIN_ID = "pdt.yworks";
	
	private List<PreferencesUpdateListener> listeners = new LinkedList<PreferencesUpdateListener>();

	// The shared instance
	private static PluginActivator plugin;
	
	public PluginActivator() {
		NodeLabelConfigurationsInitializer.initialize();
	}

	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		if (Util.isMacOS()) {
			String c = "sun.lwawt.macosx.CViewEmbeddedFrame";
			try {
				Class.forName(c);
				SWT_AWT.embeddedFrameClass = c;
			} catch (Exception e) {}
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	public static PluginActivator getDefault() {
		return plugin;
	}
	
	public void addPreferencesUpdateListener(PreferencesUpdateListener listener) {
		listeners.add(listener);
	}
	
	public void removePreferencesUpdateListener(PreferencesUpdateListener listener) {
		listeners.remove(listener);
	}
	
	public void preferencesUpdated() {
		for (PreferencesUpdateListener l : listeners) {
			l.preferencesUpdated();
		}
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
}


