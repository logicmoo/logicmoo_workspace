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

package org.cs3.pdt.editor;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.IPrologProcessService;
import org.cs3.pdt.connector.util.DefaultErrorMessageProvider;
import org.cs3.pdt.connector.util.ErrorMessageProvider;
import org.cs3.pdt.editor.internal.editors.ColorManager;
import org.cs3.pdt.editor.internal.editors.CurrentProcessListener;
import org.cs3.pdt.editor.internal.editors.EditorConsultListener;
import org.cs3.pdt.editor.internal.editors.breakpoints.PDTBreakpointHandler;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin implements IStartup, ISelectionProvider{
	
	private List<ISelectionChangedListener> changeListeners = new ArrayList<ISelectionChangedListener>();
	private ISelection selection;

	public static final String MODULEPREFIX = "pdtplugin:";

	private static ColorManager colorManager; 
	
	// The shared instance.
	private static PDTPlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	private DefaultErrorMessageProvider errorMessageProvider;

	/**
	 * The constructor.
	 */
	public PDTPlugin() {
		super();
		plugin = this;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		IPreferencesService service = Platform.getPreferencesService();
		String qualifier = getBundle().getSymbolicName();
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}

	public void setPreferenceValue(String key, String value) {
		getPreferenceStore().setValue(key, value);
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		try {
			super.start(context);
			
			IPrologProcessService prologProcessService = PDTConnectorPlugin.getDefault().getPrologProcessService();
			CurrentProcessListener processListener = new CurrentProcessListener();
			prologProcessService.registerActivePrologProcessListener(processListener);
			PDTCommonPlugin.getDefault().registerProcessStartListener(processListener);
			prologProcessService.registerConsultListener(new EditorConsultListener());
			if (prologProcessService.hasActivePrologProcess()) {
				processListener.activePrologProcessChanged(prologProcessService.getActivePrologProcess());
			}
			PDTBreakpointHandler.getInstance();
		} catch (Throwable t) {
			Debug.report(t);
		}
	}

	public String getId() {
		return getBundle().getSymbolicName();
	}

	public ErrorMessageProvider getErrorMessageProvider() {
		if (errorMessageProvider == null) {
			errorMessageProvider = new DefaultErrorMessageProvider(this);
		};
		return errorMessageProvider;
	}

	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
			.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
				return null;
		}
		return  activeWorkbenchWindow.getActivePage();
	}
	

	@Override
	public void earlyStartup() {
	}

	public ColorManager getColorManager() {
		if(colorManager == null) {
			colorManager = new ColorManager();
		}
		return colorManager;
	}


	@Override
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		synchronized (changeListeners) {
			changeListeners.add(listener);
		}
	}

	@Override
	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		synchronized (changeListeners) {
			changeListeners.remove(listener);
		}
	}
	
	@Override
	public void setSelection(ISelection selection) {
		this.selection = selection;
		informListenersAboutEditorContent(selection);
	}
	
	public void informListenersAboutEditorContent(ISelection selection) {
		synchronized (changeListeners) {
			for (ISelectionChangedListener listener : changeListeners) {
				listener.selectionChanged(new SelectionChangedEvent(this, selection));
			}
		}
	}

	@Override
	public ISelection getSelection() {
		return selection;
	}

}


