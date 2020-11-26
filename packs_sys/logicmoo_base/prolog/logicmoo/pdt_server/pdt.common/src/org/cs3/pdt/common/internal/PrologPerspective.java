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

package org.cs3.pdt.common.internal;

import org.cs3.pdt.common.callhierachy.CallHierarchyView;
import org.cs3.prolog.connector.common.Util;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.views.IViewRegistry;

public class PrologPerspective implements IPerspectiveFactory {
	
	private static final String CONSOLE_VIEW_ID = "org.cs3.pdt.console.internal.views.PrologConsoleView";
	public static final String CONSOLE_FOLDER = "prolog.perspective.console.folder";
	public static final String VIEWS_FOLDER = "prolog.perspective.views.folder";
	
	private static final String PACKAGE_EXPLORER = "org.eclipse.jdt.ui.PackageExplorer";
	
	private static final String CONTEXT_VIEW_ID = "pdt.view.focus";
	private static final String GLOBAL_VIEW_ID = "pdt.view.global";
	private static final String LOAD_GRAPH_ID = "pdt.view.dependencies";
	private static final String LOGTALK_DIAGRAMS_VIEW_ID = "pdt.view.logtalk.entity";

	@Override
	public void createInitialLayout(IPageLayout layout) {
		IViewRegistry viewRegistry = PlatformUI.getWorkbench().getViewRegistry();
		defineActions(layout);
		defineViewShortCuts(layout, viewRegistry);
		defineLayout(layout, viewRegistry);
	}

	public void defineActions(IPageLayout layout) {
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");
		layout.addNewWizardShortcut("pdt.module.wizard");
	}

	private void defineViewShortCuts(IPageLayout layout, IViewRegistry viewRegistry) {
		layout.addShowViewShortcut(CallHierarchyView.ID);
		layout.addShowViewShortcut(CONSOLE_VIEW_ID);
		if (!Util.isMacOS() && viewRegistry.find("") != null) {
			layout.addShowViewShortcut(CONTEXT_VIEW_ID);
			layout.addShowViewShortcut(GLOBAL_VIEW_ID);
			layout.addShowViewShortcut(LOAD_GRAPH_ID);
			layout.addShowViewShortcut(LOGTALK_DIAGRAMS_VIEW_ID);
		}
	}
	
	public void defineLayout(IPageLayout layout, IViewRegistry viewRegistry) {
		String editorArea = layout.getEditorArea();

		IFolderLayout consoleFolder = layout.createFolder(CONSOLE_FOLDER, IPageLayout.BOTTOM, 0.65f, editorArea);
		
		consoleFolder.addView(CONSOLE_VIEW_ID);
		consoleFolder.addView(IPageLayout.ID_PROBLEM_VIEW);
		consoleFolder.addView(NewSearchUI.SEARCH_VIEW_ID);
		
		if (viewRegistry.find(PACKAGE_EXPLORER) != null) {
			layout.addView(PACKAGE_EXPLORER, IPageLayout.LEFT, 0.2f, editorArea);
		} else {
			layout.addView(IPageLayout.ID_PROJECT_EXPLORER, IPageLayout.LEFT, 0.2f, editorArea);
		}
		layout.addView(IPageLayout.ID_OUTLINE, IPageLayout.RIGHT, 0.8f, editorArea);
		
		if (!Util.isMacOS() && viewRegistry.find(CONTEXT_VIEW_ID) != null) {
			IFolderLayout graphicalViewsFolder = layout.createFolder(VIEWS_FOLDER, IPageLayout.RIGHT, 0.5f, CONSOLE_FOLDER);
			graphicalViewsFolder.addView(CONTEXT_VIEW_ID);
			graphicalViewsFolder.addView(GLOBAL_VIEW_ID);
			graphicalViewsFolder.addView(LOAD_GRAPH_ID);
		}
	}
	
}


