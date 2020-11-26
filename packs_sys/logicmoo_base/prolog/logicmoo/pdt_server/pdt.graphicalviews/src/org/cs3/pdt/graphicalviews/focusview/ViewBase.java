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

package org.cs3.pdt.graphicalviews.focusview;

import javax.swing.JComponent;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.graphicalviews.internal.ImageRepository;
import org.cs3.pdt.graphicalviews.internal.ui.ToolBarAction;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.pdt.graphicalviews.preferences.MainPreferencePage;
import org.cs3.pdt.graphicalviews.preferences.PredicateLayoutPreferences;
import org.cs3.pdt.graphicalviews.preferences.PreferenceConstants;
import org.cs3.pdt.graphicalviews.view.modes.MouseHandler;
import org.cs3.pdt.graphicalviews.view.modes.OpenInEditorViewMode;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.eclipse.albireo.core.SwingControl;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;


public abstract class ViewBase extends ViewPart {
	
	public static final String ID = "org.cs3.pdt.graphicalviews.focusview.FocusView";
	private Composite viewContainer;
	private Label info;
	private String infoText = "", statusText = "";
	private ViewCoordinatorBase focusViewCoordinator;
	private boolean navigationEnabled = false;
	
	public ViewBase() {
	}

	protected abstract ViewCoordinatorBase createViewCoordinator();
	
	protected abstract GraphProcessLoaderBase createGraphProcessLoader(PDTGraphView pdtGraphView);
	
	@Override
	public void createPartControl(final Composite parent) {
		if (Util.isMacOS()) {
			createNotSupportedLabel(parent);
			return;
		}
		try {
			FormLayout layout = new FormLayout();
			parent.setLayout(layout);
			
			// View container initialization
			viewContainer = new Composite(parent, SWT.NONE);
			viewContainer.setLayout(new StackLayout());
			
			FormData viewContainerLD = new FormData();
			viewContainerLD.left = new FormAttachment(0, 0);
			viewContainerLD.right = new FormAttachment(100, 0);
			viewContainerLD.top = new FormAttachment(0, 0);
			viewContainerLD.bottom = new FormAttachment(100, -25);
			viewContainer.setLayoutData(viewContainerLD);
			
			initGraphNotLoadedLabel();
			
			initInfoLabel(parent);
			
			initButtons(parent);
			
			focusViewCoordinator = createViewCoordinator();
			String currentPath = getCurrentFilePath();
			if (currentPath != null) {
				focusViewCoordinator.swichFocusView(currentPath);
			}
		} catch (Throwable e) {
			Debug.report(e);
		}
	}
	
	private void createNotSupportedLabel(Composite parent) {
		try {
			Composite container = new Composite(parent, SWT.NONE);
			
			container.setLayout(new FillLayout());
			
			Label label = new Label(container, SWT.BORDER | SWT.CENTER);
			label.setFont(makeBold(label.getFont()));
			label.setText("This view is not supported on Mac OS");
			
			GridData gridData = new GridData();
			gridData.grabExcessHorizontalSpace = true;
			gridData.horizontalAlignment = GridData.FILL;
			gridData.grabExcessVerticalSpace = true;
			gridData.verticalAlignment = GridData.FILL;
			
			container.setLayoutData(gridData);
			
			viewContainer = container;
		} catch (Throwable e) {
			Debug.report(e);
		}
	}

	private Font makeBold(Font font) {
		FontRegistry fontRegistry = new FontRegistry();
		FontData fontData = font.getFontData()[0];

		fontData.setStyle(SWT.BOLD);
		fontRegistry.put("pdt_dummy", new FontData[]{fontData} );
		return fontRegistry.get("pdt_dummy");
	}

	private String getCurrentFilePath() {
		IWorkbenchPage page = this.getSite().getWorkbenchWindow().getActivePage();
		if (page == null) {
			return null;
		}
		for (IEditorReference p : page.getEditorReferences()) {
			IEditorPart editor = p.getEditor(false);
			if (page.isPartVisible(editor)) {
				String fileName = PDTCommonUtil.prologFileName(editor.getEditorInput());
				if (fileName.endsWith(".pl") || !fileName.endsWith(".pro")) {
					return fileName;
				}
			}
		}
		return null;
	}

	protected void initGraphNotLoadedLabel() {
		// Temporal label initialization
		Label l = new Label(viewContainer, SWT.NONE);
		l.setText("Graph is not loaded");
		((StackLayout)viewContainer.getLayout()).topControl = l;
		viewContainer.layout();
	}

	protected void initInfoLabel(final Composite parent) {
		// Info label initialization
		info = new Label(parent, SWT.NONE);
		
		FormData infoLD = new FormData();
		infoLD.left = new FormAttachment(0, 5);
		infoLD.top = new FormAttachment(viewContainer, 3);
		infoLD.right = new FormAttachment(100, 0);
		info.setLayoutData(infoLD);
	}

	protected void initButtons(final Composite parent) {
		IActionBars bars = this.getViewSite().getActionBars();
		IToolBarManager toolBarManager = bars.getToolBarManager();

		initViewButtons(toolBarManager);
		
		toolBarManager.add(new Separator("control"));
		
		toolBarManager.add(new ToolBarAction("Navigation", 
				ImageRepository.getImageDescriptor(ImageRepository.MOVE)) {

				@Override
				public int getStyle() {
					return IAction.AS_CHECK_BOX;
				}
			
				@Override
				public void performAction() {
					navigationEnabled = !navigationEnabled;
					focusViewCoordinator.currentFocusView.recalculateMode();
				}
			});
		
		toolBarManager.add(new ToolBarAction("Update", "WARNING: Current layout will be rearranged!", 
				ImageRepository.getImageDescriptor(ImageRepository.REFRESH)) {

				@Override
				public void performAction() {
					updateCurrentFocusView();	
				}
			});
		
		toolBarManager.add(new Separator("layout"));
		
		toolBarManager.add(new ToolBarAction("Hierarchical layout", 
				org.cs3.pdt.graphicalviews.internal.ImageRepository.getImageDescriptor(
						org.cs3.pdt.graphicalviews.internal.ImageRepository.HIERARCHY)) {

				@Override
				public void performAction() {
					PredicateLayoutPreferences.setLayoutPreference(PreferenceConstants.LAYOUT_HIERARCHY);
					updateCurrentFocusViewLayout();
				}
			});
		
		toolBarManager.add(new ToolBarAction("Organic layout", 
				org.cs3.pdt.graphicalviews.internal.ImageRepository.getImageDescriptor(
						org.cs3.pdt.graphicalviews.internal.ImageRepository.ORGANIC)) {

				@Override
				public void performAction() {
					PredicateLayoutPreferences.setLayoutPreference(PreferenceConstants.LAYOUT_ORGANIC);
					updateCurrentFocusViewLayout();
				}
			});
		
		toolBarManager.add(new Separator("preferences"));
		
		toolBarManager.add(new ToolBarAction("Preferences", 
				ImageRepository.getImageDescriptor(ImageRepository.PREFERENCES)) {

				@Override
				public void performAction() {
					PreferenceManager globalmgr = PlatformUI.getWorkbench().getPreferenceManager();
					IPreferenceNode node = globalmgr.find("org.cs3.pdt.common.internal.preferences.PDTCommonPreferencePage/org.cs3.pdt.graphicalviews.preferences.MainPreferencePage");
					
					IPreferencePage page = new MainPreferencePage();
					page.setTitle("Context View");
					IPreferenceNode root = new PreferenceNode("PreferencePage", page);
					root.add(node);
					
					PreferenceManager mgr = new PreferenceManager('.', (PreferenceNode)root);
					
					PreferenceDialog dialog = new PreferenceDialog(getSite().getShell(), mgr);
					dialog.create();
					dialog.setMessage(page.getTitle());
					dialog.open();
				}
			});

		toolBarManager.add(new ToolBarAction("Help", 
				ImageRepository.getImageDescriptor(ImageRepository.HELP)) {
			
			@Override
			public void performAction() {
				new HelpDialog(getSite().getShell()).open();
			}
		});

	}

	protected void initViewButtons(IToolBarManager toolBarManager) {
	}
	
	@Override
	public void setFocus() {
		viewContainer.setFocus();
	}

	public Composite getViewContainer() {
		return viewContainer;
	}

	public void setCurrentFocusView(FocusViewControl focusView) {
		((StackLayout)viewContainer.getLayout()).topControl = focusView;
		viewContainer.layout();
	}
	
	public void updateCurrentFocusView() {
		FocusViewControl f = getCurrentFocusView();
		if (f != null) {
			f.reload();
		}
	}
	
	public void updateCurrentFocusViewLayout() {
		FocusViewControl f = getCurrentFocusView();
		if (f != null)
			f.updateLayout();
	}

	public FocusViewControl createFocusViewControl(PDTGraphView pdtGraphView, GraphProcessLoaderBase loader) {
		return new FocusViewControl(pdtGraphView, loader);
	}
	
	private FocusViewControl getCurrentFocusView() {
		Control f = ((StackLayout)viewContainer.getLayout()).topControl;
		if (f instanceof FocusViewControl)
			return (FocusViewControl) f;
		return null;
	}
	
	public String getInfoText() {
		return infoText;
	}
	
	public void setInfoText(String text) {
		infoText = text;
		updateInfo();
	}

	public String getStatusText() {
		return statusText;
	}
	
	public void setStatusText(String text) {
		statusText = text;
		updateInfo();
	}
	
	protected void updateInfo() {
		final String text = statusText + " " + infoText;
		new UIJob("Update Status") {
		    @Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
		        if (info.isDisposed()) {
		        	return Status.CANCEL_STATUS;
		        } else {
			    	info.setText(text);
			        return Status.OK_STATUS;
		        }
		    }
		}.schedule();
	}

	public boolean isNavigationModeEnabled() {
		return navigationEnabled;
	}
	
	@Override
	public void dispose() {
		if (focusViewCoordinator != null) {
			focusViewCoordinator.dispose();
		}
		super.dispose();
	}
	
	// DO NOT MOVE OUT OF THE CLASS
	public class FocusViewControl extends SwingControl {

		private final String FOCUS_VIEW_IS_OUTDATED = "[FocusView is outdated]";
		
		private final PDTGraphView pdtGraphView;
		private final GraphProcessLoaderBase processLoader;
		
		private boolean isDirty = false;
		
		public FocusViewControl(PDTGraphView pdtGraphView, GraphProcessLoaderBase processLoader) {
			super(getViewContainer(), SWT.NONE);
			
			this.pdtGraphView = pdtGraphView;
			this.processLoader = processLoader;
			
			pdtGraphView.addViewMode(new OpenInEditorViewMode(pdtGraphView, processLoader));
			pdtGraphView.addViewMode(new MouseHandler(this));
		}
		
		public boolean isNavigationEnabled() {
			return navigationEnabled;
		}
		
		public PDTGraphView getPdtGraphView() {
			return pdtGraphView;
		}
		
		public void recalculateMode() {
			this.pdtGraphView.recalculateMode();
		}

		public boolean isEmpty() {
			return pdtGraphView.isEmpty();
		}
		
		public void setDirty() {
			setStatusText(FOCUS_VIEW_IS_OUTDATED);
			isDirty = true;
		}
		
		public boolean isDirty() {
			return isDirty;
		}
		
		public GraphProcessLoaderBase getProcessLoader() {
			return processLoader;
		}

		public void reload() {
			Job j = new Job("Reloading Graph") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					processLoader.loadGraph();
					setStatusText("");
					
					isDirty = false;

					return Status.OK_STATUS;
				}
			};
			j.schedule();
		}
		
		public void updateLayout() {
			pdtGraphView.updateLayout();
		}

		@Override
		protected JComponent createSwingComponent() {
			return pdtGraphView;
		}

		@Override
		public Composite getLayoutAncestor() {
			return getViewContainer();
		}

		public String getInfoText() {
			return infoText;
		}

		public void setInfoText(String text) {
			ViewBase.this.setInfoText(text);
		}
	}
}