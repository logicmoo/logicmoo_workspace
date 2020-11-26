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
package org.cs3.pdt.common.callhierachy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cs3.pdt.common.internal.ImageRepository;
import org.cs3.pdt.connector.util.ExternalPrologFilesProjectUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.ide.IDE.SharedImages;
import org.eclipse.ui.part.ViewPart;

public class CallHierarchyView extends ViewPart {
	
	private class ScopeAction extends Action implements IMenuCreator {
		
		private ScopeAction() {
			setText(null);
			setToolTipText("Select Scope");
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_SCOPE_WORKSPACE));
			setMenuCreator(this);
		}
		
		@Override
		public void run() {
			if (scope != null) {
				askAndSelectProjectScope();
			}
		}

		private Menu menu;
		
		@Override
		public void dispose() {
			if (menu != null) {
				menu.dispose();
			}
		}
		
		@Override
		public Menu getMenu(Control parent) {
			if (menu != null) {
				menu.dispose();
			}
			Menu newMenu = new Menu(parent);
			final MenuItem setWorkspaceScope = new MenuItem(newMenu, SWT.RADIO);
			setWorkspaceScope.setText("Workspace");
			setWorkspaceScope.setImage(ImageRepository.getImage(ImageRepository.CH_SCOPE_WORKSPACE));
			setWorkspaceScope.setSelection(scope == null);
			setWorkspaceScope.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (setWorkspaceScope.getSelection()) {
						setScope(null);
						setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_SCOPE_WORKSPACE));
					}
				}
			});
			final MenuItem setProjectScope = new MenuItem(newMenu, SWT.RADIO);
			setProjectScope.setText("Project...");
			setProjectScope.setImage(ImageRepository.getImage(ImageRepository.CH_SCOPE_PROJECT));
			setProjectScope.setSelection(scope != null);
			setProjectScope.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (setProjectScope.getSelection()) {
						askAndSelectProjectScope();
					}
				}
			});
			
			menu = newMenu;
			return menu;
		}

		@Override
		public Menu getMenu(Menu parent) {
			return null;
		}

		void askAndSelectProjectScope() {
			ListDialog dialog = new ListDialog(getSite().getShell());
			dialog.setTitle("Prolog Call Hierarchy");
			dialog.setMessage("Select Project Scope");
			dialog.setHelpAvailable(false);
			dialog.setContentProvider(new ArrayContentProvider());
			dialog.setLabelProvider(new LabelProvider() {
				@Override
				public String getText(Object element) {
					if (element instanceof IProject) {
						return ((IProject) element).getName();
					}
					return super.getText(element);
				}
				@Override
				public Image getImage(Object element) {
					return PlatformUI.getWorkbench().getSharedImages().getImage(SharedImages.IMG_OBJ_PROJECT);
				}
			});
			dialog.setInput(getProjects());
			int result = dialog.open();
			if (result == Window.OK) {
				Object[] selectedProjects = dialog.getResult();
				if (selectedProjects.length == 1 && selectedProjects[0] instanceof IProject) {
					setScope((IProject) selectedProjects[0]);
					setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_SCOPE_PROJECT));
				}
			}
		}

		private ArrayList<IProject> getProjects() {
			ArrayList<IProject> projects = new ArrayList<>();
			IProject externalPrologFiles = null;
			try {
				externalPrologFiles = ExternalPrologFilesProjectUtils.getExternalPrologFilesProject();
			} catch (CoreException e) {
				Debug.report(e);
			}
	    	for (IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
	    		if (project.isAccessible() && (!(externalPrologFiles.isAccessible() && project.equals(externalPrologFiles)))) {
	    			projects.add(project);
	    		}
	    	}
	    	Collections.sort(projects, new Comparator<IProject>() {
				@Override
				public int compare(IProject o1, IProject o2) {
					return String.CASE_INSENSITIVE_ORDER.compare(o1.getName(), o2.getName());
				}
			});
			return projects;
		}
	}
	
	private class HistoryAction extends Action implements IMenuCreator {
		
		private HistoryAction() {
			setText(null);
			setToolTipText("Select Root From History");
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_HISTORY_LIST));
			setMenuCreator(this);
		}
		
		private Menu menu;
		
		@Override
		public void dispose() {
			if (menu != null) {
				menu.dispose();
			}
		}
		
		@Override
		public Menu getMenu(Control parent) {
			if (menu != null) {
				menu.dispose();
			}
			Menu newMenu = new Menu(parent);
			for (Predicate predicate : history) {
				addMenuItem(newMenu, predicate);
			}
			new MenuItem(newMenu, SWT.SEPARATOR);
			MenuItem clearItem = new MenuItem(newMenu, SWT.PUSH);
			clearItem.setText("Clear History");
			clearItem.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					history.clear();
					CallHierarchyUtil.clearHistory();
				}
			});
			menu = newMenu;
			return menu;
		}
		
		private void addMenuItem(Menu newMenu, final Predicate predicate) {
			MenuItem item = new MenuItem(newMenu, SWT.RADIO, 0);
			item.setText(predicate.getLabel());
			item.setImage(CallHierarchyUtil.getImageForVisibility(predicate.getVisibility()));
			item.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					internalSetRoot(predicate);
				}
			});
			item.setSelection(predicate.equals(root));
		}
		
		@Override
		public Menu getMenu(Menu parent) {
			return null;
		}
		
	}
	
	public static final String ID = "org.cs3.pdt.common.callhierarchy";

	static final int CALLER_MODE = 0;
	static final int CALLEE_MODE = 1;
	
	private static final String MODE_PREFERENCE = "pdt.callhierarchy.mode";
	
	private TreeViewer callTreeViewer;
	private TableViewer locationTableViewer;
	
	private CallTreeContentProvider callTreeContentProvider;
	private CallTreeLabelProvider callTreeLabelProvider;
	
	private Action setRootAction;
	private Action refreshAction;
	private Action callerAction;
	private Action calleeAction;
	private Action historyAction;
	private Action scopeAction;
	
	private List<Predicate> history;

	private int mode = -1;
	
	private Predicate root;
	
	private IProject scope;
	
	private DeferredWorkbenchAdapter deferredWorkbenchAdapter = new DeferredWorkbenchAdapter();
	
	private LocationProvider locationProvider;

	@Override
	public void createPartControl(Composite parent) {
		Composite compLeft = new Composite(parent, SWT.NONE);
		compLeft.setLayout(new GridLayout(1, false));
		
		final Sash sash = new Sash(parent, SWT.VERTICAL);

		Composite compRight = new Composite(parent, SWT.NONE);
		compRight.setLayout(new GridLayout(1, false));
		
		createCallTreeViewer(compLeft);
		createLocationTableViewer(compRight);
		
		initSashFormData(parent, compLeft, sash, compRight);
		
		initToolbar();
		
		init();
		
		history = CallHierarchyUtil.loadHistory();
	}

	private void createCallTreeViewer(Composite compLeft) {
		callTreeViewer = new TreeViewer(compLeft, SWT.H_SCROLL | SWT.V_SCROLL);
		callTreeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		callTreeViewer.setAutoExpandLevel(2);
		
		callTreeContentProvider = new CallTreeContentProvider(this);
		callTreeViewer.setContentProvider(callTreeContentProvider);
		
		callTreeLabelProvider = new CallTreeLabelProvider();
		callTreeViewer.setLabelProvider(new DelegatingStyledCellLabelProvider(callTreeLabelProvider));
		
		callTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection selection = event.getSelection();
				if (!selection.isEmpty() && selection instanceof TreeSelection) {
					Object firstElement = ((TreeSelection) selection).getFirstElement();
					if (firstElement instanceof PredicateEdge) {
						locationProvider.fillLocations((PredicateEdge) firstElement);
						return;
					}
				}
				locationProvider.fillLocations(null);
			}
		});
		callTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection = event.getSelection();
				if (!selection.isEmpty() && selection instanceof TreeSelection) {
					Object firstElement = ((TreeSelection) selection).getFirstElement();
					if (firstElement instanceof PredicateEdge) {
						locationProvider.selectFirstLocationInEditor((PredicateEdge) firstElement);
					}
				}
			}
		});
	}

	private void createLocationTableViewer(Composite compRight) {
		locationTableViewer = new TableViewer(compRight, SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.VIRTUAL);
		locationTableViewer.getTable().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		TableViewerColumn iconColumn = new TableViewerColumn(locationTableViewer, SWT.NONE);
		iconColumn.getColumn().setWidth(30);
		
		TableViewerColumn lineColumn = new TableViewerColumn(locationTableViewer, SWT.NONE);
		lineColumn.getColumn().setWidth(50);
		lineColumn.getColumn().setText("Line");
		
		TableViewerColumn callColumn = new TableViewerColumn(locationTableViewer, SWT.NONE);
		callColumn.getColumn().setWidth(400);
		callColumn.getColumn().setText("Call");
		
		locationTableViewer.getTable().setHeaderVisible(true);
		
		locationTableViewer.setContentProvider(new ArrayContentProvider());
		locationTableViewer.setLabelProvider(new LocationTableLabelProvider());
		
		locationTableViewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection = event.getSelection();
				if (!selection.isEmpty() && selection instanceof IStructuredSelection) {
					Object firstElement = ((IStructuredSelection) selection).getFirstElement();
					if (firstElement instanceof Location) {
						CallHierarchyUtil.selectLocationInEditor((Location) firstElement);
					}
				}
			}
		});
		
		locationTableViewer.setInput(new ArrayList<Location>());
		
		locationProvider = new LocationProvider(locationTableViewer, callTreeViewer);
	}
	
	private void initSashFormData(final Composite parent, Composite compLeft, final Sash sash, Composite compRight) {
		final FormLayout form = new FormLayout();
		parent.setLayout(form);
		FormData button1Data = new FormData();
		button1Data.left = new FormAttachment(0, 0);
		button1Data.right = new FormAttachment(sash, 0);
		button1Data.top = new FormAttachment(0, 0);
		button1Data.bottom = new FormAttachment(100, 0);
		compLeft.setLayoutData (button1Data);
		
		final int limit = 20, percent = 50;
		final FormData sashData = new FormData();
		sashData.left = new FormAttachment(percent, 0);
		sashData.top = new FormAttachment(0, 0);
		sashData.bottom = new FormAttachment(100, 0);
		sash.setLayoutData(sashData);
		sash.addListener (SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event e) {
				Rectangle sashRect = sash.getBounds();
				Rectangle shellRect = parent.getClientArea();
				int right = shellRect.width - sashRect.width - limit;
				e.x = Math.max (Math.min (e.x, right), limit);
				if (e.x != sashRect.x)  {
					sashData.left = new FormAttachment(0, e.x);
					parent.layout();
				}
			}
		});
		
		FormData button2Data = new FormData();
		button2Data.left = new FormAttachment(sash, 0);
		button2Data.right = new FormAttachment(100, 0);
		button2Data.top = new FormAttachment(0, 0);
		button2Data.bottom = new FormAttachment(100, 0);
		compRight.setLayoutData(button2Data);
	}
	
	private void initToolbar() {
		IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
		
		setRootAction = new Action("Set Root Predicate") {
			private final Pattern p = Pattern.compile("([^:]+):([^:/]+)/(\\d+)");
			@Override
			public void run() {
				IInputValidator validator = new IInputValidator() {
					@Override
					public String isValid(String newText) {
						Matcher matcher = p.matcher(newText);
						if (matcher.matches()) {
							return null;
						}
						return "Invalid predicate indicator";
					}
				};
				InputDialog inputDialog = new InputDialog(getViewSite().getShell(), "Prolog Call Hierarchy", "Enter a predicate indicator of the form 'Module:Name/Arity'", "", validator);
				int result = inputDialog.open();
				if (result == Window.OK) {
					Matcher matcher = p.matcher(inputDialog.getValue());
					if (matcher.matches()) {
						String specifiedModule = matcher.group(1);
						String name = matcher.group(2);
						String arity = matcher.group(3);
						try {
							Predicate newRoot = CallHierarchyUtil.createPredicate(specifiedModule, name, arity);
							setRoot(newRoot);
						} catch (PrologProcessException e) {
							Debug.report(e);
						}
					}
				}
			}
		};
		setRootAction.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_ROOT));
		setRootAction.setToolTipText(setRootAction.getText());
		toolBarManager.add(setRootAction);
		
		refreshAction = new Action("Refresh") {
			@Override
			public void run() {
				refresh();
			}
		};
		refreshAction.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_REFRESH));
		refreshAction.setToolTipText(refreshAction.getText());
		toolBarManager.add(refreshAction);
		toolBarManager.add(new Separator());
		
		callerAction = new Action("Show Caller Hierarchy", Action.AS_RADIO_BUTTON) {
			@Override
			public void run() {
				setMode(CALLER_MODE);
			}
		};
		callerAction.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_CALLERS));
		callerAction.setToolTipText(callerAction.getText());
		toolBarManager.add(callerAction);
		
		calleeAction = new Action("Show Callee Hierarchy", Action.AS_RADIO_BUTTON) {
			@Override
			public void run() {
				setMode(CALLEE_MODE);
			}
		};
		calleeAction.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.CH_CALLEES));
		calleeAction.setToolTipText(calleeAction.getText());
		toolBarManager.add(calleeAction);
		
		scopeAction = new ScopeAction();
		toolBarManager.add(scopeAction);
		
		historyAction = new HistoryAction();
		toolBarManager.add(historyAction);
	}

	private void init() {
		int mode;
		try {
			mode = CallHierarchyUtil.getSettings().getInt(MODE_PREFERENCE);
		} catch (NumberFormatException e) {
			mode = CALLER_MODE;
			CallHierarchyUtil.getSettings().put(MODE_PREFERENCE, mode);
		}
		setMode(mode);
	}

	@Override
	public void setFocus() {
		callTreeViewer.getTree().setFocus();
	}
	
	private void setMode(int mode) {
		if (this.mode == mode) {
			return;
		}
		
		this.mode = mode;
		CallHierarchyUtil.getSettings().put(MODE_PREFERENCE, mode);
		callerAction.setChecked(mode == CALLER_MODE);
		calleeAction.setChecked(mode == CALLEE_MODE);
		
		deferredWorkbenchAdapter.setMode(mode);
		locationProvider.setMode(mode);
		
		updateLabel();
		if (root != null) {
			refresh();
		}
	}
	
	private void setScope(IProject scope) {
		if (this.scope == null && scope == null
				|| this.scope != null && this.scope.equals(scope)) {
			return;
		}
		this.scope = scope;
		
		deferredWorkbenchAdapter.setScope(scope);
		locationProvider.setScope(scope);
		
		updateLabel();
		refresh();
	}
	
	private void updateLabel() {
		if (root != null) {
			StringBuilder buf = new StringBuilder();
			if (mode == CALLER_MODE) {
				buf.append("Predicates calling ");
			} else {
				buf.append("Predicates called by ");
			}
			buf.append(root.getLabel());
			buf.append(" \u2013 in ");
			if (scope != null) {
				buf.append(scope.getName());
			} else {
				buf.append("Workspace");
			}
			setContentDescription(buf.toString());
		} else {
			setContentDescription("Select a root predicate to display its call hierarchy");
		}
	}
	
	void setRoot(Predicate root) {
		if (root != null) {
			history.remove(root);
			history.add(root);
			if (history.size() > CallHierarchyUtil.MAX_HISTORY_SIZE) {
				history.remove(0);
			}
			CallHierarchyUtil.saveHistory(history);
		}
		
		internalSetRoot(root);
	}
	
	private void internalSetRoot(Predicate root) {
		this.root = root;
		updateLabel();
		refresh();
	}

	private void refresh() {
		locationProvider.clearCache();
		if (root != null) {
			root.setDeferredWorkbenchAdapter(deferredWorkbenchAdapter);
			callTreeViewer.setInput(new Predicate[]{root});
		} else {
			callTreeViewer.setInput(new Predicate[0]);
		}
	}
}
