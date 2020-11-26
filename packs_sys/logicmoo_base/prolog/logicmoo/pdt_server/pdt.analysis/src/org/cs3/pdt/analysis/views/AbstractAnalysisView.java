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
package org.cs3.pdt.analysis.views;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

import org.cs3.pdt.analysis.ImageRepository;
import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IAnalysisCategory;
import org.cs3.pdt.analysis.model.IResult;
import org.cs3.pdt.analysis.model.IResultModel;
import org.cs3.pdt.common.PDTCommonUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.MarkerUtilities;


public abstract class AbstractAnalysisView extends ViewPart {
	
	private CheckboxTreeViewer analysisTreeViewer;
	private TreeViewer resultTreeViewer;
	
	private TreeViewerColumn[] analysisTreeViewerColumns;
	private TreeViewerColumn[] resultTreeViewerColumns;
	
	private AnalysisTableContentProvider analysisContentProvider;
	private ResultTableContentProvider resultContentProvider;
	
	public AbstractAnalysisView(){
	}
	
	protected abstract IResultModel getResultModel();
	
	protected abstract void log(Exception e);
	
	protected CheckboxTreeViewer getAnalysisTreeViewer() {
		return analysisTreeViewer;
	}

	protected TreeViewer getResultTreeViewer() {
		return resultTreeViewer;
	}

	protected TreeViewerColumn[] getAnalysisTreeViewerColumns() {
		return analysisTreeViewerColumns;
	}

	protected TreeViewerColumn[] getResultTreeViewerColumns() {
		return resultTreeViewerColumns;
	}

	protected AnalysisTableContentProvider getAnalysisContentProvider() {
		return analysisContentProvider;
	}

	protected ResultTableContentProvider getResultContentProvider() {
		return resultContentProvider;
	}

	@Override
	public void createPartControl(final Composite parent) {
		Composite compLeft = new Composite(parent, SWT.NONE);
		final Sash sash = new Sash (parent, SWT.VERTICAL);

		compLeft.setLayout(new GridLayout(1, false));
		Composite compRight = new Composite(parent, SWT.NONE);
		compRight.setLayout(new GridLayout(1, false));
		createControllerArea(compLeft);
		analysisTreeViewer = createAnalysisTreeViewer(compLeft);
		
		Label resultLabel = new Label(compRight, SWT.NONE);
		resultLabel.setText("Results for selected analyses");

		resultTreeViewer = createResultTreeViewer(compRight);
		initSashFormData(parent, compLeft, sash, compRight);
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
	
	protected Control createControllerArea(Composite parent) {
		Composite controllerArea = new Composite(parent, SWT.NONE);
	    controllerArea.setLayout(new GridLayout(2, false));
	    controllerArea.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
	    
	    Control selector = createSelector(controllerArea);
	    selector.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
		
		ToolBar controllerToolBar = new ToolBar(controllerArea, SWT.HORIZONTAL);
		controllerToolBar.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
		ToolBarManager controllerToolBarManager = new ToolBarManager(controllerToolBar);
	    createToolbarActions(controllerToolBarManager);
	    controllerToolBarManager.update(true);
		
	    return controllerArea;
	}
	
	protected abstract Control createSelector(Composite parent);
	
	protected void createToolbarActions(IToolBarManager toolBarManager) {
		toolBarManager.add(new Action(){
			@Override
			public void run(){
				runEnabledAnalyses();
			}
			
			@Override
			public ImageDescriptor getImageDescriptor() {
				return ImageRepository.getImageDescriptor(ImageRepository.ICON_RUN);
			}

			@Override
			public String getToolTipText() {
				return getText();
			}

			@Override
			public String getText() {
				return "Run all enabled analyses";
			}
		});
		
		toolBarManager.add(new Action(){
			@Override
			public void run(){
				clearAllResults();
			}
			@Override
			public ImageDescriptor getImageDescriptor() {
				return ImageRepository.getImageDescriptor(ImageRepository.ICON_REMOVE);
			}

			@Override
			public String getToolTipText() {
				return getText();
			}

			@Override
			public String getText() {
				return "Clear all analysis results";
			}
		});
		toolBarManager.add(new Separator());
	}
	
	protected abstract void runEnabledAnalyses();
	
	protected abstract void clearAllResults();
	
	protected abstract void runSelectedAnalyses();
	
	protected abstract void clearSelectedAnalyses();
	
	protected abstract void refreshAnalysisEnablement();

	protected TreeViewer createResultTreeViewer(Composite parent) {
		final TreeViewer treeViewer = new TreeViewer(parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
		GridData gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessVerticalSpace = true;
		gridData.verticalAlignment = GridData.FILL;

		Tree tree = treeViewer.getTree();
		
		tree.addMouseListener(new MouseListener() {

			@Override
			public void mouseUp(MouseEvent e) {}

			@Override
			public void mouseDown(MouseEvent e) {}

			@Override
			public void mouseDoubleClick(MouseEvent e) {
				ISelection selection = treeViewer.getSelection();
				if (selection instanceof ITreeSelection) {
					Object o = ((ITreeSelection) selection).getFirstElement();
					if (o instanceof IResult) {
						selectResultInEditor((IResult) o);
					}
				}
			}
		});
		tree.setLayoutData(gridData);
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);

		resultContentProvider = new ResultTableContentProvider(getResultModel());
		treeViewer.setContentProvider(resultContentProvider);
		treeViewer.setInput(new Object());

		resultTreeViewerColumns = new TreeViewerColumn[3];

		for (int i = 0; i < resultTreeViewerColumns.length; i++) {
			resultTreeViewerColumns[i] = new TreeViewerColumn(treeViewer, SWT.NONE);
			resultTreeViewerColumns[i].getColumn().setMoveable(true);
		}

		resultTreeViewerColumns[0].getColumn().setWidth(300);
		resultTreeViewerColumns[0].getColumn().setText("Description");
		resultTreeViewerColumns[0].setLabelProvider(new ResultTableDescriptionColumnLabelProvider());
		
		resultTreeViewerColumns[1].getColumn().setWidth(150);
		resultTreeViewerColumns[1].getColumn().setText("Resource");
		resultTreeViewerColumns[1].setLabelProvider(new ResultTableResourceColumnLabelProvider());
		
		resultTreeViewerColumns[2].getColumn().setWidth(100);
		resultTreeViewerColumns[2].getColumn().setText("Location");
		resultTreeViewerColumns[2].setLabelProvider(new ResultTableLocationColumnLabelProvider());
		
		return treeViewer;
	}
	
	protected void selectResultInEditor(IResult result) {
		IFile resource = result.getResource();
		int start = MarkerUtilities.getCharStart(result.getMarker());
		int end = MarkerUtilities.getCharEnd(result.getMarker());
		try {
			PDTCommonUtil.selectInEditor(start, end - start, resource, true, false);
		} catch (PartInitException e) {
			log(e);
		}
	}
	
	protected CheckboxTreeViewer createAnalysisTreeViewer(Composite parent) {
		final CheckboxTreeViewer treeViewer = new CheckboxTreeViewer(parent, SWT.CHECK | SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
	    GridData gridData = new GridData();
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.horizontalAlignment = GridData.FILL;
	    gridData.grabExcessVerticalSpace = true;
	    gridData.verticalAlignment = GridData.FILL;
		
		final Tree tree = treeViewer.getTree();
		tree.setLayoutData(gridData);
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);
		
		Menu menu = new Menu(tree.getShell(), SWT.POP_UP);
		
		MenuItem mnItemRun = new MenuItem(menu, SWT.PUSH);
		mnItemRun.setText("Run analysis");
		mnItemRun.setImage(ImageRepository.getImage(ImageRepository.ICON_RUN));
		mnItemRun.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent evt) {
				runSelectedAnalyses();
			}
		});
		
		MenuItem mnItemRemove = new MenuItem (menu, SWT.PUSH);
		mnItemRemove.setText("Clear analysis results");
		mnItemRemove.setImage(ImageRepository.getImage(ImageRepository.ICON_REMOVE));
		mnItemRemove.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				clearSelectedAnalyses();
			}
		});
		
		new MenuItem(menu, SWT.SEPARATOR);
		
		MenuItem mnItemSelectAll = new MenuItem(menu, SWT.PUSH);
		mnItemSelectAll.setText("Enable all");
		mnItemSelectAll.setImage(ImageRepository.getImage(ImageRepository.ICON_SELECT_ALL));
		mnItemSelectAll.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setEnabledOnAll(true);
				analysisCheckStateChanged();
			}
		});

		MenuItem mnItemSelectNone = new MenuItem(menu, SWT.PUSH);
		mnItemSelectNone.setText("Disable all");
		mnItemSelectNone.setImage(ImageRepository.getImage(ImageRepository.ICON_SELECT_NONE));
		mnItemSelectNone.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setEnabledOnAll(false);
				analysisCheckStateChanged();
			}
		});
		
		MenuItem mnItemExpandAll = new MenuItem(menu, SWT.PUSH);
		mnItemExpandAll.setText("Expand all");
		mnItemExpandAll.setImage(ImageRepository.getImage(ImageRepository.ICON_EXPAND_ALL));
		mnItemExpandAll.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				treeViewer.expandAll();
			}
		});
		
		MenuItem mnItemCollapseAll = new MenuItem(menu, SWT.PUSH);
		mnItemCollapseAll.setText("Collapse all");
		mnItemCollapseAll.setImage(ImageRepository.getImage(ImageRepository.ICON_COLLAPSE_ALL));
		mnItemCollapseAll.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				treeViewer.collapseAll();
			}
		});
		
		tree.setMenu(menu);
		treeViewer.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(CheckStateChangedEvent event) {
				treeViewer.setSubtreeChecked(event.getElement(), event.getChecked());
				checkAncestorPath(event.getElement(), event.getChecked(), false, null);
				
				analysisCheckStateChanged();
			}
		});
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				analysisSelectionChanged(event.getSelection());
			}
		});
		
		analysisContentProvider = new AnalysisTableContentProvider(getResultModel());
		analysisContentProvider.setView(this);
		treeViewer.setContentProvider(analysisContentProvider);
		treeViewer.setInput(new Object());
		
		analysisTreeViewerColumns = new TreeViewerColumn[3];
		
		for (int i = 0; i < analysisTreeViewerColumns.length; i++) {
			analysisTreeViewerColumns[i] = new TreeViewerColumn(treeViewer, SWT.NONE);
			analysisTreeViewerColumns[i].getColumn().setMoveable(true);
		}
		
		analysisTreeViewerColumns[0].getColumn().setWidth(200);
		analysisTreeViewerColumns[0].getColumn().setText("Name");
		analysisTreeViewerColumns[0].setLabelProvider(new AnalysisTableNameColumnLabelProvider());
		
		analysisTreeViewerColumns[1].getColumn().setWidth(300);
		analysisTreeViewerColumns[1].getColumn().setText("Description");
		analysisTreeViewerColumns[1].setLabelProvider(new AnalysisTableDescriptionColumnLabelProvider());
		
		analysisTreeViewerColumns[2].getColumn().setWidth(100);
		analysisTreeViewerColumns[2].getColumn().setText("# Results");
		analysisTreeViewerColumns[2].setLabelProvider(new AnalysisTableCountColumnLabelProvider());
		
		ColumnViewerToolTipSupport.enableFor(treeViewer, ToolTip.NO_RECREATE);
		
		return treeViewer;
	}
	
	protected void analysisSelectionChanged(ISelection newSelection) {
		resultContentProvider.setSelectedAnalyses(getSelectedAnalyses(newSelection));
	}
	
	protected void analysisCheckStateChanged(){
	}
	
	protected List<IAnalysis> getSelectedAnalyses(ISelection selection) {
		TreeSet<IAnalysis> result = new TreeSet<IAnalysis>();
		if (!(selection instanceof ITreeSelection)) {
			return new ArrayList<>(result);
		}
		ITreeSelection treeSelection = (ITreeSelection) selection;
		for (Object o : treeSelection.toArray()) {
			if (o instanceof IAnalysis) {
				result.add((IAnalysis) o);
			} else if (o instanceof IAnalysisCategory) {
				result.addAll(((IAnalysisCategory) o).getAnalyses());
			}
		}
		return new ArrayList<>(result);
	}
	
	protected List<IAnalysis> getEnabledAnalyses() {
		TreeSet<IAnalysis> result = new TreeSet<IAnalysis>();
		for (Object element : analysisTreeViewer.getCheckedElements()) {
			if (element instanceof IAnalysis) {
				result.add((IAnalysis) element);
			}
		}
		return new ArrayList<>(result);
	}
	
	protected void setEnabledAnalyses(List<IAnalysis> analyses) {
		Object[] elements = analyses.toArray();
		
		analysisTreeViewer.setCheckedElements(elements);
		
		HashSet<Object> checkedElements = new HashSet<>();
		for (Object element : elements) {
			checkAncestorPath(element, true, false, checkedElements);
		}
	}
	
	protected void setEnabledOnAll(boolean enabled) {
		TreePath[] expandedTreePaths = analysisTreeViewer.getExpandedTreePaths();
		analysisTreeViewer.expandAll();
		
		for (Object element : analysisTreeViewer.getExpandedElements()){
			analysisTreeViewer.setSubtreeChecked(element, enabled);
		}
		
		analysisTreeViewer.setExpandedTreePaths(expandedTreePaths);
	}

	private void checkAncestorPath(Object element, boolean checked, boolean grayed, HashSet<Object> checkedElements) {
	    if (element == null) {
	    	return;
	    }
	    if (checkedElements != null && checkedElements.contains(element)) {
	    	return;
	    }
	    if (grayed) {
	        checked = true;
	    } else {
	        Object[] children = analysisContentProvider.getChildren(element);
	        for (Object child : children) {
	            if (analysisTreeViewer.getGrayed(child) || checked != analysisTreeViewer.getChecked(child)) {
	                checked = true;
	                grayed = true;
	                break;
	            }
	        }
	    }
	    analysisTreeViewer.setChecked(element, checked);
	    analysisTreeViewer.setGrayed(element, grayed);
	    
	    if (checkedElements != null) {
	    	checkedElements.add(element);
	    }
	    
	    checkAncestorPath(analysisContentProvider.getParent(element), checked, grayed, checkedElements);
	}

	@Override
	public void setFocus() {
		analysisTreeViewer.getTree().setFocus();
	}
	
}