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

/*
 * Created on 23.08.2004
 *
 */
package org.cs3.pdt.common.search;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.internal.ImageRepository;
import org.cs3.pdt.common.structureElements.AbstractPrologMatch;
import org.cs3.pdt.common.structureElements.ModuleMatch;
import org.cs3.pdt.common.structureElements.PredicateMatch;
import org.cs3.pdt.common.structureElements.SearchModuleElement;
import org.cs3.pdt.common.structureElements.SearchPredicateElement;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;


public class PrologSearchResultPage extends AbstractTextSearchViewPage {

	private PrologSearchContentProvider fContentProvider;
	protected static final Image IMAGE = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

	public PrologSearchResultPage(){
		super(AbstractTextSearchViewPage.FLAG_LAYOUT_TREE);
		init(NewSearchUI.getSearchResultView().getActivePage().getSite());
	}
	
	@Override
	protected void elementsChanged(Object[] objects) {
		if (fContentProvider != null)
			fContentProvider.elementsChanged(objects);
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	@Override
	protected void clear() {
		if (fContentProvider != null)
			fContentProvider.clear();
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	@Override
	protected void configureTreeViewer(TreeViewer viewer) {
		viewer.setLabelProvider(new DecoratingPrologSearchLabelProvider(new PrologSearchLabelProvider()));
		ColumnViewerToolTipSupport.enableFor(viewer, ToolTip.NO_RECREATE);
//		viewer.setLabelProvider(new DecoratingLabelProvider(new PrologSearchLabelProvider(), 
//				PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator()));
		fContentProvider= new PrologSearchTreeContentProvider(this);
		viewer.setContentProvider(fContentProvider);
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection = event.getSelection();
				if (selection instanceof TreeSelection) {
					Object firstElement = ((TreeSelection) selection).getFirstElement();
					Match m = null;
					if (firstElement instanceof SearchModuleElement) {
						m = ((SearchModuleElement) firstElement).getMatch();
					} else if (firstElement instanceof SearchPredicateElement) {
						m = ((SearchPredicateElement) firstElement).getFirstOccurrence();
					}
					if (m != null) {
						final Match match = m;
						UIJob job = new UIJob("Show Match") {
							@Override
							public IStatus runInUIThread(IProgressMonitor monitor) {
								try {
									showMatch(match, match.getOffset(), match.getLength(), true);
								} catch (PartInitException e) {
									Debug.report(e);
								}
								return Status.OK_STATUS;
							}
						};
						job.schedule();
					}
				}
			}
		});
		viewer.addTreeListener(new ITreeViewerListener() {
			@Override
			public void treeExpanded(TreeExpansionEvent event) {
				final AbstractTreeViewer treeViewer = event.getTreeViewer();
				PrologSearchTreeContentProvider contentProvider = (PrologSearchTreeContentProvider) treeViewer.getContentProvider();
				final Object element = event.getElement();
				int expandLevel = 1;
				Object[] children = contentProvider.getChildren(element);
				while (children.length == 1) {
					expandLevel++;
					children = contentProvider.getChildren(children[0]);
				}
				if (expandLevel > 1) {
					final int finalExpandLevel = expandLevel;
					treeViewer.getControl().getDisplay().asyncExec(new Runnable() {
						@Override
						public void run() {
							treeViewer.expandToLevel(element, finalExpandLevel);
						}
					});
				}
			}
			
			@Override
			public void treeCollapsed(TreeExpansionEvent event) {}
		});
	}
	
	@Override
	public StructuredViewer getViewer() {		
		return super.getViewer();
	}

	@Override
	protected void configureTableViewer(TableViewer viewer) {
		viewer.setLabelProvider(new DecoratingLabelProvider(new PrologSearchLabelProvider(), 
				PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator()));
		fContentProvider= new PrologSearchTableContentProvider(this);
		viewer.setContentProvider(fContentProvider);
	}
	
	@Override
	protected void showMatch(Match match, int currentOffset, int currentLength, boolean activate) throws PartInitException {
		if (match instanceof AbstractPrologMatch) {
			AbstractPrologMatch prologMatch = (AbstractPrologMatch) match;
			if (prologMatch.isLineLocation()) {
				PDTCommonUtil.selectInEditor(prologMatch.getLine(), prologMatch.getFile(), activate);
			} else {
				PDTCommonUtil.selectInEditor(currentOffset, currentLength, prologMatch.getFile(), activate, false);
			}
		} else if (match instanceof ModuleMatch) {
			ModuleMatch moduleMatch = (ModuleMatch) match;
			PDTCommonUtil.selectInEditor(moduleMatch.getOffset(), moduleMatch.getFile(), activate);
		} else if (match instanceof PredicateMatch) {
			UIUtils.displayMessageDialog(getSite().getShell(), "Show Match", "There is no source code for this predicate.");
		}
	}

}


