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

package org.cs3.pdt.editor.internal.views.lightweightOutline;

/*******************************************************************************
 * Copyright (c) 2000, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import java.util.Map;

import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.internal.queries.PDTOutlineQuery;
import org.cs3.pdt.editor.internal.structureElements.OutlineClauseElement;
import org.cs3.pdt.editor.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.editor.internal.structureElements.OutlinePredicateElement;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

/**
 * Show outline in light-weight control.
 *
 * @since 2.1
 */
public class PrologOutlineInformationControl extends AbstractInformationControl {

	private OutlineContentProvider fOutlineContentProvider;
	private PrologSourceFileModel fInput;
	private ViewerComparator fOutlineSorter;
	private LabelProvider fInnerLabelProvider;
	private LexicalSortingAction fLexicalSortingAction;
	/**
	 * Category filter action group.
	 * @since 3.2
	 */
	public String fPattern;
	private IDocument document;

	/**
	 * Creates a new Java outline information control.
	 * @param iDocument 
	 * @param parent
	 * @param shellStyle
	 * @param treeStyle
	 * @param commandId
	 */
	public PrologOutlineInformationControl(IDocument document, Shell parent, int shellStyle, int treeStyle) {
		super(parent, shellStyle, treeStyle, true);
		this.document = document;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Text createFilterText(Composite parent) {
		Text text= super.createFilterText(parent);
		return text;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected TreeViewer createTreeViewer(Composite parent, int style) {
		Tree tree= new Tree(parent, SWT.SINGLE | (style & ~SWT.MULTI));
		GridData gd= new GridData(GridData.FILL_BOTH);
		gd.heightHint= tree.getItemHeight() * 12;
		tree.setLayoutData(gd);

		final TreeViewer treeViewer= new OutlineTreeViewer(tree);
		treeViewer.addDoubleClickListener(new IDoubleClickListener() {
			
			@Override
			public void doubleClick(DoubleClickEvent event) {
				gotoSelectedElement();
			}
		});
		// Hard-coded filters
		treeViewer.addFilter(new NamePatternFilter(this, this.getMatcher()));

		fInnerLabelProvider = new OutlineLabelProvider();
		treeViewer.setLabelProvider(fInnerLabelProvider);

		fLexicalSortingAction= new LexicalSortingAction(/*this,*/ treeViewer);

		fOutlineContentProvider= new OutlineContentProvider();
		treeViewer.setContentProvider(fOutlineContentProvider);
		fOutlineSorter= new ViewerComparator();
		treeViewer.setComparator(fOutlineSorter);
		treeViewer.setAutoExpandLevel(2);

		return treeViewer;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String getStatusFieldText() {
		return "";
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#getId()
	 * @since 3.0
	 */
	@Override
	protected String getId() {
		return "org.eclipse.jdt.internal.ui.text.QuickOutline"; //$NON-NLS-1$
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setInput(Object information) {
		if(information instanceof String) {
			String fileName = (String)information;
			Map<String, OutlineModuleElement> modules = PDTOutlineQuery.getProgramElementsForFile(fileName/*, getShell()*/);

			PrologSourceFileModel model = new PrologSourceFileModel(modules);
			inputChanged(model, modules.size()>0?modules.get(0):null);
			fInput=model;
			return;
		}
		if (information == null || information instanceof String) {
			inputChanged(null, null);
			return;
		}

		inputChanged(fInput, information);

	}

	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#fillViewMenu(org.eclipse.jface.action.IMenuManager)
	 */
	@Override
	protected void fillViewMenu(IMenuManager viewMenu) {
		super.fillViewMenu(viewMenu);

		viewMenu.add(new Separator("Sorters")); //$NON-NLS-1$
		viewMenu.add(fLexicalSortingAction);
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#setMatcherString(java.lang.String, boolean)
	 * @since 3.2
	 */
	@Override
	protected void setMatcherString(String pattern, boolean update) {
		fPattern= pattern;
		if (pattern.length() == 0) {
			super.setMatcherString(pattern, update);
			return;
		}

		boolean ignoreCase= pattern.toLowerCase().equals(pattern);
		String pattern2= "*" + pattern; //$NON-NLS-1$
		setfStringMatcher(new OrStringMatcher(pattern, pattern2, ignoreCase, false));

		if (update)
			stringMatcherUpdated();

	}

	protected void gotoSelectedElement() {
		Object selection = getSelectedElement();
		int line = -1;
		if (selection instanceof OutlinePredicateElement) {
			OutlinePredicateElement predicate=(OutlinePredicateElement)selection;
			line = predicate.getLine()-1;
		} 
		if (selection instanceof OutlineClauseElement) {
			OutlineClauseElement occurance = (OutlineClauseElement)selection;
			line = occurance.getLine()-1;
		}
		ISelection textSelection;
		try {
			textSelection = new TextSelection(document,document.getLineOffset(line),0);
			UIUtils.getActiveEditor().getEditorSite().getSelectionProvider().setSelection(textSelection);
		} catch (BadLocationException e1) {
			e1.printStackTrace();
		}
		close();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.PopupDialog#getDialogSettings()
	 */
	@Override
	// override for visibility reasons
	//TODO: super call always returns null - maybe it's a good idea to redo this somehow
	protected IDialogSettings getDialogSettings() {
		return super.getDialogSettings();
	}
}


