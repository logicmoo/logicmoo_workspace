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

import java.util.Collections;
import java.util.List;

import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IAnalysisCategory;
import org.cs3.pdt.analysis.model.IFactbase;
import org.cs3.pdt.analysis.model.IResultModel;
import org.cs3.pdt.analysis.model.IResultModelListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class AnalysisTableContentProvider implements ITreeContentProvider, IResultModelListener {
	
	private static final Object[] EMPTY_ARRAY = new Object[0];
	
	private IResultModel resultModel;
	
	private Viewer viewer;

	private AbstractAnalysisView view;
	
	private IFactbase currentFactbase;

	public AnalysisTableContentProvider(IResultModel resultModel) {
		this.resultModel = resultModel;
		resultModel.addListener(this);
	}
	
	public void setFactbase(String factbaseName) {
		if (factbaseName == null) {
			currentFactbase = null;
		} else {
			currentFactbase = resultModel.getFactbase(factbaseName);
		}
		viewer.refresh();
	}
	
	public void setView(AbstractAnalysisView view) {
		this.view = view;
	}

	@Override
	public void dispose() {
		resultModel.removeListener(this);
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		this.viewer = viewer;
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (currentFactbase == null) {
			return EMPTY_ARRAY;
		}
		List<IAnalysisCategory> categories = currentFactbase.getCategories();
		Collections.sort(categories);
		return categories.toArray();
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof IAnalysisCategory) {
			IAnalysisCategory category = (IAnalysisCategory) parentElement;
			List<IAnalysis> analyses = category.getAnalyses();
			Collections.sort(analyses);
			return analyses.toArray();
		}
		return EMPTY_ARRAY;
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof IAnalysis) {
			return ((IAnalysis) element).getCategory();
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof IAnalysisCategory) {
			IAnalysisCategory category = (IAnalysisCategory) element;
			return !category.getAnalyses().isEmpty();
		}
		return false;
	}

	@Override
	public void analysesUpdated(IFactbase factbase) {
		if (factbase.equals(currentFactbase)) {
			viewer.getControl().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					viewer.refresh();
					view.refreshAnalysisEnablement();
				}
			});
		}
	}

	@Override
	public void resultsUpdated(IFactbase factbase, List<IAnalysis> analyses) {
		if (factbase.equals(currentFactbase)) {
			viewer.getControl().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					viewer.refresh();
				}
			});
		}
	}

}
