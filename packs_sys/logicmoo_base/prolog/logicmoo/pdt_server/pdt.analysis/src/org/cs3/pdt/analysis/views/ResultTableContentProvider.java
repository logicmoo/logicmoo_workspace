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
import java.util.Collections;
import java.util.List;

import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IFactbase;
import org.cs3.pdt.analysis.model.IResultElement;
import org.cs3.pdt.analysis.model.IResultModel;
import org.cs3.pdt.analysis.model.IResultModelListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class ResultTableContentProvider implements ITreeContentProvider, IResultModelListener {
	
	private static final Object[] EMPTY_ARRAY = new Object[0];
	
	private IResultModel resultModel;
	
	private ArrayList<IResultElement> results = new ArrayList<>();

	private Viewer viewer;

	private IFactbase currentFactbase;
	private List<IAnalysis> currentAnalyses = new ArrayList<>();

	public ResultTableContentProvider(IResultModel resultModel) {
		this.resultModel = resultModel;
		resultModel.addListener(this);
	}
	
	public void setFactbase(String factbaseName) {
		currentFactbase = resultModel.getFactbase(factbaseName);
		currentAnalyses.clear();
		viewer.refresh();
	}
	
	public void setSelectedAnalyses(List<IAnalysis> analyses) {
		currentAnalyses.clear();
		currentAnalyses.addAll(analyses);
		fillResults();
		viewer.refresh();
	}

	private void fillResults() {
		synchronized (results) {
			results.clear();
			if (currentFactbase == null) {
				return;
			}
			for (IAnalysis analysis : currentAnalyses) {
				results.addAll(currentFactbase.getResults(analysis));
			}
			Collections.sort(results);
		}
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
		synchronized (results) {
			return results.toArray();
		}
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof IResultElement) {
			List<IResultElement> children = ((IResultElement) parentElement).getChildren();
			Collections.sort(children);
			return children.toArray();
		}
		return EMPTY_ARRAY;
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof IResultElement) {
			return ((IResultElement) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof IResultElement) {
			return ((IResultElement) element).hasChildren();
		}
		return false;
	}

	@Override
	public void analysesUpdated(IFactbase factbase) {
		if (factbase.equals(currentFactbase)) {
			fillResults();
			viewer.getControl().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					viewer.refresh();
				}
			});
		}
	}

	@Override
	public void resultsUpdated(IFactbase factbase, List<IAnalysis> analyses) {
		if (factbase.equals(currentFactbase)) {
			boolean needRefresh = false;
			for (IAnalysis analysis : analyses) {
				if (currentAnalyses.contains(analysis)) {
					needRefresh = true;
					break;
				}
			}
			if (needRefresh) {
				fillResults();
				viewer.getControl().getDisplay().asyncExec(new Runnable() {
					@Override
					public void run() {
						viewer.refresh();
					}
				});
			}
		}
	}

}
