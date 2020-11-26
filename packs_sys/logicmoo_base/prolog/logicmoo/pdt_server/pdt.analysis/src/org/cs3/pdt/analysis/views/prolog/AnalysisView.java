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
package org.cs3.pdt.analysis.views.prolog;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.analysis.AnalysisProperties;
import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IResultModel;
import org.cs3.pdt.analysis.model.prolog.PrologFactbase;
import org.cs3.pdt.analysis.model.prolog.PrologResultModel;
import org.cs3.pdt.analysis.views.AbstractAnalysisView;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ActivePrologProcessListener;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;

public class AnalysisView extends AbstractAnalysisView implements ActivePrologProcessListener {

	public static final String ID = "org.cs3.pdt.analysis.views.AnalysisView";
	
	private PrologResultModel resultModel = PrologResultModel.getInstance();
	
	private PrologFactbase currentFactbase;

	private ProcessSelector selector;
	
	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		PDTConnectorPlugin.getDefault().getPrologProcessService().registerActivePrologProcessListener(this);
		UIJob j = new UIJob("Initialize Prolog Analyses View") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				PrologProcess process = PDTCommonUtil.getActivePrologProcess();
				String factbaseName = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process);
				selector.setSelectedProcessName(factbaseName);
				processSelected(factbaseName);
				return Status.OK_STATUS;
			}
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.setDisplay(getViewSite().getShell().getDisplay());
		j.schedule();
	}
	
	@Override
	public void dispose() {
		PDTConnectorPlugin.getDefault().getPrologProcessService().unRegisterActivePrologProcessListener(this);
		super.dispose();
	}
	
	@Override
	protected IResultModel getResultModel() {
		return resultModel;
	}

	@Override
	protected void log(Exception e) {
		Debug.report(e);
	}

	@Override
	protected Control createSelector(Composite parent) {
		selector = new ProcessSelector(parent);
		selector.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				String selectedProcessName = selector.getSelectedProcessName();
				processSelected(selectedProcessName);

				PrologProcess process = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getPrologProcess(selectedProcessName);
				PDTConnectorPlugin.getDefault().getPrologProcessService().setActivePrologProcess(process);
				
				getAnalysisTreeViewer().getTree().setFocus();
			}
		});
		return selector;
	}
	
	private void processSelected(String processName) {
		getAnalysisContentProvider().setFactbase(processName);
		getResultContentProvider().setFactbase(processName);
		if (processName == null) {
			currentFactbase = null;
		} else {
			currentFactbase = resultModel.getFactbase(processName);
			refreshAnalysisEnablement();
		}
	}

	@Override
	protected void runEnabledAnalyses() {
		if (currentFactbase == null) {
			return;
		}
		final List<IAnalysis> enabledAnalyses = getEnabledAnalyses();
		Job j = new Job("Run enabled analyses") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				resultModel.updateAnalyses(currentFactbase, enabledAnalyses, getCurrentProcess(), monitor);
				return Status.OK_STATUS;
			}
		};
		j.schedule();
	}

	@Override
	protected void clearAllResults() {
		if (currentFactbase == null) {
			return;
		}
		Job j = new Job("Run all analysis results") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				resultModel.clearResults(currentFactbase);
				return Status.OK_STATUS;
			}
		};
		j.schedule();
	}

	@Override
	protected void runSelectedAnalyses() {
		if (currentFactbase == null) {
			return;
		}
		final List<IAnalysis> selectedAnalyses = getSelectedAnalyses(getAnalysisTreeViewer().getSelection());
		Job j = new Job("Run analyses") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				resultModel.updateAnalyses(currentFactbase, selectedAnalyses, getCurrentProcess(), monitor);
				return Status.OK_STATUS;
			}
		};
		j.schedule();
	}

	@Override
	protected void clearSelectedAnalyses() {
		if (currentFactbase == null) {
			return;
		}
		final List<IAnalysis> selectedAnalyses = getSelectedAnalyses(getAnalysisTreeViewer().getSelection());
		Job j = new Job("Clear analysis results") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				resultModel.clearResults(currentFactbase, selectedAnalyses);
				return Status.OK_STATUS;
			}
		};
		j.schedule();
	}
	
	@Override
	protected void analysisCheckStateChanged() {
		super.analysisCheckStateChanged();
		List<IAnalysis> enabledAnalyses = getEnabledAnalyses();
		ArrayList<String> enabledAnalysisNames = new ArrayList<>();
		for (IAnalysis analysis : enabledAnalyses) {
			enabledAnalysisNames.add(analysis.getName());
		}
		AnalysisProperties.setEnabledAnalyses(currentFactbase.getName(), enabledAnalysisNames);
	}
	
	private PrologProcess getCurrentProcess() {
		if (currentFactbase != null) {
			return PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getPrologProcess(currentFactbase.getName());
		}
		return null;
	}

	@Override
	protected void refreshAnalysisEnablement() {
		if (currentFactbase == null) {
			return;
		}
		Set<String> enabledAnalysisNames = AnalysisProperties.getEnabledAnalyses(currentFactbase.getName());
		List<IAnalysis> allAnalyses = currentFactbase.getAnalyses();
		ArrayList<IAnalysis> enabledAnalyses = new ArrayList<>();
		for (IAnalysis analysis : allAnalyses) {
			if (enabledAnalysisNames.contains(analysis.getName())) {
				enabledAnalyses.add(analysis);
			}
		}
		setEnabledAnalyses(enabledAnalyses);
	}

	@Override
	public void activePrologProcessChanged(final PrologProcess process) {
		Display display = getSite().getShell().getDisplay();
		display.asyncExec(new Runnable() {
			@Override
			public void run() {
				String factbaseName = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process);
				selector.setSelectedProcessName(factbaseName);
				processSelected(factbaseName);
			}
		});
	}

}
