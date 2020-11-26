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
package org.cs3.pdt.analysis.model.prolog;

import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.analysis.AnalysisPredicates;
import org.cs3.pdt.analysis.PDTAnalysis;
import org.cs3.pdt.analysis.model.AbstractResultModel;
import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IFactbase;
import org.cs3.pdt.analysis.model.IResult;
import org.cs3.pdt.analysis.model.IResultElement;
import org.cs3.pdt.analysis.model.Result;
import org.cs3.pdt.analysis.model.ResultList;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PrologProcessStartListener;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PrologResultModel extends AbstractResultModel implements ConsultListener, PrologProcessStartListener {

	private static PrologResultModel instance;
	
	public static PrologResultModel getInstance() {
		if (instance == null) {
			instance = new PrologResultModel();
		}
		return instance;
	}
	
	private PrologResultModel() {
		PDTConnectorPlugin.getDefault().getPrologProcessService().registerConsultListener(this);
		PDTCommonPlugin.getDefault().registerProcessStartListener(this);
		initializeFromMarkers();
	}
	
	private HashMap<String, ResultList> results = new HashMap<>();
	
	private HashMap<String, PrologFactbase> factbases = new HashMap<>();
	
	@Override
	public PrologFactbase getFactbase(String name) {
		PrologFactbase factbase = factbases.get(name);
		if (factbase == null) {
			factbase = new PrologFactbase(name, this);
			factbases.put(name, factbase);
		}
		return factbase;
	}

	@Override
	public List<IFactbase> getFactbases() {
		return new ArrayList<IFactbase>(factbases.values());
	}

	public List<IResultElement> getResults(IAnalysis analysis) {
		ResultList resultList = getResultList(analysis.getName());
		synchronized (resultList) {
			return resultList.getResults();
		}
	}

	private ResultList getResultList(String name) {
		ResultList resultList;
		synchronized (results) {
			resultList = results.get(name);
			if (resultList == null) {
				resultList = new ResultList(name);
				results.put(name, resultList);
			}
		}
		return resultList;
	}

	public int getNumberOfResults(IAnalysis analysis) {
		ResultList resultList = getResultList(analysis.getName());
		synchronized (resultList) {
			return resultList.getNumberOfResults();
		}
	}

	public void clearResults(PrologFactbase factbase) {
		synchronized (results) {
			Set<String> analysisNames = results.keySet();
			for (String analysisName : analysisNames) {
				ResultList resultList = getResultList(analysisName);
				synchronized (resultList) {
					for (IResultElement resultElement : resultList.getResults()) {
						deleteResultMarkers(resultElement);
					}
					resultList.clearResults();
				}
			}
		}
		fireResultsUpdated(factbase, factbase.getAnalyses());
	}

	public void clearResults(PrologFactbase factbase, IAnalysis analysis) {
		doClearResults(analysis);
		ArrayList<IAnalysis> analyses = new ArrayList<>();
		analyses.add(analysis);
		fireResultsUpdated(factbase, analyses);
	}
	
	public void clearResults(PrologFactbase factbase, List<IAnalysis> analyses) {
		for (IAnalysis analysis : analyses) {
			doClearResults(analysis);
		}
		fireResultsUpdated(factbase, analyses);
	}
	
	private void doClearResults(IAnalysis analysis) {
		ResultList resultList = getResultList(analysis.getName());
		synchronized (resultList) {
			for (IResultElement resultElement : resultList.getResults()) {
				deleteResultMarkers(resultElement);
			}
			resultList.clearResults();
		}
	}
	
	private void deleteResultMarkers(IResultElement element) {
		if (element instanceof IResult) {
			IMarker marker = ((IResult) element).getMarker();
			try {
				marker.delete();
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		if (element.hasChildren()) {
			for (IResultElement child : element.getChildren()) {
				deleteResultMarkers(child);
			}
		}
	}

	@Override
	public void beforeConsult(PrologProcess process, List<IFile> files, IProgressMonitor monitor) throws PrologProcessException {
	}

	@Override
	public void afterConsult(PrologProcess process, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologProcessException {
		String factbaseName = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process);
		PrologFactbase factbase = getFactbase(factbaseName);
		
		factbase.collectAnalyses(process);
		fireAnalysesUpdated(factbase);
		
		List<IAnalysis> enabledAnalyses = factbase.getEnabledAnalyses();
		if (!enabledAnalyses.isEmpty()) {
			updateAnalyses(factbase, enabledAnalyses, process, monitor);
			fireResultsUpdated(factbase, enabledAnalyses);
		}
		
		monitor.done();
	}

	public void updateAnalyses(PrologFactbase factbase, List<IAnalysis> analyses, PrologProcess process, IProgressMonitor monitor) {
		try {
			monitor.beginTask("Run Prolog analyses", analyses.size() * 3);
			for (IAnalysis analysis : analyses) {
				String analysisName = analysis.getName();
				monitor.subTask(" : " + analysisName + " : Clearing results");
				doClearResults(analysis);
				monitor.worked(1);
				
				monitor.subTask(" : " + analysisName + " : Running query");
				List<Map<String, Object>> results = process.queryAll(bT(AnalysisPredicates.ANALYSIS_MARKER,
						quoteAtom(analysisName),
						"Severity",
						"File",
						"Location",
						"Description"));
				monitor.worked(1);
				
				monitor.subTask(" : " + analysisName + " : Processing results");
				SubProgressMonitor subMonitor = new SubProgressMonitor(monitor, 1);
				subMonitor.beginTask("Processing results", results.size());
				ResultList resultList = getResultList(analysisName);
				synchronized (resultList) {
					for (Map<String, Object> result : results) {
						try {
							IFile file = FileUtils.findFileForLocation((String) result.get("File"));
							String description = (String) result.get("Description");
							String severity = (String) result.get("Severity");
							Result r = new Result(analysisName, description, severity, file, null);
							
							IMarker marker = file.createMarker(PDTAnalysis.MARKER_TYPE);
							marker.setAttribute(IMarker.SEVERITY, PDTAnalysis.getMarkerSeverity(severity));
							marker.setAttribute(PDTAnalysis.ANALYSIS_NAME_ATTRIBUTE, analysisName);
							marker.setAttribute(IMarker.MESSAGE, description);
							
							String location = (String) result.get("Location");
							int line;
							int start;
							int end;
							IDocument doc = UIUtils.getDocument(file);
							if (location.indexOf("-") >= 0) {
								String[] positions = location.split("-");
								start = Integer.parseInt(positions[0]);
								end = Integer.parseInt(positions[1]);
								start = UIUtils.logicalToPhysicalOffset(doc, start);
								end = UIUtils.logicalToPhysicalOffset(doc, end);
								line = doc.getLineOfOffset(start);
							} else {
								line = Integer.parseInt(location) - 1;
								start = doc.getLineOffset(line);
								end = start;
							}
							MarkerUtilities.setCharStart(marker, start);
							MarkerUtilities.setCharEnd(marker, end);
							MarkerUtilities.setLineNumber(marker, line);
							r.setMarker(marker);
							resultList.addResult(r);
						} catch (Exception e){
							Debug.report(e);
						}
						subMonitor.worked(1);
					}
				}
				subMonitor.done();
			}
		} catch (Exception e) {
			Debug.report(e);
		}
		fireResultsUpdated(factbase, analyses);
	}

	private void initializeFromMarkers() {
		Job j = new Job("Initialize Prolog Result Model") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				synchronized (results) {
					try {
						IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDTAnalysis.MARKER_TYPE, false, IResource.DEPTH_INFINITE);
						for (IMarker marker : markers) {
							String analysisName = marker.getAttribute(PDTAnalysis.ANALYSIS_NAME_ATTRIBUTE, null);
							String description = marker.getAttribute(IMarker.MESSAGE, null);
							int severityValue = marker.getAttribute(IMarker.SEVERITY, -1);
							IResource resource = marker.getResource();
							if (analysisName == null || description == null || severityValue == -1 || !(resource instanceof IFile)) {
								continue;
							}
							ResultList resultList = getResultList(analysisName);
							Result result = new Result(analysisName, description, PDTAnalysis.getSeverityText(severityValue), (IFile) resource, null);
							result.setMarker(marker);
							resultList.addResult(result);
						}
					} catch (Exception e) {
						Debug.report(e);
					}
				}
				return Status.OK_STATUS;
			}
		};
		j.schedule();
	}

	@Override
	public void prologProcessStarted(PrologProcess process) {
		String factbaseName = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process);
		PrologFactbase factbase = getFactbase(factbaseName);
		
		factbase.collectAnalyses(process);
		fireAnalysesUpdated(factbase);
	}

}
