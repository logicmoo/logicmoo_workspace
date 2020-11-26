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

package org.cs3.pdt.editor.internal.editors;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.connector.PrologConnectorPredicates;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDTPredicates;
import org.cs3.pdt.editor.quickfix.PDTMarker;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PLMarkerUtils {

	private static int mapSeverity(String severity) {
		if ("error".equals(severity)) {
			return IMarker.SEVERITY_ERROR;
		}
		if ("warning".equals(severity)) {
			return IMarker.SEVERITY_WARNING;
		}
		if ("info".equals(severity)) {
			return IMarker.SEVERITY_INFO;
		}

		throw new IllegalArgumentException("cannot map severity constant: "
				+ severity);
	}

	public static void addMarkers(PrologProcess process, List<String> allConsultedFiles, IProgressMonitor monitor) {
		monitor.beginTask("Update markers", 2);
		PrologSession session =null;
		try {
			session = process.getSession();
			Map<String, IFile> fileNameToIFiles = new HashMap<String, IFile>();
			monitor.subTask("add markers for errors and warnings");
			collectIFilesForFileNames(allConsultedFiles, fileNameToIFiles);
			addMarkersForErrorsAndWarnings(session, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK), fileNameToIFiles);
			monitor.subTask("Update Prolog Smells Detectors");
			addMarkersForSmellDetectors(session, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK), fileNameToIFiles);
//			addMarkersUndefinedCalls(session, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK), fileNameToIFiles);
//			session.queryOnce("deactivate_warning_and_error_tracing");
		} catch (PrologException e) {
			// this may be a reload_timeout_reached exception
			// (shouldn't happen anymore, but maybe it does)

			// so at least we deactivate the tracing, because
			// otherwise error markers will still be visible after removing the error
//			try {
//				session.queryOnce("deactivate_warning_and_error_tracing");
//			} catch (Exception e1) {
//				Debug.report(e1);
//			}
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null) {
				session.dispose();
			}
			monitor.done();
		}
	}
	
	private static void addMarkersForErrorsAndWarnings(PrologSession session, SubProgressMonitor monitor, Map<String, IFile> fileNameToIFiles) throws PrologProcessException, CoreException {
		List<Map<String, Object>> msgs = session.queryAll(bT(PrologConnectorPredicates.ERRORS_AND_WARNINGS, "Kind", "Line", "Length", "Message", "File"));
		monitor.beginTask("Add markers for errors and warnings", msgs.size());
		
		for (Map<String, Object> msg : msgs) {
			int severity=0;
			try {
				severity = mapSeverity(((String)msg.get("Kind")));
			} catch(IllegalArgumentException e){
				monitor.worked(1);
				continue;
			}
			
			String fileName = msg.get("File").toString();
			IFile file = getFile(fileName, fileNameToIFiles);
			if (file == null) {
				monitor.worked(1);
				continue;
			}
			
			IMarker marker = file.createMarker(IMarker.PROBLEM);

			marker.setAttribute(IMarker.SEVERITY, severity);

			String msgText = msg.get("Message").toString();
			int line = Integer.parseInt(msg.get("Line").toString());
			if (severity == IMarker.SEVERITY_ERROR && msgText.startsWith("Exported procedure ") && msgText.endsWith(" is not defined\n")){
				line = 1;
			}
			
			MarkerUtilities.setLineNumber(marker, line);
			
			marker.setAttribute(IMarker.MESSAGE, msgText);
			monitor.worked(1);
		}
		monitor.done();
	}
	
	private static void addMarkersForSmellDetectors(PrologSession session, IProgressMonitor monitor, Map<String, IFile> fileNameToIFiles) throws PrologProcessException, CoreException {
		monitor.beginTask("Update Prolog Smells Detectors", fileNameToIFiles.size());

		for (String fileName : fileNameToIFiles.keySet()) {
			String query = bT(PDTPredicates.SMELL_MARKER_PDT, "Name", "Description", "QuickfixDescription", "QuickfixAction", QueryUtils.quoteAtom(fileName), "Start", "Length");
			List<Map<String, Object>> msgsSmells = session.queryAll(query);
			
			if(msgsSmells!=null) {
				IFile file = getFile(fileName, fileNameToIFiles);
				IDocument doc = UIUtils.getDocument(file);
				for (Map<String, Object> msg : msgsSmells) {
					IMarker marker = file.createMarker(IMarker.PROBLEM);
					marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
					marker.setAttribute(PDTMarker.SMELL_NAME, msg.get("Name").toString());
					marker.setAttribute(PDTMarker.QUICKFIX_DESCRIPTION, msg.get("QuickfixDescription").toString());
					
					String msgText = (String)msg.get("Description");
					int startPl = Integer.parseInt(msg.get("Start").toString());
					int start = UIUtils.logicalToPhysicalOffset(doc,startPl);
					int length = Integer.parseInt(msg.get("Length").toString());
					
					MarkerUtilities.setCharStart(marker, start);
					MarkerUtilities.setCharEnd(marker, start+length);
					
					marker.setAttribute(PDTMarker.QUICKFIX_ACTION, msg.get("QuickfixAction".toString()));
					marker.setAttribute(IMarker.MESSAGE, msgText);
				}
			}
			monitor.worked(1);
		}
		monitor.done();
	}
	
//	private static void addMarkersUndefinedCalls(PrologSession session, SubProgressMonitor monitor, Map<String, IFile> fileNameToIFiles) throws PrologException, PrologProcessException {
//		monitor.beginTask("Add markers for undefined calls", 1);
//		boolean doUndefinedCallAnalysis = PDTPlugin.getDefault().getPreferenceStore().getBoolean(PDT.PREF_UNDEFINED_CALL_ANALYSIS);
//		if (doUndefinedCallAnalysis) {
//			List<Map<String, Object>> results = session.queryAll(bT(PDTCommonPredicates.FIND_UNDEFINED_CALL, "Goal", "File", "Start", "End"));
//			for (Map<String, Object> result : results) {
//				try {
//					IFile file = getFile(result.get("File").toString(), fileNameToIFiles);
//					if (file == null) {
//						continue;
//					}
//					IDocument doc = UIUtils.getDocument(file);
//					int start = UIUtils.logicalToPhysicalOffset(doc, Integer.parseInt(result.get("Start").toString()));
//					int end = UIUtils.logicalToPhysicalOffset(doc, Integer.parseInt(result.get("End").toString()));
//					IMarker marker = file.createMarker(IMarker.PROBLEM);
//					marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
//					MarkerUtilities.setCharStart(marker, start);
//					MarkerUtilities.setCharEnd(marker, end);
//					marker.setAttribute(IMarker.MESSAGE, "Undefined call: " + result.get("Goal").toString());
//				} catch (Exception e) {
//					Debug.report(e);
//					continue;
//				}
//			}
//		}
//		monitor.done();
//	}

	private static void collectIFilesForFileNames(List<String> fileNames, Map<String, IFile> fileNameToIFiles) {
		for (String fileName : fileNames) {
			try {
				getFile(fileName, fileNameToIFiles);
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
	}
	
	private static IFile getFile(String fileName, Map<String, IFile> fileNameToIFiles) throws CoreException {
		IFile file = fileNameToIFiles.get(fileName);
		if (file != null) {
			return file;
		} else {
			try {
				file = FileUtils.findFileForLocation(fileName);
			} catch (IOException e1) {
				return null;
			} catch (IllegalArgumentException e2){
				return null;
			}
			if (file == null || !file.exists()){
				return null;
			}
			file.deleteMarkers(IMarker.PROBLEM, false, IResource.DEPTH_INFINITE);
			fileNameToIFiles.put(fileName, file);
			return file;
		}
	}


}


