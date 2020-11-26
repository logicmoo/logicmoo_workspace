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

import java.io.File;
import java.net.MalformedURLException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.pdt.graphicalviews.preferences.PredicateVisibilityPreferences;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;

public abstract class GraphProcessLoaderBase {

	protected File helpFile;
	protected PDTGraphView view;
	private PrologProcess process;
	//private ExecutorService executor = Executors.newCachedThreadPool();

	public GraphProcessLoaderBase(PDTGraphView view, String helpFileName) {
		this.view = view;
		helpFile = new File(System.getProperty("java.io.tmpdir"), helpFileName);
	}

	protected abstract String generateQuery(File helpFile);
	
	public Map<String, Object> loadGraph() {

		try {
			helpFile.delete();
			process = getActiveProcess();
			if (process != null) {
				String query = generateQuery(helpFile);
				Map<String, Object> output = sendQueryToCurrentProcess(query);
					
				// query =
				// "collect_ids_for_focus_file(FocusId,Files,CalledPredicates,Calls)";
				// Map<String, Object> result = sendQueryToCurrentProcess(query);
				// result.get("FocusId");

				new UIJob("Layouting") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							doLoadFile();
						} catch (MalformedURLException e) {
							Debug.rethrow(e);
						}
						return Status.OK_STATUS;
					}
				}.schedule();
				
				return output;
			}
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (PrologProcessException e) {
			e.printStackTrace();
		}
		return null;
	}

	protected String getSettings() {
		List<String> settings = new LinkedList<String>();

		if (!PredicateVisibilityPreferences.showPDTPredicates())
			settings.add("hide_pdt_predicates");

//		if (!PredicateVisibilityPreferences.showPDTMetapredicates())
//			settings.add("hide_pdt_metapredicates");

		if (!PredicateVisibilityPreferences.showSWIPredicates())
			settings.add("hide_swi_predicates");

//		if (!PredicateVisibilityPreferences.showSWIMetapredicates())
//			settings.add("hide_swi_metapredicates");

		if (settings.size() == 0)
			return "[]";

		StringBuilder sb = new StringBuilder(200);
		sb.append("[");

		for (int i = 0; i < settings.size() - 1; i++) {
			sb.append(settings.get(i));
			sb.append(", ");
		}
		sb.append(settings.get(settings.size() - 1));

		sb.append("]");
		return sb.toString();
	}

	protected void doLoadFile() throws MalformedURLException {
		view.loadGraph(helpFile.toURI().toURL());
	};
	
	public Map<String, Object> sendQueryToCurrentProcess(String query)
		throws PrologProcessException {
	
		PrologSession session = process.getSession(PrologProcess.DEFAULT);
		Map<String, Object> result = session.queryOnce(query);
		return result;
	}

	public PrologProcess getActiveProcess() {
		PrologProcess process = PDTCommonUtil.getActivePrologProcess();
		return process;
	}

	public abstract void setCurrentPath(String currentPath);

	public abstract String getCurrentPath();
}