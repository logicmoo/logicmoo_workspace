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

import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Display;

public class LocationProvider {

	private HashMap<PredicateEdge, List<Location>> cachedLocations = new HashMap<>();

	private int mode;

	private TableViewer locationTableViewer;
	private TreeViewer callTreeViewer;

	private IProject scope;

	public LocationProvider(TableViewer locationTableViewer, TreeViewer callTreeViewer) {
		this.locationTableViewer = locationTableViewer;
		this.callTreeViewer = callTreeViewer;
	}
	
	public void fillLocations(final PredicateEdge edge) {
		if (edge == null) {
			locationTableViewer.setInput(new ArrayList<Location>());
		} else {
			List<Location> locations;
			synchronized (cachedLocations) {
				locations = cachedLocations.get(edge);
			}
			if (locations != null) {
				locationTableViewer.setInput(locations);
			} else {
				final List<Location> newLocations = new ArrayList<>();
				Job j = getLocationFillJob(edge, newLocations);
				j.addJobChangeListener(new JobChangeAdapter() {
					@Override
					public void done(IJobChangeEvent event) {
						Display.getDefault().asyncExec(new Runnable() {
							@Override
							public void run() {
								PredicateEdge selectedPredicateEdge = getSelectedPredicateEdge();
								if (selectedPredicateEdge != null && selectedPredicateEdge.equals(edge)) {
									locationTableViewer.setInput(newLocations);
								}
							}
						});
					}
				});
				j.schedule();
			}
		}
	}

	public void selectFirstLocationInEditor(final PredicateEdge edge) {
		if (edge == null) {
			return;
		} 
		List<Location> locations;
		synchronized (cachedLocations) {
			locations = cachedLocations.get(edge);
		}
		if (locations != null) {
			if (locations.size() > 0) {
				CallHierarchyUtil.selectLocationInEditor(locations.get(0));
			}
			locationTableViewer.setInput(locations);
		} else {
			final ArrayList<Location> newLocations = new ArrayList<>();
			Job j = getLocationFillJob(edge, newLocations);
			j.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(IJobChangeEvent event) {
					Display.getDefault().asyncExec(new Runnable() {
						@Override
						public void run() {
							if (newLocations.size() > 0) {
								CallHierarchyUtil.selectLocationInEditor(newLocations.get(0));
							}
						}
					});
				}
			});
			j.schedule();
		}
	}
	
	private Job getLocationFillJob(final PredicateEdge edge, final List<Location> newLocations) {
		Job j = new Job("Search call locations") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (monitor.isCanceled()) {
					throw new OperationCanceledException();
				}
				synchronized (cachedLocations) {
					if (cachedLocations.containsKey(edge)) {
						return Status.OK_STATUS;
					}
				}
				PrologProcess process = PDTCommonUtil.getActivePrologProcess();
				try {
					Predicate from;
					Predicate to;
					if (mode == CallHierarchyView.CALLER_MODE) {
						from = edge.getTarget();
						to = edge.getSource();
					} else {
						from = edge.getSource();
						to = edge.getTarget();
					}
					List<Map<String, Object>> results = process.queryAll(bT(PDTCommonPredicates.FIND_CALL_LOCATION,
							quoteAtom(from.getModule()),
							quoteAtom(from.getName()),
							from.getArity(),
							quoteAtom(to.getModule()),
							quoteAtom(to.getName()),
							to.getArity(),
							scope != null ? QueryUtils.prologFileNameQuoted(scope.getLocation().toFile()) : "_",
									"File",
							"Position"));
					for (Map<String,Object> result : results) {
						if (monitor.isCanceled()) {
							throw new OperationCanceledException();
						}

						try {
							IFile file = FileUtils.findFileForLocation((String) result.get("File"));
							String position = (String) result.get("Position");
							int line;
							int start;
							int end;
							String text;
							IDocument doc = UIUtils.getDocument(file);
							if (position.indexOf("-") >= 0) {
								String[] positions = position.split("-");
								start = Integer.parseInt(positions[0]);
								end = Integer.parseInt(positions[1]);
								start = UIUtils.logicalToPhysicalOffset(doc, start);
								end = UIUtils.logicalToPhysicalOffset(doc, end);
								line = doc.getLineOfOffset(start);
								text = doc.get(start, end - start).replaceAll("\n|\r", "");
							} else {
								line = Integer.parseInt(position) - 1;
								start = doc.getLineOffset(line);
								end = start;
								text = edge.getTarget().getLabel();
							}
							Location location = new Location(text, file, start, end, line);
							newLocations.add(location);
						} catch (Exception e) {
							Debug.report(e);
						}
					}
					Collections.sort(newLocations);
					synchronized (cachedLocations) {
						cachedLocations.put(edge, newLocations);
					}
				} catch (Exception e) {
					Debug.report(e);
				}
				return Status.OK_STATUS;
			}
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		return j;
	}

	public void clearCache() {
		synchronized (cachedLocations) {
			cachedLocations.clear();
		}
	}

	public void setMode(int mode) {
		this.mode = mode;
	}
	
	public void setScope(IProject scope) {
		this.scope = scope;
	}
	
	private PredicateEdge getSelectedPredicateEdge() {
		ISelection selection = callTreeViewer.getSelection();
		if (!selection.isEmpty() && selection instanceof TreeSelection) {
			Object firstElement = ((TreeSelection) selection).getFirstElement();
			if (firstElement instanceof PredicateEdge) {
				return (PredicateEdge) firstElement;
			}
		}
		return null;
	}

}
