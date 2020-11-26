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

package org.cs3.pdt.editor.internal.editors.breakpoints;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.ProcessReconsulterListener;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ActivePrologProcessListener;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDTPredicates;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologEventDispatcher;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologEvent;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.process.PrologEventListener;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PDTBreakpointHandler implements PrologEventListener, LifeCycleHook, ActivePrologProcessListener, ConsultListener, ProcessReconsulterListener {

	private static final String ADD_BREAKPOINT = "add_breakpoint";
	private static final String REMOVE_BREAKPOINT = "remove_breakpoint";
	private static final String BREAKPOINT_LIFECYCLE_HOOK = "BreakpointLifecycleHook";
	private static final String[] DEPENDENCIES = {PDTCommonPlugin.LIFE_CYCLE_HOOK_ID};
	private static final String SOURCE_FILE = "source_file";
	private static final String DELETE_BREAKPOINT = "delete_breakpoint";
	private static final String BREAKPOINT_PROPERTY = "pdt_editor_breakpoints:pdt_breakpoint_property";

	private static final String PDT_BREAKPOINT_MARKER = "org.cs3.pdt.editor.PDTBreakpointMarker";
	private static final String BREAKPOINT_ID = "pdt.breakpoint.id";
	private static final String BREAKPOINT_OFFSET = "pdt.breakpoint.offset";
	private static final String BREAKPOINT_OFFSET_PHYSICAL = "pdt.breakpoint.offset.physical";

	private static PDTBreakpointHandler instance;

	private PrologProcess currentProcess = null;
	private PrologEventDispatcher currentDispatcher;
	private Set<String> deletedIds;

	public static PDTBreakpointHandler getInstance() {
		if (instance == null)
			instance = new PDTBreakpointHandler();
		return instance;
	}

	private PDTBreakpointHandler() {
		PDTConnectorPlugin.getDefault().getPrologProcessService().registerActivePrologProcessListener(this);
		PDTConnectorPlugin.getDefault().getPrologProcessService().registerConsultListener(this);
		PDTCommonPlugin.getDefault().getProcessReconsulter().registerListener(this);
	}

	private void checkForProcess() {
		if (currentProcess == null) {
			currentProcess = PDTCommonUtil.getActivePrologProcess();
			addProcessListener();
		}
	}


	List<MarkerBackup> markerBackup = null;

	public void backupMarkers() {
		markerBackup = new ArrayList<MarkerBackup>();

		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				int line = marker.getAttribute(IMarker.LINE_NUMBER, 0);
				int currentOffset = marker.getAttribute(IMarker.CHAR_START, -1);
				int offsetWhenSetting = marker.getAttribute(BREAKPOINT_OFFSET_PHYSICAL, -1);
				int logicOffset = marker.getAttribute(BREAKPOINT_OFFSET, -1);
				String id = marker.getAttribute(BREAKPOINT_ID, "");
				if (currentOffset == -1 || offsetWhenSetting == -1 || currentOffset != offsetWhenSetting) {
					markerBackup.add(new MarkerBackup(marker.getResource(), line, id, -1));
				} else {
					markerBackup.add(new MarkerBackup(marker.getResource(), line, id, logicOffset));	
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}

		// enable logging of deleted ids
		deletedIds = new HashSet<String>();
	}

	public boolean shouldUpdateMarkers = true;
	
	public void updateMarkers() {
			if (!shouldUpdateMarkers || markerBackup == null || deletedIds == null) {
				return;
			}
			
			for (final MarkerBackup m : markerBackup) {
				
				runAsJob("recreate marker", m.getFile(), new Runnable() {
					
					@Override
					public void run() {
						// only recreate the marker if it was deleted
						if (deletedIds.contains(m.getId())) {
							int offset = m.getOffset();
							try {
								if (offset == -1) {
									IDocument document2;
									document2 = UIUtils.getDocument(m.getFile());
									offset = UIUtils.physicalToLogicalOffset(document2, document2.getLineInformation(m.getLineNumber() - 1).getOffset());
								}
								try {
									currentProcess.queryOnce(bT(PDTPredicates.PDT_SET_BREAKPOINT, FileUtils.prologFileNameQuoted(m.getFile()), m.getLineNumber(), offset, "Id"));
								} catch (PrologProcessException e) {
									Debug.report(e);
								}
							} catch (CoreException e) {
								Debug.report(e);
							} catch (BadLocationException e) {
								Debug.report(e);
							}
						}						
					}
				});
				
				
			}

			runAsJob("recreate marker", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {
				
				@Override
				public void run() {
					// disable logging of deleted ids
					deletedIds = null;
					markerBackup = null;
				}
			});
	}

	private IMarker getBreakpointAtLine(IFile file, int line) {
		// used to check if there is an existing breakpoint in this line
		// used by toggleBreakpoint to see if the marker has to be added or removed
		if (file != null) {
			try {
				IMarker[] markers = file.findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
				for (IMarker marker : markers) {
					if (marker.getAttribute(IMarker.LINE_NUMBER, 0) == line)
						return marker;
				}
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return null;
	}

	private void addMarker(String fileName, int line, int offset, int length, String id) throws CoreException {
		// called when a breakpoint was added in prolog
		try {
			IFile file = FileUtils.findFileForLocation(fileName);
			addMarker(file, line, offset, length, id);
		} catch (IOException e) {
			Debug.report(e);
		} catch (IllegalArgumentException e) {
			if (e.getMessage().startsWith("Not in Workspace: ")) {
				Debug.warning("Try to set breakpoint marker in non-workspace file");
			} else {
				Debug.report(e);
			}
		}
	}

	private void addMarker(final IFile file, final int line, final int offset, final int length, final String id) throws CoreException {
		runAsJob("Add breakpoint", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {

			@Override
			public void run() {
				HashMap<String, Object> attributes = new HashMap<String, Object>();
				attributes.put(IMarker.LINE_NUMBER, line);
				attributes.put(IMarker.MESSAGE, "Prolog Breakpoint: line[" + line + "]");
				attributes.put(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
				attributes.put(BREAKPOINT_ID, id);
				attributes.put(BREAKPOINT_OFFSET, offset);
				try {
					IDocument document2 = UIUtils.getDocument(file);
					int start = UIUtils.logicalToPhysicalOffset(document2, offset);
					int end = UIUtils.logicalToPhysicalOffset(document2, offset + length);
					attributes.put(BREAKPOINT_OFFSET_PHYSICAL, start);
					attributes.put(IMarker.CHAR_START, start);
					attributes.put(IMarker.CHAR_END, end);
				} catch (CoreException e) {
					Debug.report(e);
				}
				try {
					MarkerUtilities.createMarker(file, attributes, PDT_BREAKPOINT_MARKER);
				} catch (CoreException e) {
					Debug.report(e);
				}
			}
		});
	}

	private void removeAllBreakpointMarkers() {
		runAsJob("Remove all breakpoints", ResourcesPlugin.getWorkspace().getRoot(), new Runnable() {
			
			@Override
			public void run() {
				// called when active prolog interface was changed
				try {
					ResourcesPlugin.getWorkspace().getRoot().deleteMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
					//			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
					//			for (IMarker marker : markers) {
					//				marker.delete();
					//			}
				} catch (CoreException e) {
					Debug.report(e);
				}
				
			}
		});
	}
	
	private void runAsJob(String name, ISchedulingRule rule, final Runnable runnable) {
		Job job = new Job(name) {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				runnable.run();
				return Status.OK_STATUS;
			}
		};
		job.setRule(rule);
		job.schedule();
	}

	public void removeBreakpointFactsForFile(String prologFileName) {
		checkForProcess();
		try {
			List<Map<String, Object>> results = currentProcess.queryAll(bT(BREAKPOINT_PROPERTY, "Id" , "file('" + prologFileName + "')"));
			for (Map<String, Object> r : results) {
				currentProcess.queryOnce(bT(DELETE_BREAKPOINT, r.get("Id")));
			}
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
	}

	public void toogleBreakpoint(IFile file, int line, int offset) {
		if (file == null) {
			UIUtils.displayErrorDialog(Display.getCurrent().getActiveShell(), "File is not in workspace", "You can only set breakpoint markers to files, which are in your workspace.");
			return;
		}
		checkForProcess();
		String prologFileName;
		prologFileName = FileUtils.prologFileNameQuoted(file);
		if (line > 0) {
			IMarker existingMarker = getBreakpointAtLine(file, line);
			if (existingMarker == null) {
				// add marker
				if (file != null) {
					try {
						boolean isSourceFile = (currentProcess.queryOnce(bT(SOURCE_FILE, prologFileName)) != null);
						if (isSourceFile) {
							executeSetBreakpointQuery(prologFileName, line, offset);
						} else {
							UIUtils.displayErrorDialog(Display.getCurrent().getActiveShell(), "File is not loaded", "You are trying to set a breakpoint to a file which is not loaded. You have to consult the file before you can set a breakpoint.");
						}
					} catch (PrologProcessException e) {
						Debug.report(e);
					}
				}
			} else {
				// remove marker
				try {
					String id = existingMarker.getAttribute(BREAKPOINT_ID, "");
					if (!id.isEmpty()) {
						currentProcess.queryOnce(bT(BREAKPOINT_PROPERTY, id, "file(_)"),
								bT(DELETE_BREAKPOINT, id));
					}
					// if for some strange reason the marker is still there, even if there is no
					// breakpoint in prolog, we have to delete the marker here manually
					existingMarker.delete();
				} catch (CoreException e) {
					Debug.report(e);				
				} catch (PrologProcessException e) {
					Debug.report(e);				
				}
			}
		}		
	}

	public void executeSetBreakpointQuery(String prologFileName, int line, int offset) throws PrologProcessException {
		Debug.debug("Set breakpoint in file " + prologFileName + " (line: " + line + ", offset: " + offset + ")");
		String query = bT(PDTPredicates.PDT_SET_BREAKPOINT, prologFileName, line, offset, "_");
		PrologProcess process = PDTCommonUtil.getActivePrologProcess();
		process.queryOnce(query);
	}

	private void loadBreakpointsFromProcess() {
		List<Map<String, Object>> results;
		try {
			results = currentProcess.queryAll(bT(BREAKPOINT_PROPERTY, "Id" , "file(File)"),
					bT(BREAKPOINT_PROPERTY, "Id" , "line_count(Line)"),
					bT(BREAKPOINT_PROPERTY, "Id" , "character_range(Offset, Length)"));
			for (Map<String, Object> result : results) {
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				String id = result.get("Id").toString();
				try {
					addMarker(result.get("File").toString(), line, offset, length, id);
				} catch (IllegalArgumentException e) {
					Debug.report(e);
				}
			}
		} catch (PrologProcessException e) {
			Debug.report(e);
		} catch (CoreException e) {
			Debug.report(e);
		}


	}

	private void addProcessListener() {
		if (currentProcess != null) {
			Debug.debug("add listener for process " + currentProcess.toString());

			currentDispatcher = new PrologEventDispatcher(currentProcess);
			currentProcess.addLifeCycleHook(this, BREAKPOINT_LIFECYCLE_HOOK, DEPENDENCIES);
			try {
				currentDispatcher.addPrologEventListener(ADD_BREAKPOINT, this);
				currentDispatcher.addPrologEventListener(REMOVE_BREAKPOINT, this);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}

		}
	}

	private void removeProcessListener() {
		if (currentProcess != null && currentDispatcher != null) {
			Debug.debug("remove listener for process " + currentProcess.toString());
			currentProcess.removeLifeCycleHook(BREAKPOINT_LIFECYCLE_HOOK);
			try {
				currentDispatcher.removePrologEventListener(ADD_BREAKPOINT, this);
				currentDispatcher.removePrologEventListener(REMOVE_BREAKPOINT, this);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
		}
	}

	@Override
	public void update(PrologEvent e) {
		if (e.getSubject().equals(ADD_BREAKPOINT)) {
			String id = e.getData();
			try {
				Map<String, Object> result = currentProcess.queryOnce(bT(PDTPredicates.PDT_BREAKPOINT_PROPERTIES, id, "File", "Line", "Offset", "Length"));
				String file = result.get("File").toString();
				int line = Integer.parseInt(result.get("Line").toString());
				int offset = Integer.parseInt(result.get("Offset").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				addMarker(file, line, offset, length, id);
			} catch (PrologProcessException e1) {
				Debug.report(e1);
			} catch (CoreException e1) {
				Debug.report(e1);
			}
		} else if (e.getSubject().equals(REMOVE_BREAKPOINT)) {
			String id = e.getData();
			removeMarkerWithId(id);
		}
	}

	private void removeMarkerWithId(final String id) {
		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(PDT_BREAKPOINT_MARKER, true, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				if (marker.getAttribute(BREAKPOINT_ID, "").equals(id)) {
					final IMarker m = marker;
					runAsJob("Delete marker", marker.getResource(), new Runnable() {

						@Override
						public void run() {
							try {
								m.delete();
							} catch (CoreException e) {
								Debug.report(e);
							}
							Debug.debug("remove marker " + id);
							if (deletedIds != null) {
								deletedIds.add(id);
							}
						}
					});
					return;
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	@Override
	public void onInit(PrologProcess process, PrologSession initSession) throws PrologProcessException {
	}

	@Override
	public void afterInit(final PrologProcess process) throws PrologProcessException {
	}

	@Override
	public void beforeShutdown(PrologProcess process, PrologSession session) throws PrologProcessException {
		if (currentProcess.equals(process)) {
			shouldUpdateMarkers = false;
			backupMarkers();
			removeAllBreakpointMarkers();
		}
	}

	@Override
	public void onError(PrologProcess process) {
		if (currentProcess.equals(process)) {
			shouldUpdateMarkers = false;
			backupMarkers();
		}
	}

	@Override
	public void setData(Object data) {}

	@Override
	public void lateInit(PrologProcess process) {}

	private void waitForDispatcherSubjectActive() {
		PrologEventDispatcher dispatcher = currentDispatcher;
		for (int i = 0; i < 10; i++) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
			}
			if (dispatcher.getSubjects().contains(ADD_BREAKPOINT)) {
				return;
			}
		}
	}

	@Override
	public void activePrologProcessChanged(PrologProcess process) {
		if (currentProcess == process) {
			return;
		}
		
		removeProcessListener();

		currentProcess = process;
		removeAllBreakpointMarkers();
		loadBreakpointsFromProcess();

		addProcessListener();
	}

	@Override
	public void beforeConsult(PrologProcess process, List<IFile> files, IProgressMonitor monitor) throws PrologProcessException {
		if (process.equals(currentProcess) && markerBackup == null) {
			backupMarkers();
		}
	}

	@Override
	public void afterConsult(PrologProcess process, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologProcessException {
		if (process.equals(currentProcess)) {
			Debug.debug("update marker");
			updateMarkers();
		}
		monitor.done();
	}

	@Override
	public void afterReconsult(final PrologProcess process, String reconsultBehaviour, List<IFile> reconsultedFiles) {
		if (!process.equals(currentProcess)) {
			return;
		}
		shouldUpdateMarkers = true;
		if (markerBackup == null || markerBackup.isEmpty()) {
			markerBackup = null;
			return;
		}
		Runnable r = new Runnable() {
			@Override
			public void run() {
				waitForDispatcherSubjectActive();
				StringBuffer buf = new StringBuffer();
				boolean first = true;
				for (MarkerBackup m : markerBackup) {
					// TODO: Debug here, timing issues
					if (first) {
						first = false;
					} else {
						buf.append(", ");
					}
					buf.append(bT(PDTPredicates.PDT_SET_BREAKPOINT, FileUtils.prologFileNameQuoted(m.getFile()), m.getLineNumber(), m.getOffset(), "_"));
				}
				Debug.debug("Resetting breakpoints after restart: " + buf.toString());
//				PrologProcess process = PDTCommonUtil.getActivePrologProcess();
				try {
					process.queryOnce(buf.toString());
				} catch (PrologProcessException e) {
					Debug.report(e);
				}
				
				// disable logging of deleted ids
				markerBackup = null;
				shouldUpdateMarkers = true;
			}
		};
		if (reconsultedFiles == null || reconsultedFiles.isEmpty()) {
			runAsJob("Recreate breakpoints", null, r);
		} else {
			runAsJob("Recreate breakpoints", new MultiRule(reconsultedFiles.toArray(new IFile[reconsultedFiles.size()])), r);
		}
	}

}


