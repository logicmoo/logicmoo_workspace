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

package org.cs3.pdt.connector.internal.service;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.PrologConnectorPredicates;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.service.ActivePrologProcessListener;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.service.IPrologProcessService;
import org.cs3.pdt.connector.service.PDTReloadExecutor;
import org.cs3.pdt.connector.subscription.DefaultSubscription;
import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;

public class PrologProcessService implements IPrologProcessService {
	
	private DefaultReloadExecutor defaultReloadExecutor;
	
	public PrologProcessService() {
		defaultReloadExecutor = new DefaultReloadExecutor();
		registerPDTReloadExecutor(defaultReloadExecutor);
	}
	
	private PrologProcess activePrologProcess;
	
	private static final ISchedulingRule activeProcessChangedRule = new ISchedulingRule() {
		@Override
		public boolean isConflicting(ISchedulingRule rule) {
			return this == rule;
		}
		
		@Override
		public boolean contains(ISchedulingRule rule) {
			return this == rule;
		}
	};
	
	@Override
	public boolean hasActivePrologProcess() {
		return activePrologProcess != null;
	}
	
	PrologProcess getProcess() {
		return activePrologProcess;
	}
	
	@Override
	public PrologProcess getActivePrologProcess() {
		if (activePrologProcess == null) {
			setActivePrologProcess(null);
		}
		return activePrologProcess;
	}
	
	@Override
	public synchronized void setActivePrologProcess(PrologProcess process) {
		if (process == null) {
			activePrologProcess = getDefaultPrologProcess();
		} else {
			if (activePrologProcess == process) {
				return;
			} else {
				activePrologProcess = process;
			}
		}
		fireActivePrologProcessChanged(activePrologProcess);
	}
	
	@SuppressWarnings("unchecked")
	private synchronized void fireActivePrologProcessChanged(final PrologProcess process) {
		Job job = new Job("Active PrologProcess changed: notify listeners") {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				ArrayList<ActivePrologProcessListener> listenersClone;
				synchronized (activePrologProcessListeners) {
					listenersClone = (ArrayList<ActivePrologProcessListener>) activePrologProcessListeners.clone();
				}
				
				monitor.beginTask("Active PrologProcess changed: notify listeners", listenersClone.size());
				
				for (ActivePrologProcessListener listener : listenersClone) {
					try {
						listener.activePrologProcessChanged(process);
					} catch (Exception e) {
						Debug.report(e);
					}
					monitor.worked(1);
				}
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		job.setRule(activeProcessChangedRule);
		job.schedule();
	}
	
	private ArrayList<ActivePrologProcessListener> activePrologProcessListeners = new ArrayList<ActivePrologProcessListener>();
	
	@Override
	public void registerActivePrologProcessListener(ActivePrologProcessListener listener) {
		synchronized (activePrologProcessListeners) {
			activePrologProcessListeners.add(listener);
		}
	}
	
	@Override
	public void unRegisterActivePrologProcessListener(ActivePrologProcessListener listener) {
		synchronized (activePrologProcessListeners) {
			activePrologProcessListeners.remove(listener);
		}
	}
	
	private static final String DEFAULT_PROCESS = "Default Process";
	
	private PrologProcess getDefaultPrologProcess() {
		PrologProcessRegistry registry = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();
		Subscription subscription = registry.getSubscription(DEFAULT_PROCESS);
		if (subscription == null) {
			subscription = new DefaultSubscription(DEFAULT_PROCESS + "_indepent", DEFAULT_PROCESS, "Independent prolog process", "Prolog");
			registry.addSubscription(subscription);
		}
		PrologProcess process = PDTConnectorPlugin.getDefault().getPrologProcess(subscription);
		return process;
	}
	
	private TreeSet<PDTReloadExecutor> pdtReloadExecutors = new TreeSet<PDTReloadExecutor>(new Comparator<PDTReloadExecutor>() {
		@Override
		public int compare(PDTReloadExecutor o1, PDTReloadExecutor o2) {
			return o2.getPriority() - o1.getPriority();
		}
	});
	
	@Override
	public void registerPDTReloadExecutor(PDTReloadExecutor executor) {
		synchronized (pdtReloadExecutors) {
			pdtReloadExecutors.add(executor);
		}
	}
	
	@Override
	public void unRegisterPDTReloadExecutor(PDTReloadExecutor executor) {
		synchronized (pdtReloadExecutors) {
			pdtReloadExecutors.remove(executor);
		}
	}
	
	private HashSet<ConsultListener> consultListeners = new HashSet<ConsultListener>();

	@Override
	public void registerConsultListener(ConsultListener listener) {
		synchronized (consultListeners) {
			consultListeners.add(listener);
		}
	}
	
	@Override
	public void unRegisterConsultListener(ConsultListener listener) {
		synchronized (consultListeners) {
			consultListeners.remove(listener);
		}
	}
	
	@Override
	public void consultFile(String file) {
		consultFile(file, getActivePrologProcess(), null);
	}

	@Override
	public void consultFile(String file, PrologProcess process) {
		consultFile(file, process, null);
	}

	@Override
	public void consultFile(String file, PrologProcess process, String message) {
		try {
			consultFile(FileUtils.findFileForLocation(file), process, message);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	@Override
	public void consultFile(final IFile file) {
		consultFile(file, getActivePrologProcess(), null);
	}
	
	@Override
	public void consultFile(IFile file, PrologProcess process) {
		consultFile(file, process, null);
	}
	
	@Override
	public void consultFile(IFile file, PrologProcess process, String message) {
		ArrayList<IFile> fileList = new ArrayList<IFile>();
		fileList.add(file);
		consultFiles(fileList, process, message);
	}

	@Override
	public void consultFiles(List<IFile> files) {
		consultFiles(files, getActivePrologProcess(), null);
	}
	
	@Override
	public void consultFiles(final List<IFile> files, final PrologProcess process) {
		consultFiles(files, process, null);
	}
	
	@Override
	public void consultFiles(List<IFile> files, PrologProcess process, String message) {
		if (!files.isEmpty()) {
			consultFilesInJob(files, process, message);
		}
	}

	private void consultFilesInJob(final List<IFile> files, final PrologProcess process, final String message) {
		Job job = new Job("Consult " + files.size() + " file(s)") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					consultFilesImpl(files, process, monitor, message);
				} catch (Exception e) {
					Debug.report(e);
					return Status.CANCEL_STATUS;
				} finally {
					monitor.done();
				}
				return Status.OK_STATUS;
			}
		};
		job.setRule(new MultiRule(files.toArray(new IFile[files.size()])));
		job.schedule();
	}
	
	@SuppressWarnings("unchecked")
	private void consultFilesImpl(List<IFile> files, PrologProcess process, IProgressMonitor monitor, String message) throws PrologProcessException {
		HashSet<ConsultListener> consultListenersClone;
		synchronized (consultListeners) {
			consultListenersClone = (HashSet<ConsultListener>) consultListeners.clone();
		}
		
		monitor.beginTask("Consult " +  files.size() + " file(s)", consultListenersClone.size() * 4);
		
		for (ConsultListener listener : consultListenersClone) {
			monitor.subTask("Notify Listener");
			try {
				listener.beforeConsult(process, files, new SubProgressMonitor(monitor, 1));
			} catch (Exception e) {
				Debug.report(e);
			}
		}
		
		monitor.subTask("Execute reload");
		boolean success = false;
		try {
			success = executeReload(process, files, new SubProgressMonitor(monitor, consultListenersClone.size()), message);
		} catch (Exception e) {
			Debug.report(e);
		}
		
		if (success) {
			monitor.subTask("Collect all consulted files");
			List<String> allConsultedFiles = collectConsultedFiles(process, new SubProgressMonitor(monitor, consultListenersClone.size()));
			
			for (ConsultListener listener : consultListenersClone) {
				monitor.subTask("Notify Listener");
				try {
					listener.afterConsult(process, files, allConsultedFiles, new SubProgressMonitor(monitor, 1));
				} catch (Exception e) {
					Debug.report(e);
				}
			}
		}
		monitor.done();
	}
	
	private List<String> collectConsultedFiles(PrologProcess process, IProgressMonitor monitor) throws PrologProcessException {
		monitor.beginTask("", 1);
		
		List<String> result = new ArrayList<String>();
		
		List<Map<String, Object>> reloadedFiles = process.queryAll(bT(PrologConnectorPredicates.RELOADED_FILE, "File"));
		for (Map<String, Object> reloadedFile : reloadedFiles) {
			result.add(reloadedFile.get("File").toString());
		}
		
		monitor.done();
		
		return result;
	}

	@SuppressWarnings("unchecked")
	private boolean executeReload(PrologProcess process, List<IFile> files, IProgressMonitor monitor, String message) throws PrologProcessException {
		TreeSet<PDTReloadExecutor> executorsClone;
		synchronized (pdtReloadExecutors) {
			executorsClone = (TreeSet<PDTReloadExecutor>) pdtReloadExecutors.clone();
		}
		monitor.beginTask("Execute reload", executorsClone.size());
		String fileList = FileUtils.quotedPrologFileNameList(files);
		String query;
		if (message != null) {
			query = bT(PrologConnectorPredicates.PDT_RELOAD, fileList, message);
		} else {
			query = bT(PrologConnectorPredicates.PDT_RELOAD, fileList);
		}
		for (PDTReloadExecutor executor : executorsClone) {
			monitor.subTask("Execute reload");
			boolean success = executor.executePDTReload(process, query, new SubProgressMonitor(monitor, 1));
			if (success) {
				monitor.done();
				return true;
			}
		}
		monitor.done();
		return false;
	}

}


