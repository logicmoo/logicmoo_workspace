package org.cs3.pdt.common.internal;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.ProcessReconsulter;
import org.cs3.pdt.common.ProcessReconsulterListener;
import org.cs3.pdt.common.PrologProcessStartListener;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.service.ConsultListener;
import org.cs3.pdt.connector.service.IPrologProcessService;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class ConsultManager implements ProcessReconsulter, ConsultListener, PrologProcessStartListener {
	
	private static final String ENTRY_POINT_FILES_MESSAGE = "automatic_reconsult(entry_points)";
	private static final String ALL_FILES_MESSAGE = "automatic_reconsult(all)";
	
	private HashSet<ProcessReconsulterListener> reconsultListeners = new HashSet<ProcessReconsulterListener>();

	@Override
	public void beforeConsult(PrologProcess process, List<IFile> files, IProgressMonitor monitor) throws PrologProcessException {
	}

	@Override
	public void afterConsult(PrologProcess process, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologProcessException {
		for (IFile file : files) {
			addConsultedFile(process, file);
		}
		monitor.done();
	}

	@Override
	public void prologProcessStarted(PrologProcess process) {
		String reconsultFiles = null;
		try {
			reconsultFiles = (String) process.getAttribute(PDTCommon.PROCESS_SPECIFIC_RECONSULT_STRATEGY);
		} catch (ClassCastException e) {
		}
		process.setAttribute(PDTCommon.PROCESS_SPECIFIC_RECONSULT_STRATEGY, null);
		if (reconsultFiles == null) {
			reconsultFiles = PDTCommonPlugin.getDefault().getPreferenceValue(PDTCommon.PREF_RECONSULT_ON_RESTART, PDTCommon.RECONSULT_NONE);
		}
		
		if (reconsultFiles.equals(PDTCommon.RECONSULT_NONE)) {
			getConsultedFileList(process).clear();
			notifyReconsultListeners(process, PDTCommon.RECONSULT_NONE, null);
		} else {
			reconsultFiles(process, reconsultFiles.equals(PDTCommon.RECONSULT_ENTRY));
		}
	}
	
	// TODO: problem with quotes
	private void reconsultFiles(PrologProcess process, boolean onlyEntryPoints) {
		Debug.debug("Reconsult files");
		List<IFile> consultedFiles = getConsultedFileList(process);
		if (consultedFiles != null) {
			synchronized (consultedFiles) {
				ArrayList<IFile> entryPointFiles = new ArrayList<IFile>();
				IPrologProcessService service = PDTConnectorPlugin.getDefault().getPrologProcessService();
				if (onlyEntryPoints) {
					filterEntryPoints(consultedFiles, entryPointFiles);
					service.consultFiles(entryPointFiles, process, ENTRY_POINT_FILES_MESSAGE);
					notifyReconsultListeners(process, PDTCommon.RECONSULT_ENTRY, entryPointFiles);
				} else {
					service.consultFiles(consultedFiles, process, ALL_FILES_MESSAGE);
					notifyReconsultListeners(process, PDTCommon.RECONSULT_ALL, consultedFiles);
				}
			}
		}
	}

	private List<IFile> getConsultedFileList(PrologProcess process) {
		@SuppressWarnings("unchecked")
		List<IFile> consultedFiles = (List<IFile>) process.getAttribute(PDTCommon.CONSULTED_FILES);
		return consultedFiles;
	}
	
	private void addConsultedFile(PrologProcess process, IFile file) {
		List<IFile> consultedFiles = getConsultedFileList(process);
		if (consultedFiles == null) {
			consultedFiles = new ArrayList<IFile>();
			process.setAttribute(PDTCommon.CONSULTED_FILES, consultedFiles);
		}
		synchronized (consultedFiles) {
			// only take the last consult of a file
			if (consultedFiles.remove(file)) {
				Debug.debug("move " + file + " to end of consulted files");			
			} else {
				Debug.debug("add " + file + " to consulted files");
			}
			consultedFiles.add(file);
		}
	}
	
	private void filterEntryPoints(List<IFile> files, List<IFile> entryPointFiles) {
		for (IFile file : files) {
			if (PDTCommonPlugin.getDefault().isEntryPoint(file)) {
				entryPointFiles.add(file);
			}
		}
	}

	@Override
	public void registerListener(ProcessReconsulterListener listener) {
		synchronized (reconsultListeners) {
			reconsultListeners.add(listener);
		}
	}

	@Override
	public void unRegisterListener(ProcessReconsulterListener listener) {
		synchronized (reconsultListeners) {
			reconsultListeners.remove(listener);
		}
	}
	
	@SuppressWarnings("unchecked")
	private void notifyReconsultListeners(PrologProcess process, String reconsultBehaviour, List<IFile> reconsultedFiles) {
		HashSet<ProcessReconsulterListener> listenersClone;
		synchronized (reconsultListeners) {
			listenersClone = (HashSet<ProcessReconsulterListener>) reconsultListeners.clone();
		}
		for (ProcessReconsulterListener listener : listenersClone) {
			listener.afterReconsult(process, reconsultBehaviour, reconsultedFiles);
		}
	}

}
