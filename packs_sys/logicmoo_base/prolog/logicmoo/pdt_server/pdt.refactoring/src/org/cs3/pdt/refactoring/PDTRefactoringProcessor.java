package org.cs3.pdt.refactoring;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.SharableParticipants;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;

public class PDTRefactoringProcessor extends RefactoringProcessor {

    private static final String PROCESSOR_ID = "org.cs3.pdt.refactoring.processor";
	private static final String PROCESSOR_NAME = "PDT Refactoring Processor";
	
	private final PrologProcess process;
	private final String query;

	public PDTRefactoringProcessor(PrologProcess process, String query) {
		this.process = process;
		this.query = query;
	}
	
	@Override
	public Object[] getElements() {
		return new Object[]{""};
	}

	@Override
	public String getIdentifier() {
		return PROCESSOR_ID;
	}

	@Override
	public String getProcessorName() {
		return PROCESSOR_NAME;
	}

	@Override
	public boolean isApplicable() throws CoreException {
		return true;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm) throws CoreException, OperationCanceledException {
		return new RefactoringStatus();
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm, CheckConditionsContext context) throws CoreException, OperationCanceledException {
		return new RefactoringStatus();
	}

	@Override
	public RefactoringParticipant[] loadParticipants(RefactoringStatus status, SharableParticipants sharedParticipants) throws CoreException {
		return new RefactoringParticipant[0];
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException, OperationCanceledException {
		CompositeChange change = null;
		try {
			List<Map<String, Object>> results = process.queryAll(query);
			change = new CompositeChange("Replacements");
			HashMap<String, TextFileChange> changes = new HashMap<>();
			for (Map<String, Object> result : results) {
				try {
					String path = (String) result.get(PDTRefactoring.VARIABLE_FILE);
					int offsetStart = Integer.parseInt((String) result.get(PDTRefactoring.VARIABLE_OFFSET_START));
					int offsetEnd = Integer.parseInt((String) result.get(PDTRefactoring.VARIABLE_OFFSET_END));
					String replacement = (String) result.get(PDTRefactoring.VARIABLE_REPLACEMENT);
					if (path == null || replacement == null) {
						continue;
					}
					path = Util.unquoteAtom(path);
					TextFileChange textFileChange = changes.get(path);
					if (textFileChange == null) {
						IFile file = FileUtils.findFileForLocation(path);
						textFileChange = new TextFileChange(file.getName(), file);
						MultiTextEdit fileChangeRootEdit = new MultiTextEdit();
						textFileChange.setEdit(fileChangeRootEdit);
						changes.put(path, textFileChange);
						change.add(textFileChange);
					}
					
					IDocument document = UIUtils.getDocument(new File(path));
					int convertedOffsetStart = UIUtils.logicalToPhysicalOffset(document, offsetStart);
					int convertedOffsetEnd = UIUtils.logicalToPhysicalOffset(document, offsetEnd);
					ReplaceEdit replaceEdit = new ReplaceEdit(convertedOffsetStart, convertedOffsetEnd - convertedOffsetStart, replacement);
					textFileChange.addEdit(replaceEdit);
				} catch (Exception e) {
					Debug.report(e);
				}
			}
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
		return change;
	}

}
