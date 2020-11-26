package org.cs3.pdt.refactoring;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;

public class PDTRefactoring {

	public static final String VARIABLE_REPLACEMENT = "Replacement";
	public static final String VARIABLE_OFFSET_END = "OffsetEnd";
	public static final String VARIABLE_OFFSET_START = "OffsetStart";
	public static final String VARIABLE_FILE = "File";

	public static int runRefactoringWithQuery(Shell shell, String query) throws InterruptedException {
		return runRefactoringWithQuery(shell, PDTCommonUtil.getActivePrologProcess(), query);
	}
	
	public static int runRefactoringWithQuery(Shell shell, PrologProcess process, String query) throws InterruptedException {
		ProcessorBasedRefactoring refactoring = new ProcessorBasedRefactoring(new PDTRefactoringProcessor(process, query));
		PDTRefactoringWizard wizard = new PDTRefactoringWizard(refactoring);
		RefactoringWizardOpenOperation operation = new RefactoringWizardOpenOperation(wizard);
		return operation.run(shell, "PDT Refactoring");
	}

}
