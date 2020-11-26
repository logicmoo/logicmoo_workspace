package org.cs3.pdt.refactoring;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

public class PDTRefactoringWizard extends RefactoringWizard {

	public PDTRefactoringWizard(Refactoring refactoring) {
		super(refactoring, WIZARD_BASED_USER_INTERFACE);
		setForcePreviewReview(true);
	}

	@Override
	protected void addUserInputPages() {
	}

}
