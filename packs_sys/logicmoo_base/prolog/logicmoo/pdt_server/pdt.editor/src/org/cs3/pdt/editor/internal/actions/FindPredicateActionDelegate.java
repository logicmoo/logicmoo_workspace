/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.actions;

import static org.cs3.pdt.common.search.SearchConstants.RESULT_KIND_DYNAMIC;
import static org.cs3.pdt.common.search.SearchConstants.RESULT_KIND_FOREIGN;
import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	
	private static final String MESSAGE_EXTERNAL_PREDICATE = "There is no Prolog source code for this predicate (only compiled external language code).";

	private static final String MESSAGE_DYNAMIC_PREDICATE = "There is no Prolog source code for this dynamic predicate.";

	private static final String MESSAGE_UNDEFINED_PREDICATE = "The selected predicate is not defined.";

	public static final String NAME = "Open Primary Definition or Declaration";
	
	private PLEditor editor;

	/**
	 *  
	 */
	public FindPredicateActionDelegate(PLEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI), FindPredicateActionDelegate.class.getName(), editor);
		this.editor = editor;

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	@Override
	public void run() {
		try {
			final Goal goal = editor.getSelectedPrologElement();
			Shell shell = editor.getEditorSite().getShell();
			if (goal == null) {
				UIUtils.displayMessageDialog(shell, "PDT Plugin", "Cannot locate a predicate at the specified location.");
				return;
			}

			Job j = new Job("Searching predicate definition") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					try {
						monitor.beginTask("searching...", IProgressMonitor.UNKNOWN);
						run_impl(goal);
					} catch (Throwable e) {
						Debug.report(e);
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

			};
			j.schedule();
		} catch (Throwable t) {
			Debug.report(t);
		}

	}

	public void dispose() {
	}

	private void run_impl(final Goal goal) throws CoreException {
		try {
			IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
			int start = UIUtils.physicalToLogicalOffset(document, goal.getStart());
			int end = UIUtils.physicalToLogicalOffset(document, goal.getEnd());
			String query = bT(PDTCommonPredicates.FIND_PRIMARY_DEFINITION_VISIBLE_IN,
					quoteAtom(goal.getFilePath()),
					Integer.toString(goal.getLine()),
					Integer.toString(start),
					Integer.toString(end),
					quoteAtom(goal.getTermString()),
					"TargetKind",
					"TargetFile",
					"TargetLine",
					"TargetLabel");

			PrologProcess process = PDTCommonUtil.getActivePrologProcess();
			final List<Map<String, Object>> results = process.queryAll(query);

			if (results.isEmpty()) {
				showErrorMessage(MESSAGE_UNDEFINED_PREDICATE);
				return;
			}
			String targetKind = (String) results.get(0).get("TargetKind");
			if (RESULT_KIND_DYNAMIC.equals(targetKind)) {
				showErrorMessage(MESSAGE_DYNAMIC_PREDICATE);
			}
			if (RESULT_KIND_FOREIGN.equals(targetKind)) {
				showErrorMessage(MESSAGE_EXTERNAL_PREDICATE);
			}
			if (results.size() == 1) {
				selectResultInEditor(results.get(0));
			} else {
				if (SearchConstants.RESULT_KIND_TRANSPARENT.equals(targetKind)) {
					editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
						@Override
						public void run() {
							AlternativeDialog alternativeDialog = new AlternativeDialog(
									editor.getEditorSite().getShell(),
									results,
									"The selected predicate " + goal.getSignature() + " has multiple targets.\n" +
									"Select a target and press OK to open it in an editor.");
							alternativeDialog.setBlockOnOpen(false);
							alternativeDialog.open();
						}
					});
				} else if (SearchConstants.RESULT_KIND_MULTIFILE.equals(targetKind)) {
					selectResultInEditor(results.get(0));
					new FindDefinitionsActionDelegate(editor).run();
				} else if (SearchConstants.RESULT_KIND_DWIM.equals(targetKind)) {
					editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
						@Override
						public void run() {
							AlternativeDialog alternativeDialog = new AlternativeDialog(
									editor.getEditorSite().getShell(),
									results,
									"The selected predicate " + goal.getSignature() + " was not found. A list of similar predicates is listed below.\n" +
									"Select a predicate and press OK to open it in an editor.");
							alternativeDialog.setBlockOnOpen(false);
							alternativeDialog.open();
						}
					});
				}
			}
		} catch (Exception e) {
			Debug.report(e);
		}
	}
	
	private void showErrorMessage(String message) {
		UIUtils.displayMessageDialog(
				editor.getSite().getShell(),
				NAME,
				message);
	}
	
	private void selectResultInEditor(Map<String, Object> result) {
		final String fileName = (String) result.get("TargetFile");
		final int line;
		try {
			line = Integer.parseInt((String) result.get("TargetLine"));
			editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						PDTCommonUtil.selectInEditor(line, fileName, true);
					} catch (PartInitException e) {
						Debug.report(e);
					}
				}
			});
		} catch (NumberFormatException e) {
		}
	}
	
}



