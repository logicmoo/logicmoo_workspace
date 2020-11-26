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

package org.cs3.pdt.connector.util;

import java.io.File;

import org.cs3.prolog.connector.common.Util;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * most of the methods in this class include code that needs to run on the ui
 * thread.
 * 
 * If the calling thread is not the ui thread, this methods will take care of
 * scheduling the respective code using Diplay.asyncExec() for void methods and
 * Display.syncExec for all others.
 * 
 */
public final class UIUtils {

	/**
	 * hint for file option. if true, option values should be in the workspace
	 */
	public final static String IS_WORKSPACE_RESOURCE = "is_workspace_resource";

	/**
	 * hint for file options. if set, only files/folders below the given
	 * container should be legal option values.
	 */
	public final static String ROOT_CONTAINER = "root_container";

	/**
	 * hint for file options. if set to true, values should be interpreted as
	 * relative paths. This typically involves ROOT_CONTAINER, too.
	 * 
	 */
	public final static String RELATIVE = "relative";

	private abstract static class _SyncReturn implements Runnable {
		public Object rval;

		_SyncReturn() {
			Display display = getDisplay();
			if (Display.getCurrent() != display) {
				display.syncExec(this);
			} else {
				run();
			}
		}

		@Override
		public void run() {
			rval = getRVal();
		}

		abstract Object getRVal();
	}

	public static IFile getFileInActiveEditor() {
		try {
			return (IFile) new _SyncReturn() {
				@Override
				Object getRVal() {
					IEditorPart activeEditor = getActiveEditor();
					FileEditorInput fileEditorInput = ((FileEditorInput) activeEditor
							.getEditorInput());
					IFile file = fileEditorInput.getFile();
					return file;
				}
			}.rval;
		} catch (Exception e) {
			return null;
		}
	}

	public static Display getDisplay() {
		return PlatformUI.getWorkbench().getDisplay();
	}

	public static Shell getActiveShell() {
		return (Shell) new _SyncReturn() {
			@Override
			Object getRVal() {

				return getDisplay().getActiveShell();
			}
		}.rval;
	}

	public static IWorkbenchPage getActivePage() {
		return (IWorkbenchPage) new _SyncReturn() {
			@Override
			Object getRVal() {
				IWorkbenchWindow workbench = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				if (workbench == null) {
					return null;
				}
				return workbench.getActivePage();
			}
		}.rval;
	}

	public static IEditorPart getActiveEditor() {
		return (IEditorPart) new _SyncReturn() {
			@Override
			Object getRVal() {
				IWorkbenchPage page = getActivePage();
				if (page == null)
					return null;
				return page.getActiveEditor();
			}
		}.rval;
	}

	public static void displayMessageDialog(final Shell shell,
			final String title, final String msg) {
		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					displayMessageDialog(shell, title, msg);
				}
			});
			return;
		}
		MessageDialog.openInformation(shell, title, msg);
	}

	/**
	 * Copied here from JTUtils.
	 * 
	 * @return
	 */
	public static boolean isTestingMode() {
		String testingEnv = System.getProperty("JT_TESTING");
		if (testingEnv == null) {
			testingEnv = System.getenv("JT_TESTING");
		}
		return (testingEnv != null) && testingEnv.toLowerCase().equals("true");
	}

	public static void displayErrorDialog(final Shell shell,
			final String title, final String msg) {
		if (isTestingMode())
			throw new RuntimeException("Error Dialog: \n" + title + "\n" + msg);

		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					displayMessageDialog(shell, title, msg);
				}
			});
			return;
		}
		MessageDialog.openError(shell, title, msg);
	}

	public static void displayErrorDialog(final ErrorMessageProvider provider,
			final Shell shell, final int code, final int context,
			final Exception x) {

		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					displayErrorDialog(provider, shell, code, context, x);
				}
			});
			return;
		}
		IStatus status = createErrorStatus(provider, x, code);
		String cxMsg = provider.getContextMessage(context);
		ErrorDialog.openError(shell, "Problem encountered", cxMsg, status);
	}

	public static IStatus createErrorStatus(ErrorMessageProvider provider,
			Throwable e, int errCode) {
		Status status = new Status(IStatus.ERROR, provider.getId(), errCode,
				provider.getErrorMessage(errCode), e);
		return status;
	}

	public static void logError(final ErrorMessageProvider provider,
			final int code, final int context, final Exception x) {
		IStatus status = createErrorStatus(provider, x, code);
		Plugin plugin = provider.getPlugin();
		plugin.getLog().log(status);
	}

	public static void logAndDisplayError(final ErrorMessageProvider provider,
			final Shell shell, final int code, final int context,
			final Exception x) {
		logError(provider, code, context, x);
		if (shell != null) {
			displayErrorDialog(provider, shell, code, context, x);
		}
	}

	public static void setStatusErrorMessage(final String string) {
		getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				getActiveEditor().getEditorSite().getActionBars()
				.getStatusLineManager().setErrorMessage(string);
			}
		});
	}

	public static void setStatusMessage(final String string) {
		getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				getActiveEditor().getEditorSite().getActionBars()
				.getStatusLineManager().setMessage(string);
			}
		});
	}

	public static IViewPart showView(String viewId) throws PartInitException {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
				.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null)
			return null;
		final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
		if (activePage == null)
			return null;
		return activePage.showView(viewId);
	}

	public static String getFileFromActiveEditor() {
		String enclFile;
		IEditorInput editorInput = UIUtils.getActiveEditor().getEditorInput();
		enclFile = getFileNameForEditorInput(editorInput);
		return enclFile;
	}

	public static String getFileNameForEditorInput(IEditorInput editorInput) {
		String enclFile;
		if (editorInput instanceof FileEditorInput) {
			enclFile = ((FileEditorInput) editorInput).getFile()
					.getRawLocation().toPortableString();
		} else {
			enclFile = new File(((FileStoreEditorInput) editorInput).getURI())
			.getAbsolutePath().replace('\\', '/');
		}
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}
		return enclFile;
	}

	public static int logicalToPhysicalOffset(IDocument doc, int offset) {
		return Util.logicalToPhysicalOffset(doc.get(), offset);
	}
	
	public static int physicalToLogicalOffset(IDocument doc, int offset) {
		return Util.physicalToLogicalOffset(doc.get(), offset);
	}
	
	public static IDocument getDocument(IFile file) throws CoreException{
		IPath path = file.getFullPath();
		return getDocument(path,LocationKind.IFILE);
	}

	public static IDocument getDocument(File file) throws CoreException{
		IPath path = new Path(file.getAbsolutePath());
		return getDocument(path,LocationKind.NORMALIZE);
	}

	public static IDocument getDocument(IPath location, LocationKind kind) throws CoreException{
		ITextFileBufferManager manager= FileBuffers.getTextFileBufferManager();
		try {
			manager.connect(location, kind,null);
			ITextFileBuffer buffer= manager.getTextFileBuffer(location,kind);
			// note: could be null
			return buffer.getDocument();
		}
		finally {
			manager.disconnect(location, kind,null);
		}
	}

}

