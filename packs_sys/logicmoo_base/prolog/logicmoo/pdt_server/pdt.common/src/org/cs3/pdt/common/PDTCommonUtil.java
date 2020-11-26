/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.common.metadata.SourceLocation;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class PDTCommonUtil {
	
	private static final String SPAN_HIDDEN = "<span style={display:none}>";
	private static final String NO_DOCUMENTATION = "NO_DOC";
	
	public static String[] getPredicateArgNamesFromDocumentation(String doc) {
		String[] names = null;
		if (doc != null && !doc.equals(NO_DOCUMENTATION)) {
			if (doc.contains("<dt class=\"pubdef\">")){
				if (doc.indexOf("arglist") > 0 && doc.indexOf("</var>") > doc.indexOf("arglist")) {
					String commaSeparatedArgs = doc.substring(doc.indexOf("arglist") + 10, doc.indexOf("</var>") -1);
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\?", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\-", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\+", "");
					names = commaSeparatedArgs.split(",");
					for (int i = 0; i < names.length; i++) {
						int typeSeparator = names[i].indexOf(':');
						if (typeSeparator >= 0) {
							names[i] = names[i].substring(0, typeSeparator);
						}
						names[i] = names[i].trim();
					}
				}
			} else if (doc.indexOf(SPAN_HIDDEN) > 0 && doc.indexOf("</span>") > 0) {
				String head = doc.substring(doc.indexOf(SPAN_HIDDEN) + SPAN_HIDDEN.length(), doc.indexOf("</span>"));
				int indexOfOpeningBracket = head.indexOf("(");
				if (indexOfOpeningBracket != -1) {
					names = head.substring(indexOfOpeningBracket + 1, head.lastIndexOf(")")).split(",");
				}
			}
		}
		return names;
	}
	
	public static String prologFileName(IEditorInput input) {
		if (input instanceof FileEditorInput) {
			FileEditorInput fileEditorInput = (FileEditorInput)input;
			IPath path = fileEditorInput.getPath();
			File file = path.toFile();
			return QueryUtils.prologFileName(file);
		}
		if (input instanceof FileStoreEditorInput) {
			FileStoreEditorInput e = (FileStoreEditorInput)input;
			File file = new File(e.getURI());
			return QueryUtils.prologFileName(file);
		}
		return input.getName();
	}

	public static void showSourceLocation(final SourceLocation loc) {
		if (Display.getCurrent() != UIUtils.getDisplay()) {
	
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					showSourceLocation(loc);
				}
			});
			return;
		}
		
		// For predicates implemented by external language code the Prolog side returns
		// an error message instead of a file name. Intercept and display it:
		if (loc.file.equals("No Prolog source code (only compiled external language code)")) {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			UIUtils.displayMessageDialog(
					shell,
					"External language predicate",
					"There is no Prolog source code for this predicate (only compiled external language code).");
			return;
		}
		
		if (loc.isLineLocation()) {
			try {
				selectInEditor(loc.getLine(), loc.file, true);
			} catch (PartInitException e) {
				Debug.report(e);
			}
		} else {
			try {
				selectInEditor(loc.getOffset(), loc.getEndOffset() - loc.getOffset(), loc.file, true);
			} catch (PartInitException e) {
				Debug.report(e);
			}
		}
		
		
	}

	public static String getProperty(String property, List<String> properties) {
		for (String p : properties) {
			if (p.startsWith(property)) {
				return p.substring(property.length() + 1, p.length() - 1);
			}
		}
		return null;
	}

	private abstract static class _SyncReturn implements Runnable {
		public Object rval;

		_SyncReturn() {
			Display display = UIUtils.getDisplay();
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
	
	/**
	 * Open file in its default editor.
	 * 
	 * @param file
	 * @param activate
	 * @return
	 * @throws PartInitException
	 */
	static public IEditorPart openInEditor(final IFile file, boolean activate)
			throws PartInitException {
		if (file != null) {
			IWorkbenchPage p = UIUtils.getActivePage();
			final long limit = PDTCommonPlugin.getDefault().getPreferenceStore().getLong(PDTCommon.PREF_FILE_SIZE_LIMIT);
			if (limit > 0) {
				try {
					long length = EFS.getStore(file.getLocationURI()).fetchInfo().getLength();
					if (length > limit * 1024) {
						Boolean answer = (Boolean) new _SyncReturn() {
							@Override
							Object getRVal() {
								boolean answer = MessageDialog.openQuestion(UIUtils.getActiveShell(), "Open file " + file.getName(), "The file " + file.getName() + " is larger than " + limit + "kB. Continue opening the file?");
								return new Boolean(answer);
							}
						}.rval;
						if (!answer) {
							return null;
						}
					}
				} catch (CoreException e) {
					Debug.report(e);
					return null;
				}
			}
			if (p != null) {
				IEditorPart editorPart = IDE.openEditor(p, file, activate);
				return editorPart;
			}
		}
		return null;
	}

	public static IEditorPart openInEditor(String fileName) {
		try {
			Path path = new Path(new File(fileName).getCanonicalPath());
	
			IFile file = FileUtils.findFileForLocation(path);
			if (file == null){
				IFileStore fileStore = EFS.getLocalFileSystem().getStore(path);
				if (!fileStore.fetchInfo().isDirectory() && fileStore.fetchInfo().exists()) {
					IWorkbenchPage page = UIUtils.getActivePage();
					IEditorPart part = IDE.openEditorOnFileStore(page, fileStore);
					return part;
				}
			} else {
				final IEditorPart part = openInEditor(file, false);
				if (part != null) {
					Display.getDefault().asyncExec(new Runnable() {
						@Override
						public void run() {
							part.setFocus();
						}
					});
				}
				return part;
			}
		} catch (IOException e) {
			Debug.report(e);
		} catch (PartInitException e) {
			Debug.report(e);
		}
		return null;
	}
	
	public static void selectInEditor(int start, int length, String filename, boolean activate) throws PartInitException {
		selectInEditor(start, length, filename, activate, true);
	}
	
	public static void selectInEditor(int start, int length, String filename, boolean activate, boolean adjustOffset) throws PartInitException {
		try {
			IFile file = FileUtils.findFileForLocation(filename);
			selectInEditor(start, length, file, activate, adjustOffset);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	public static void selectInEditor(int start, int length, IFile file, boolean activate) throws PartInitException {
		selectInEditor(start, length, file, activate, true);
	}
	
	public static void selectInEditor(int start, int length, IFile file, boolean activate, boolean adjustOffset) throws PartInitException {
		if (file == null) {
			return;
		}
		IEditorPart editor = openInEditor(file, activate);
		if (editor == null || !(editor instanceof AbstractTextEditor)) {
			return;
		}
		IDocument document = ((AbstractTextEditor) editor).getDocumentProvider().getDocument(editor.getEditorInput());
		if (adjustOffset) {
			int end = UIUtils.logicalToPhysicalOffset(document, start + length);
			start = UIUtils.logicalToPhysicalOffset(document, start);
			length = end - start;
		}
		ISelection selection = new TextSelection(document, start, length);
		editor.getEditorSite().getSelectionProvider().setSelection(selection);
	}

	public static void selectInEditor(int line, String filename, boolean activate) throws PartInitException {
		try {
			IFile file = FileUtils.findFileForLocation(filename);
			selectInEditor(line, file, activate);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	public static void selectInEditor(int line, IFile file, boolean activate) throws PartInitException {
		if (file == null) {
			return;
		}
		IEditorPart editor = openInEditor(file, activate);
		if (editor == null || !(editor instanceof AbstractTextEditor)) {
			return;
		}
		IDocument document = ((AbstractTextEditor) editor).getDocumentProvider().getDocument(editor.getEditorInput());
		int offset;
		try {
			offset = document.getLineInformation(line - 1).getOffset();
			TextSelection newSelection = new TextSelection(document, offset, 0);
			editor.getEditorSite().getSelectionProvider().setSelection(newSelection);
		} catch (BadLocationException e) {
			Debug.report(e);
		} catch (Exception e) {
			Debug.report(e);
		}
	}

	/*
	 * save URL from urlString to file outputFile
	 */
	public static boolean saveUrlToFile(String urlString, File outputFile, IProgressMonitor monitor)
	{	
		
		URL url;
		try {
			url = new URL(urlString.replace(" ", "%20"));
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.connect();

			int responseCode = conn.getResponseCode();

			if (responseCode == HttpURLConnection.HTTP_OK)
			{
				if (monitor != null)
				{
					int size = conn.getContentLength();
					monitor.beginTask("Downloading " + outputFile.getName(), size);
				}
				OutputStream os = new FileOutputStream(outputFile);

				byte tmp_buffer[] = new byte[4096];
				InputStream is = conn.getInputStream();
				int n;
				while ((n = is.read(tmp_buffer)) > 0) {
					os.write(tmp_buffer, 0, n);
					os.flush();
					if (monitor != null)
						monitor.worked(4096);
				}
				is.close();
				os.close();
				return true;
			} else {
				return false;
				//throw new IllegalStateException("HTTP response: " + responseCode);
			}
		} catch (Exception e) {
			return false;
		}
		
	}
	
	/*
	 * get filesize from URL from urlString
	 */
	public static int getFilesizeFromUrl(String urlString)
		throws  IllegalStateException, IOException
	{	
		
		URL url = new URL(urlString.replace(" ", "%20"));
		HttpURLConnection conn = (HttpURLConnection) url.openConnection();
		conn.setRequestMethod("GET");
		conn.connect();
		
		int responseCode = conn.getResponseCode();
		
		if (responseCode == HttpURLConnection.HTTP_OK)
		{
			return conn.getContentLength();
		} else {
	    throw new IllegalStateException("HTTP response: " + responseCode);
	}
	}
	
	public static String getHtmlDocumentation(String docKind, String doc) {
		if (SearchConstants.COMPLETION_DOC_KIND_NODOC.equals(docKind)) {
			return null;
		} else if (SearchConstants.COMPLETION_DOC_KIND_TEXT.equals(docKind)) {
			return "<html><head><style>\n" + getPlDocCss() + "\n</style></head><body>" + doc + "</body></html>";
		} else if (SearchConstants.COMPLETION_DOC_KIND_HTML.equals(docKind)) {
			if (doc != null) {
//				if(doc.indexOf("\n") > -1){
//					doc="<b>"+doc.trim().replaceFirst("\n", "</b><br/>").replace("\n", "<br/>");
//				}
				return "<html><head><style>\n" + getPlDocCss() + "\n</style></head><body>" + doc.trim() + "</body></html>";
			} else {
				return null;
			}
		} else if (SearchConstants.COMPLETION_DOC_KIND_FILE.equals(docKind)) {
			String fileContent = Util.readFromFile(new File(doc));
			if (fileContent != null && !fileContent.isEmpty()) {
				return fileContent;
			}
		} else if (SearchConstants.COMPLETION_DOC_KIND_LGT_HELP_FILE.equals(docKind)) {
			String fileContent = Util.readFromFile(new File(doc));
			if (fileContent != null && !fileContent.isEmpty()) {
				return fileContent.substring(fileContent.indexOf("<html"));
			}
		}
		return null;
	}

	private static String plDocCss;
	
	private static String getPlDocCss() {
		if (plDocCss == null) {
			URL url = PDTCommonPlugin.getDefault().getBundle().getEntry("/css/pldoc.css");
			StringBuilder buf = new StringBuilder();
			try {
				InputStream inputStream = url.openConnection().getInputStream();
				BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
				String inputLine;
				while ((inputLine = in.readLine()) != null) {
					buf.append(inputLine);
				}
				in.close();
			} catch (IOException e) {
				Debug.report(e);
			}
			plDocCss = buf.toString();
		}
		return plDocCss;
	}
	
	public static PrologProcess getActivePrologProcess() {
		return PDTConnectorPlugin.getDefault().getPrologProcessService().getActivePrologProcess();
	}
	
	public static boolean hasActivePrologProcess() {
		return PDTConnectorPlugin.getDefault().getPrologProcessService().hasActivePrologProcess();
	}
	
	private static final HashSet<String> PROLOG_FILE_EXTENSIONS = new HashSet<String>();

	static {
		PROLOG_FILE_EXTENSIONS.add("pl");
		PROLOG_FILE_EXTENSIONS.add("plt");
		PROLOG_FILE_EXTENSIONS.add("pro");
		PROLOG_FILE_EXTENSIONS.add("lgt");
		PROLOG_FILE_EXTENSIONS.add("logtalk");
	}

	public static Set<String> getPrologFileExtensions() {
		return PROLOG_FILE_EXTENSIONS;
	}
	
	public static boolean isPrologFile(IFile file) {
		return PROLOG_FILE_EXTENSIONS.contains(file.getFileExtension());
	}
	
	public static boolean isPrologFile(String filename) {
		return filename.contains(".") && PROLOG_FILE_EXTENSIONS.contains(filename.substring(filename.lastIndexOf('.') + 1));
	}

	public static void updateEntryPointsInProcess(PrologProcess process) throws PrologProcessException {
		process.queryOnce(bT(PDTCommonPredicates.REMOVE_ENTRY_POINTS, "_"));

		for (IFile file : PDTCommonPlugin.getDefault().getAllEntryPoints()) {
			try {
				String prologFileName = QueryUtils.prologFileName(file.getLocation().toFile().getCanonicalFile());
				process.queryOnce(bT(PDTCommonPredicates.ADD_ENTRY_POINT, QueryUtils.quoteAtom(prologFileName)));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}
	
	public static String cropText(String text, int maxLength) {
		if (text == null) {
			return null;
		}
		if (maxLength < 25 || text.length() <= maxLength) {
			return text.replace("\n", " ").replace("\r", "").replace("\t", "");
		}
		return (text.substring(0, maxLength / 2) + "..." + text.substring(text.length() - maxLength / 2)).replace("\n", " ").replace("\r", "").replace("\t", "");
	}

}
