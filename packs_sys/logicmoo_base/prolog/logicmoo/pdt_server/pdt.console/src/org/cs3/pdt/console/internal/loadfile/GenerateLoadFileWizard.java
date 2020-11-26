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

package org.cs3.pdt.console.internal.loadfile;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

public class GenerateLoadFileWizard extends Wizard implements INewWizard {

	private List<String> consultedFiles;
	
	private IStructuredSelection selection;
	private GenerateLoadFilePage newFileWizardPage;

	public GenerateLoadFileWizard(List<String> consultedFiles) {
		setWindowTitle("Generate prolog load file");
		this.consultedFiles = consultedFiles;
	} 

    @Override    
    public void addPages() {
    	if (selection == null) {
    		selection = new StructuredSelection();
    	}
    	newFileWizardPage = new GenerateLoadFilePage(selection);
    	addPage(newFileWizardPage);
    }
    
    @Override
    public boolean performFinish() {
    	IFile file = newFileWizardPage.createNewFile();
    	if (file != null) {
    		writeFileContent(file);
    		if (newFileWizardPage.isEntryPoint()) {
    			addEntryPoint(file);
    		}

    		return true;
    	}
    	else
    		return false;
    	}

	public void addEntryPoint(IFile file) {
		PDTCommonPlugin.getDefault().addEntryPoint(file);
		
		PrologProcess process = PDTCommonUtil.getActivePrologProcess();

		if (process != null) {
			try {
				String prologFileName = FileUtils.prologFileNameQuoted(file);

				process.queryOnce(bT(PDTCommonPredicates.ADD_ENTRY_POINT, prologFileName));
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
		}
	}

	public void writeFileContent(IFile file) {
		try {
			IPath projectLocation = file.getProject().getLocation();
			IPath projectFolder = new Path(projectLocation.toString().toLowerCase());
			
			StringBuffer buf = new StringBuffer();
			buf.append(":- dynamic user:file_search_path/2.\n");
			buf.append(":- multifile user:file_search_path/2.\n");
			String projectAlias = QueryUtils.quoteAtom(file.getProject().getName());
			buf.append("user:file_search_path(" + projectAlias + ", '" + projectFolder + "').\n\n");
			for (String fileName : consultedFiles) {
				String fileNameWithoutQuotes = Util.unquoteAtom(fileName);
				IPath path = new Path(fileNameWithoutQuotes);
				IPath relPath = path.makeRelativeTo(projectFolder);
				if (path.equals(relPath)) {
					buf.append(":- consult('" + relPath.toString().toLowerCase() + "').\n");
				} else {
					buf.append(":- consult(" + projectAlias + "('" + relPath.toString().toLowerCase() + "')).\n");
				}
			}
			String content = buf.toString();
			
			file.setContents(new ByteArrayInputStream(content.getBytes()), IResource.FORCE, null);
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
    
    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
    	this.selection = selection;
    }


    public class GenerateLoadFilePage extends WizardNewFileCreationPage {

    	public GenerateLoadFilePage(IStructuredSelection selection) {
    		super("GenerateLoadFilePage", selection);  
    		setTitle("Load File");
    		setDescription("Creates a new load file");
    		setFileExtension("pl");
    		setFileName("load.pl");
    	}
    	
    	Button entryPointCheckbox;
    	
    	 @Override
		public void createControl(Composite parent) {
    	      // inherit default container and name specification widgets
    	      super.createControl(parent);
    	      Composite composite = (Composite)getControl();
    	      
    	      entryPointCheckbox = new Button(composite,SWT.CHECK);
    	      entryPointCheckbox.setText("Mark file as entry point");
    	      entryPointCheckbox.setSelection(true);
    	 }
    	 
    	 public boolean isEntryPoint() {
    		 return entryPointCheckbox.getSelection();
    	 }

    }

}



