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


package org.cs3.pdt.editor.internal.editors;

import java.io.File;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.editor.internal.ImageRepository;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.part.MultiPageEditorPart;

public class QLFEditor extends MultiPageEditorPart {

	@Override
	protected void createPages() {
		Composite composite = new Composite(getContainer(), SWT.NONE);

		IPath newPath = getCurrentIFile().getLocation().removeFileExtension().addFileExtension("pl");
		final File plFile = newPath.toFile();
		boolean plFileExists = plFile.exists();
		
		try {
			FormLayout thisLayout = new FormLayout();
			composite.setLayout(thisLayout);
			composite.setSize(233, 272);
			{
				Label iconLabel = new Label(composite, SWT.NONE);
				FormData label1LData = new FormData();
				label1LData.left = new FormAttachment(0, 1000, 16);
				label1LData.top = new FormAttachment(0, 1000, 12);
				label1LData.width = 21;
				label1LData.height = 18;
				iconLabel.setLayoutData(label1LData);
				iconLabel.setImage(ImageRepository.getImage(ImageRepository.MESSAGE_INFO));
			}
			{
				Label label2 = new Label(composite, SWT.NONE);
				FormData label2LData = new FormData();
				label2LData.left =  new FormAttachment(0, 1000, 38);
				label2LData.top =  new FormAttachment(0, 1000, 15);
				label2LData.width = 300;
				label2LData.height = 15;
				label2.setLayoutData(label2LData);
				String labelText = "Can't edit binary file. ";
				if (plFileExists) {
					labelText += "Source file available.";
				} else {
					labelText += "No source file available.";
				}
				label2.setText(labelText);
			}
			{
				Button buttonEdit = new Button(composite, SWT.PUSH | SWT.CENTER);
				FormData buttonEditData = new FormData();
				buttonEditData.left =  new FormAttachment(0, 1000, 101);
				buttonEditData.top =  new FormAttachment(0, 1000, 42);
				buttonEditData.width = 92;
				buttonEditData.height = 25;
				buttonEdit.setLayoutData(buttonEditData);
				buttonEdit.setText("Edit Source File");
				buttonEdit.setEnabled(plFileExists);
				
				buttonEdit.addListener(SWT.Selection, new Listener() {
					@Override
					public void handleEvent(Event event) {
						
						PDTCommonUtil.openInEditor(plFile.toString());
						
					}
				});
			}
			{
				Button buttonConsult = new Button(composite, SWT.PUSH | SWT.CENTER);
				FormData buttonConsultData = new FormData();
				buttonConsultData.left =  new FormAttachment(0, 1000, 12);
				buttonConsultData.top =  new FormAttachment(0, 1000, 42);
				buttonConsultData.width = 77;
				buttonConsultData.height = 25;
				buttonConsult.setLayoutData(buttonConsultData);
				buttonConsult.setText("Consult QLF");

				buttonConsult.addListener(SWT.Selection, new Listener() {
					@Override
					public void handleEvent(Event event) {

						PDTConnectorPlugin.getDefault().getPrologProcessService().consultFile(getCurrentIFile());
					}
					
				});
			}
			composite.layout();
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		setPartName(getEditorInput().getName());
		addPage(composite);
	}
	

	private IFile getCurrentIFile() {
		if (getEditorInput() instanceof IFileEditorInput) {
			IFileEditorInput input = (IFileEditorInput) getEditorInput();
			return input.getFile();
		}
		return null;
	}
	
	@Override
	public void doSave(IProgressMonitor monitor) {}

	@Override
	public void doSaveAs() {}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	
}


