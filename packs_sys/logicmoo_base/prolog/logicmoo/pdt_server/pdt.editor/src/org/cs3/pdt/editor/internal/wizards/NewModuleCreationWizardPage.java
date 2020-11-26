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

package org.cs3.pdt.editor.internal.wizards;


import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

public class NewModuleCreationWizardPage extends WizardNewFileCreationPage {

	/**
	 * copied from: org.eclipse.jdt.ui.wizards.ImportsManager:
	 */
	
	
//	private final static String PAGE_NAME= "NewModuleCreationWizardPage"; //$NON-NLS-1$
	
		
		/**
		 * Creates a new file creation wizard page. If the initial resource
		 * selection contains exactly one container resource then it will be used as
		 * the default container resource.
		 * 
		 * @param pageName
		 *            the name of the page
		 * @param selection
		 *            the current resource selection
		 */
		public NewModuleCreationWizardPage(String pageName,
				IStructuredSelection selection) {
			super(pageName,selection);
			setPageComplete(false);
			
			setTitle("New Prolog Module"); 
			setDescription("New Module");
			setFileExtension("pl");
		}
	
	@Override
	protected String getNewFileLabel() {
		return "Module Name: ";
	}
	
	/**
	 * TRHO
	 * 
	 * Returns the file extension to use when creating the new file.
	 * 
	 * @return the file extension or <code>null</code>.
	 * @see WizardNewFileCreationPage#setFileExtension(String)
	 * @since 3.3
	 */
	@Override
	public String getFileExtension() {
		return "pl";
	}

	/**
	 * Returns a stream containing the initial contents to be given to new file
	 * resource instances. <b>Subclasses</b> may wish to override. This default
	 * implementation provides no initial contents.
	 * 
	 * @return initial contents to be given to new file resource instances
	 */
	@Override
	protected InputStream getInitialContents() {
		String content =":- module(" + getFileName().substring(0, getFileName().length()-3) + ",[\n\t\t\n\t]).\n";
		return new ByteArrayInputStream(content.getBytes());
	}

}


