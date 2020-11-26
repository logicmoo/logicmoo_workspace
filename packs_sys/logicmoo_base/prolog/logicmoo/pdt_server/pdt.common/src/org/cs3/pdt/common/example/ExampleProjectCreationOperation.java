/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.example;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.dialogs.IOverwriteQuery;

public class ExampleProjectCreationOperation implements IRunnableWithProgress {

	private IResource elementToOpen;

	private String urlToOpen;

	public String getUrlToOpen() {
		return urlToOpen;
	}

	public IResource getFileToOpen() {
		return elementToOpen;
	}

	private IOverwriteQuery overwriteQuery;

	private ExampleProjectCreationWizardPage[] pages;

	/**
	 * Constructor for ExampleProjectCreationOperation
	 */
	public ExampleProjectCreationOperation(ExampleProjectCreationWizardPage[] myPages, IOverwriteQuery myOverwriteQuery) {
		elementToOpen = null;
		pages = myPages;
		overwriteQuery = myOverwriteQuery;
	}


	/*
	 * @see IRunnableWithProgress#run(IProgressMonitor)
	 */
	@Override
	public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		try {
			int countExamples = 0, countDownloads = 0;
			for (ExampleProject e : pages[0].getSelectedExampleProjects())
			{
					countExamples++;
					if (e.downloadAvailable() && (e.needsDownload() || e.needsUpdate())) {
						countDownloads++;
					}
			}
			
			monitor.beginTask("Example Project Download", countDownloads);

			for (ExampleProject e : pages[0].getSelectedExampleProjects())
			{
					if (e.downloadAvailable() && (e.needsDownload() || e.needsUpdate()))
					{
						e.download(new SubProgressMonitor(monitor, 1));
					}
			}
			
			monitor.beginTask("Example Project Creation", countExamples);
			
			for (ExampleProject e : pages[0].getSelectedExampleProjects())
			{
					// ... create it
					e.create(new SubProgressMonitor(monitor, 1), overwriteQuery);
			}

		}
		finally {
			monitor.done();
		}
	}
}
