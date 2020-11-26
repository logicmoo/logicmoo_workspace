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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.cs3.pdt.common.PDTCommonUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.eclipse.ui.wizards.datatransfer.ZipFileStructureProvider;

public class ExampleProject {

	
	public static final String relativePath = "jt_examples";
	
	private String name, description;

	private String projectName, fileName, url;
	
	private boolean needsUpdate, needsDownload, downloadAvailable;

	private String[] treePath;

	
	public ExampleProject(String name, String projectName, String fileName, String description, String url) {
		this(name, projectName, fileName, description, url, new String[0]);
	}
	
	public ExampleProject(String name, String projectName, String fileName, String description, String url, String[] treePath)
	{
		setName(name);
		this.description = description;
		this.fileName = fileName;
		this.projectName = projectName;
		this.url = url;
		this.treePath = treePath;
		checkAvailable();
		
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}
	
	public boolean needsDownload()
	{
		return needsDownload;
	}
	
	public boolean needsUpdate()
	{
		return needsUpdate;
	}
	
	public boolean downloadAvailable()
	{
		return downloadAvailable;
	}
	
	private File getPath() {
		File tempDir = new File(System.getProperty("java.io.tmpdir"));
		return new File(tempDir, relativePath);
	}
	
	private void checkAvailable() {
	
		File verz = getPath();
		
		File f = new File(verz, fileName);
		
		int externFileSize = -1;
		try {
			externFileSize = PDTCommonUtil.getFilesizeFromUrl(url);
		} catch (Exception e) {
		}

		/*
		 * Wenn die Dateigröße korrekt ausgelesen wurde,
		 * sollte der Download verfügbar sein
		 */
		downloadAvailable = (externFileSize > -1);
		
		if (f.exists())
		{
			// Datei existiert, neuer Download nicht nötig
			needsDownload = false;
			
			/* Aber evtl. ein Update
			 *  (falls Dateien unterschiedlich groß sind) */
			needsUpdate = (externFileSize != f.length());
		}
		else
		{
			needsDownload = true;
		}
		
	}

	public void create( IProgressMonitor monitorIProject, IOverwriteQuery overwriteQuery ) {
		// create example project in the workspace
		
		try {
			String calculatedProjectName = calculateInitialProjectName();
			IProject project = configNewProject(ResourcesPlugin.getWorkspace().getRoot(), calculatedProjectName, monitorIProject);

			IPath destPath = project.getFullPath();

			File file = new File(getPath() + "/" + fileName);
			ZipFile zipFile = null;
				
			try {
				zipFile = new ZipFile(file);
			} catch (ZipException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}

			if (zipFile != null) {
				if (monitorIProject == null) {
					importFilesFromZip(zipFile, destPath, null, overwriteQuery, calculatedProjectName);
				} else {
					importFilesFromZip(zipFile, destPath, new SubProgressMonitor(monitorIProject, 1), overwriteQuery, calculatedProjectName);
				}
				zipFile.close();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}


	private void importFilesFromZip(ZipFile srcZipFile, IPath destPath, IProgressMonitor monitor, IOverwriteQuery overwriteQuery, String projectName) throws InvocationTargetException, InterruptedException {
		ZipFileStructureProvider structureProvider = new ZipFileStructureProvider(srcZipFile);
		ImportOperation op = new ImportOperation(destPath, structureProvider.getRoot(), structureProvider, overwriteQuery);
		op.run(monitor); 
		
	}
	
	private IProject configNewProject(IWorkspaceRoot root, String name, IProgressMonitor monitor) throws InvocationTargetException {
		try {
			IProject project = root.getProject(name);
			if (!project.exists()) {
				project.create(null);
			}
			if (!project.isOpen()) {
				project.open(null);
			}
			IProjectDescription desc = project.getDescription();
			desc.setLocation(null);
			if (monitor == null) {
				project.setDescription(desc, null);				
			} else {
				project.setDescription(desc, new SubProgressMonitor(monitor, 1));
			}
	
			return project;
		}
		catch (CoreException e) {
			throw new InvocationTargetException(e);
		}
	}

	protected String calculateInitialProjectName() {
		IProject projectHandle = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		if (!projectHandle.exists()) {
			return projectName;
		}
		// Change the name until it doesn't exists. Try many times and then
		// give up.
		for (int i = 1; i < 10000; i++) {
			projectHandle = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName + "_" + i);
			if (!projectHandle.exists()) {
				return projectName + "_" + i;
			}
		}
		return projectName + "_99999"; //$NON-NLS-1$
	}

	public boolean download(SubProgressMonitor monitor) {
		
		File verz = getPath();
		File targetFile = new File(verz, fileName);
		
		// Backup File anlegen und Zieldatei löschen
		File backupFile = new File(verz + "/" + fileName + ".bak");
		if (backupFile.exists())
			backupFile.delete();
		targetFile.renameTo(backupFile);
		
		// Projekt unter der angegeben url runterladen und unter filename abspeichern
		if (PDTCommonUtil.saveUrlToFile(url, targetFile, monitor))
		{
			backupFile.delete();
		} else
		{
			if (!targetFile.exists())
			{
				backupFile.renameTo(targetFile);
			}
		}
		
		return false;
	}
	
	public String[] getTreePath() {
		return treePath;
	}
}
