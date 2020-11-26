/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
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

import java.io.IOException;
import java.util.Stack;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public class ExternalPrologFilesProjectUtils {
	
	private static IProject externalPrologFilesProject;
	private static final String name = "External Prolog Files";
	
	public static synchronized IProject getExternalPrologFilesProject() throws CoreException {
		if (externalPrologFilesProject == null) {
			externalPrologFilesProject = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
		}
		return externalPrologFilesProject;
	}

	public static IProject getExternalPrologFilesProjectEnsureAccessible() throws CoreException {
		IProject project = getExternalPrologFilesProject();
		if (!project.exists()) {
			project.create(null);
		}
		if (!project.isOpen()) {
			project.open(null);
		}
		return project;
	}
	
	public static IFile linkFile(String fileName) throws CoreException {
		return linkFile(new Path(fileName));
	}

	public static IFile linkFile(IPath path) throws CoreException {
		if (!path.toFile().exists()) {
			return null;
		}
		IProject project = getExternalPrologFilesProjectEnsureAccessible();
		IPath pathWithoutDevice;
		try {
			pathWithoutDevice = new Path(path.toFile().getCanonicalPath()).setDevice(null);
		} catch (IOException e) {
			pathWithoutDevice = path.setDevice(null);
		}
		IFile file = project.getFile(pathWithoutDevice);
		if (!file.exists()) {
			createParentsIfNecessary(file);
			file.createLink(path, IResource.BACKGROUND_REFRESH, null);
		}
		return file;
	}
	
	private static void createParentsIfNecessary(IFile folder) {
		IResource parent = folder.getParent();
		Stack<IResource> nonExistentParents = new Stack<IResource>();
		while (!parent.exists()) {
			nonExistentParents.push(parent);
			parent = parent.getParent();
		}
		if (nonExistentParents.size() > 0) {
			while (!nonExistentParents.isEmpty()) {
				IResource element = nonExistentParents.pop();
				if (element instanceof IFolder) {
					try {
						((IFolder) element).create(true, true, null);
					} catch (CoreException e) {
					}
				}
			}
		}
	}
	
	public static boolean isExternalFile(IFile file) {
		IProject project;
		try {
			project = getExternalPrologFilesProject();
		} catch (CoreException e) {
			return false;
		}
		return project.isAccessible() && project.equals(file.getProject());
	}
}


