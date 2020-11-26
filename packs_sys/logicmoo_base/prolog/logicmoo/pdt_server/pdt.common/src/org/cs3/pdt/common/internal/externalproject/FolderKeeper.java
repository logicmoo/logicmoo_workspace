/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.internal.externalproject;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.connector.util.ExternalPrologFilesProjectUtils;
import org.cs3.pdt.connector.util.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class FolderKeeper {

	private static final List<String> FILE_EXTENSIONS = new ArrayList<String>();

	public static QualifiedName REAL_LOCATION = new QualifiedName(
			"org.cs3.pdt.externalproject", "real.location");

	private static String JOB_NAME = "External Prolog Files";

	static {
		FILE_EXTENSIONS.add("pl");
		FILE_EXTENSIONS.add("plt");
		FILE_EXTENSIONS.add("pro");
		FILE_EXTENSIONS.add("lgt");
		FILE_EXTENSIONS.add("logtalk");
	}

	public static List<String> getFileExtensions() {
		return FILE_EXTENSIONS;
	}

	public FolderKeeper() {
	}

	public void clearProject(final IProject project) {
		Job j = new Job(JOB_NAME) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				clearProjectImpl(project);
				return Status.OK_STATUS;
			}
		};
		try {
			j.setRule(ExternalPrologFilesProjectUtils
					.getExternalPrologFilesProjectEnsureAccessible());
		} catch (CoreException e) {
			e.printStackTrace();
			return;
		}
		j.schedule();
	}

	private void clearProjectImpl(IProject project) {
		clearFolder(project);
		try {
			for (IResource resource : project.members()) {
				if (resource instanceof IFolder) {
					deepClearAndListAll((IFolder) resource, false);
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	public void clearAndListAll(IFolder folder) {
		clearAndListAll(folder, true);
	}

	public void clearAndListAll(final IFolder folder, final boolean doList) {
		Job j = new Job(JOB_NAME) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				clearAndListAllImpl(folder, doList);
				return Status.OK_STATUS;
			}
		};
		try {
			j.setRule(ExternalPrologFilesProjectUtils.getExternalPrologFilesProjectEnsureAccessible());
		} catch (CoreException e) {
			e.printStackTrace();
			return;
		}
		j.schedule();
	}

	private void clearAndListAllImpl(IFolder folder, boolean doList) {
		clearFolder(folder);
		if (doList) {
			listFiles(folder);
		}
	}

	public void deepClearAndListAll(IFolder folder) {
		deepClearAndListAll(folder, true);
	}

	public void deepClearAndListAll(final IFolder folder, final boolean doList) {
		Job j = new Job(JOB_NAME) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				deepClearAndListAllImpl(folder, doList);
				return Status.OK_STATUS;
			}
		};
		try {
			j.setRule(ExternalPrologFilesProjectUtils.getExternalPrologFilesProjectEnsureAccessible());
		} catch (CoreException e) {
			e.printStackTrace();
			return;
		}
		j.schedule();
	}
	
	private void deepClearAndListAllImpl(IFolder folder, boolean doList) {
		clearAndListAllImpl(folder, doList);
		try {
			for (IResource member : folder.members()) {
				if (member instanceof IFolder) {
					IFolder memberFolder = (IFolder) member;
					deepClearAndListAllImpl(memberFolder, doList);
					if (memberFolder.members().length == 0) {
						memberFolder.delete(false, new NullProgressMonitor());
					}
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private void clearFolder(IContainer folder) {
		try {
			for (IResource member : folder.members()) {
				if (member instanceof IFile) {
					IFile file = (IFile) member;
					if (file.isLinked()
							&& !file.getLocation().toFile().exists()) {
						file.delete(false, new NullProgressMonitor());
					}
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private void listFiles(IFolder folder) {
		IPath location = getLocation(folder);
		if (location == null) {
			return;
		}
		File folderFile = location.toFile();
		if (!folderFile.exists()) {
			return;
		}
		for (File child : folderFile.listFiles()) {
			if (child.isFile() && acceptFile(child)) {
				try {
					FileUtils.findFileForLocation(child.getAbsolutePath());
				} catch (IOException e) {
					e.printStackTrace();
				}
			} else if (child.isDirectory()) {
				IFolder childFolder = folder.getFolder(child.getName());
				if (!childFolder.exists()) {
					try {
						childFolder.create(true, true,
								new NullProgressMonitor());
						childFolder.setPersistentProperty(REAL_LOCATION,
								child.getAbsolutePath());
					} catch (CoreException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}

	private IPath getLocation(IFolder folder) {
		String realLocation = null;
		try {
			realLocation = folder.getPersistentProperty(REAL_LOCATION);
		} catch (CoreException e) {
			e.printStackTrace();
		}
		if (realLocation != null) {
			return new Path(realLocation);
		}
		try {
			for (IResource member : folder.members()) {
				if (member instanceof IFile) {
					return member.getLocation().removeLastSegments(1);
				} else if (member instanceof IFolder) {
					IPath memberLocation = getLocation((IFolder) member);
					if (memberLocation != null) {
						return memberLocation.removeLastSegments(1);
					}
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
		return null;
	}

	private boolean acceptFile(File file) {
		if (file.isFile()) {
			return getFileExtensions().contains(getFileExtension(file));
		} else {
			return false;
		}
	}

	private String getFileExtension(File file) {
		int dotPosition = file.getName().lastIndexOf(".");
		if (dotPosition == -1) {
			return null;
		} else {
			return file.getName().substring(dotPosition + 1);
		}
	}

}
