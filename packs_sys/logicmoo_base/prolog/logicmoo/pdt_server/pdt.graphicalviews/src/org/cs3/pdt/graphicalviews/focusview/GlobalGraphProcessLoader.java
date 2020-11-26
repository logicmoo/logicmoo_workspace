/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.graphicalviews.focusview;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.connector.util.ExternalPrologFilesProjectUtils;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.graphicalviews.PDTGraphPredicates;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.prolog.connector.common.QueryUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

public class GlobalGraphProcessLoader extends GraphProcessLoaderBase {
	
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";
	
	private static final ArrayList<String> extensions = new ArrayList<String>();
	static {
		extensions.add("pl");
		extensions.add("prolog");
		extensions.add("lgt");
		extensions.add("logtalk");
	}
	
	protected List<String> paths = new ArrayList<String>();
	protected String currentPath;

	public GlobalGraphProcessLoader(PDTGraphView view) {
		super(view, NAME_OF_GLOBAL_HELPING_FILE);
	}
	
	protected GlobalGraphProcessLoader(PDTGraphView view, String helpFileName) {
		super(view, helpFileName);
	}
	
	@Override
	public String getCurrentPath() {
		return currentPath;
	}
	
	@Override
	public void setCurrentPath(String currentPath) {
		this.currentPath = currentPath;
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		loadPaths(currentPath);
		
		String query;
		query = bT(PDTGraphPredicates.WRITE_GLOBAL_TO_GRAPHML, paths.toString(), QueryUtils.prologFileNameQuoted(helpFile), getSettings());
		return query;
	}

	protected void loadPaths(String path) {
		paths = getFilePaths(path);
	}

	public List<String> getFilePaths(String path) {
		final List<String> paths = new ArrayList<String>();
		try {			
			IProject project = FileUtils.findFileForLocation(path).getProject();
			
			if (ignoreExternalPrologFilesProject()
					&& ExternalPrologFilesProjectUtils.getExternalPrologFilesProject().isAccessible() 
					&& ExternalPrologFilesProjectUtils.getExternalPrologFilesProject().equals(project)) {
				return paths;
			}
			
			project.accept(new IResourceVisitor() {

				@Override
				public boolean visit(IResource resource) throws CoreException {
					if (!(resource instanceof IFile)) 
						return true;
					IFile file = (IFile)resource;
					if (file.getFileExtension() != null && extensions.contains(file.getFileExtension())) {
						paths.add(QueryUtils.quoteAtom(FileUtils.prologFileName(file)));
					}
					return false;
				}
				
			});
		} catch (Exception e) {
			e.printStackTrace();
		}
		return paths;
	}
	
	public boolean containsFilePath(String path) {
		return paths.contains(path);
	}
	
	protected boolean ignoreExternalPrologFilesProject() {
		return true;
	}
	
}
