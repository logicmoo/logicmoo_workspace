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
import java.io.IOException;

import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.graphicalviews.PDTGraphPredicates;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.prolog.connector.common.QueryUtils;
import org.eclipse.core.resources.IProject;

public class DependenciesGraphProcessLoader extends GlobalGraphProcessLoader {
	
	private static final String NAME_OF_DEPENDENCIES_HELPING_FILE = "pdt-dependencies-help.graphml";
	
	public DependenciesGraphProcessLoader(PDTGraphView view) {
		super(view, NAME_OF_DEPENDENCIES_HELPING_FILE);
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		try {
			loadPaths(currentPath);

			IProject project = FileUtils.findFileForLocation(currentPath).getProject();
			String projectPath = QueryUtils.normalizeOnWindows(project.getLocation().toString());
			
			String query;
			query = bT(PDTGraphPredicates.WRITE_DEPENDENCIES_TO_GRAPHML, paths.toString(), QueryUtils.quoteAtom(projectPath), QueryUtils.prologFileNameQuoted(helpFile));
			return query;
			
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	protected boolean ignoreExternalPrologFilesProject() {
		return false;
	}
	
}
