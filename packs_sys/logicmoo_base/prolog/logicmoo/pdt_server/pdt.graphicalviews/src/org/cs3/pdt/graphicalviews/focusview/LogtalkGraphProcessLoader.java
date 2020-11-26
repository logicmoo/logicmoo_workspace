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
import static org.cs3.prolog.connector.common.QueryUtils.prologFileNameQuoted;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.graphicalviews.PDTGraphPredicates;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.eclipse.core.resources.IProject;

public class LogtalkGraphProcessLoader extends GlobalGraphProcessLoader {
	
	private static final String NAME_OF_DEPENDENCIES_HELPING_FILE = "pdt-logtalk-entity-help.graphml";
	private LogtalkView focusView;
	
	public LogtalkGraphProcessLoader(PDTGraphView view, LogtalkView focusView) {
		super(view, NAME_OF_DEPENDENCIES_HELPING_FILE);
		this.focusView = focusView;
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		try {
			String query;
			
			String diagramEntity = focusView.getDiagramType().getDiagramEntity();
			switch (focusView.getInputType()) {
			case PROJECT:
				IProject project = FileUtils.findFileForLocation(currentPath).getProject();
				String projectName = project.getName();
				loadPaths(currentPath);
				query = bT(PDTGraphPredicates.WRITE_LOGTALK_PROJECT_FILES_TO_GRAPHML,
						diagramEntity,
						paths.toString(),
						quoteAtom(projectName),
						prologFileNameQuoted(helpFile));
				break;
			case LIBRARY:
				query = bT(PDTGraphPredicates.WRITE_LOGTALK_LIBRARY_TO_GRAPHML,
						diagramEntity,
						quoteAtom(focusView.getCurrentLibrary()),
						prologFileNameQuoted(helpFile));
				break;
			case RECURSIVE_LIBRARY:
				query = bT(PDTGraphPredicates.WRITE_LOGTALK_RECURSIVE_LIBRARY_TO_GRAPHML,
						diagramEntity,
						quoteAtom(focusView.getCurrentLibrary()),
						prologFileNameQuoted(helpFile));
				break;
			default:
				return "true";
			}
			return query;
			
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	@Override
	protected boolean ignoreExternalPrologFilesProject() {
		return false;
	}
	
}
