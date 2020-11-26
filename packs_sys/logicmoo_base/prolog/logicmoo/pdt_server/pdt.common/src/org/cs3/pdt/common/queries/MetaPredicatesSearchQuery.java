/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.queries;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.search.ui.text.Match;

public class MetaPredicatesSearchQuery extends MarkerCreatingSearchQuery {
	
	private static final String ATTRIBUTE = "pdt.meta.predicate";
	private static final String SMELL_NAME = "PDT_Quickfix";
	private static final String QUICKFIX_DESCRIPTION = "PDT_QuickfixDescription";
	private static final String QUICKFIX_ACTION = "PDT_QuickfixAction";
	
	public MetaPredicatesSearchQuery(boolean createMarkers) {
		super(createMarkers, ATTRIBUTE, ATTRIBUTE);
	}

	@Override
	public void setProjectScope(IProject project) {
		super.setProjectScope(project);
		if (project == null) {
			setSearchType("Undeclared meta predicates");
		} else {
			setSearchType("Undeclared meta predicates in project " + project.getName());
		}
	}

	@Override
	protected String buildSearchQuery() {
		return bT(PDTCommonPredicates.FIND_UNDECLARED_META_PREDICATE,
				getProjectPath() == null ? "_" : getProjectPath(),
				"Module",
				"Name",
				"Arity",
				"MetaSpec",
				"MetaSpecAtom",
				"File",
				"Line",
				"PropertyList",
				"Directive");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String definingModule = m.get("Module").toString();
		String functor = m.get("Name").toString();
		int arity=-1;
		try {
			arity = Integer.parseInt(m.get("Arity").toString());
		} catch (NumberFormatException e) {}
		
		IFile file = findFile(m.get("File").toString());
		
		if (getProject() != null && !getProject().equals(file.getProject())) {
			return null;
		}

		int line = Integer.parseInt(m.get("Line").toString());

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		Match match = createUniqueMatch(PROLOG_MATCH_KIND_DEFAULT, definingModule, functor, arity, file, line, properties, "", "definition");
		
		if (createMarkers && match != null) {
			try {
				String metaSpec = m.get("MetaSpecAtom").toString();
				IMarker marker = createMarker(file, "Meta predicate: " + metaSpec, line);
				marker.setAttribute(SMELL_NAME, "Meta Predicate");
				marker.setAttribute(QUICKFIX_ACTION, m.get("Directive").toString());
				marker.setAttribute(QUICKFIX_DESCRIPTION, "Declare meta predicate");
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return match;
	}
	
}
