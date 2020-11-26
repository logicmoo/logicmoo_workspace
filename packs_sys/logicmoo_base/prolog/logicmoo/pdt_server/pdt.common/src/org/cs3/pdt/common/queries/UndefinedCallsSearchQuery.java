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
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.text.Match;

public class UndefinedCallsSearchQuery extends MarkerCreatingSearchQuery {
	
	private static final String ATTRIBUTE = "pdt.undefined.call";
	
	private boolean includeSomeExecutionContext;
	
	public UndefinedCallsSearchQuery(boolean createMarkers, boolean includeSomeExecutionContext) {
		super(createMarkers, ATTRIBUTE, ATTRIBUTE);
		this.includeSomeExecutionContext = includeSomeExecutionContext;
	}

	@Override
	public void setProjectScope(IProject project) {
		super.setProjectScope(project);
		if (project == null) {
			setSearchType("Undefined calls");
		} else {
			setSearchType("Undefined calls in project " + project.getName());
		}
	}

	@Override
	protected String buildSearchQuery() {
		return bT(PDTCommonPredicates.FIND_UNDEFINED_CALL,
				getProjectPath() == null ? "_" : getProjectPath(),
				Boolean.toString(includeSomeExecutionContext),
				"Module",
				"Name",
				"Arity",
				"File",
				"Start",
				"End",
				"UndefName",
				"UndefArity",
				"PropertyList");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String module = m.get("Module").toString();
		String name = m.get("Name").toString();
		int arity = Integer.parseInt(m.get("Arity").toString());
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		IFile file = findFile(m.get("File").toString());
		
		if (getProject() != null && !getProject().equals(file.getProject())) {
			return null;
		}

		int offset = Integer.parseInt(m.get("Start").toString());
		int end = Integer.parseInt(m.get("End").toString());
		PrologMatch match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, offset, end - offset, properties, null, "definition");
		if (createMarkers && match != null) {
			try {
				IDocument document = UIUtils.getDocument(file);
				offset = UIUtils.logicalToPhysicalOffset(document, offset);
				end = UIUtils.logicalToPhysicalOffset(document, end);
				createMarker(file, "Undefined call: " + m.get("UndefName") + "/" + m.get("UndefArity") + " is not defined", offset, end);
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return match;
	}
	
}
