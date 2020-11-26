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

package org.cs3.pdt.common.queries;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class GlobalDefinitionsSearchQuery extends PDTSearchQuery {
	
	public GlobalDefinitionsSearchQuery(String goal, String searchGoalLabel, boolean isExactMatch) {
		super(goal, searchGoalLabel, isExactMatch);
		if (isExactMatch) {
			setSearchType("Definitions and declarations of");
		} else {
			setSearchType("Definitions and declarations containing");			
		}
	}

	@Override
	protected String buildSearchQuery() {
		String query = bT(PDTCommonPredicates.FIND_PREDICATE_DEFINITIONS,
				getGoal(),
				Boolean.toString(isExactMatch()),
				getProjectPath() == null ? "_" : getProjectPath(),
				"DefiningModule",
				"Functor",
				"Arity",
				"DeclOrDef",
				"File",
				"Line",
				"PropertyList");
		return query;
	}

	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String definingModule = m.get("DefiningModule").toString();
		String functor = m.get("Functor").toString();
		int arity=-1;
		try {
			arity = Integer.parseInt(m.get("Arity").toString());
		} catch (NumberFormatException e) {}
		
		IFile file = findFile(m.get("File").toString());
		String offsetOrLine = m.get("Line").toString();

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		String declOrDef = m.get("DeclOrDef").toString();

		Match match;
		if (getProject() != null && (file == null || !getProject().equals(file.getProject()))) {
			return null;
		}
		if (file == null) {
			match = createUniqueMatch(definingModule, functor, arity, properties, "", declOrDef);
		} else {
			if (offsetOrLine.indexOf("-") >= 0) {
				String[] positions = offsetOrLine.split("-");
				int offset = Integer.parseInt(positions[0]);
				int length = Integer.parseInt(positions[1]) - offset;
				match = createUniqueMatch(PROLOG_MATCH_KIND_DEFINITION, definingModule, functor, arity, file, offset, length, properties, "", declOrDef);
			} else {
				int line = Integer.parseInt(offsetOrLine);
				match = createUniqueMatch(PROLOG_MATCH_KIND_DEFINITION, definingModule, functor, arity, file, line, properties, "", declOrDef);
			}
		}
		
		return match;
	}
	
}


