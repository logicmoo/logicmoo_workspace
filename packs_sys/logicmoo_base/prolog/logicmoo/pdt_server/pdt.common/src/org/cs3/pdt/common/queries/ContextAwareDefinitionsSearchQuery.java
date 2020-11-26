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
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.prolog.connector.common.QueryUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class ContextAwareDefinitionsSearchQuery extends PDTSearchQuery {

	private String filePath;
	private int line;
	
	public ContextAwareDefinitionsSearchQuery(Goal goal) {
		super(PDTSearchQuery.toPredicateGoal(goal), PDTCommonUtil.cropText(goal.getTermString(), 50), true);
		setSearchType("Definitions and declarations of");
		filePath = QueryUtils.quoteAtomIfNeeded(goal.getFilePath());
		line = goal.getLine();
	}

	@Override
	protected String buildSearchQuery() {
//		String file = Util.quoteAtom(goal.getFilePath());
//		if (goal.getFilePath().isEmpty())
//			file = "OrigFile";
//
//		String module2 = module;
//		if (module.equals("''"))
//			module2 = "Module";
//		
//		String term = goal.getTermString();
//		
		String query = bT(PDTCommonPredicates.FIND_CATEGORIZED_PREDICATE_DEFINITIONS,
				getGoal(),
				filePath,
				line,
				"Functor",
				"Arity",
				"DeclOrDef",
				"DefiningModule",
				"File",
				"Line",
				"PropertyList",
				"Visibility");
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
		String visibility = m.get("Visibility").toString();

		Match match;
		if (file == null) {
			match = createUniqueMatch(definingModule, functor, arity, properties, visibility, declOrDef);
		} else {
			if (offsetOrLine.indexOf("-") >= 0) {
				String[] positions = offsetOrLine.split("-");
				int offset = Integer.parseInt(positions[0]);
				int length = Integer.parseInt(positions[1]) - offset;
				match = createUniqueMatch(PROLOG_MATCH_KIND_DEFINITION, definingModule, functor, arity, file, offset, length, properties, visibility, declOrDef);
			} else {
				int line = Integer.parseInt(offsetOrLine);
				match = createUniqueMatch(PROLOG_MATCH_KIND_DEFINITION, definingModule, functor, arity, file, line, properties, visibility, declOrDef);
			}
		}
		
		return match;
	}
	
}


