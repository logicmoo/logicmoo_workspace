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
import org.cs3.pdt.common.search.PrologSearchResult;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

/**
 * @author gk
 *
 */
public class ReferencesSearchQueryDirect extends PDTSearchQuery {

	private String updatedContextDependentGoal;
	
	public ReferencesSearchQueryDirect(String goal, String searchGoalLabel, boolean isExactMatch) {
		super(goal, searchGoalLabel, isExactMatch);
		if (isExactMatch) {
			setSearchType("References to");
		} else {
			setSearchType("References to predicates containing");			
		}
	}
	
	public ReferencesSearchQueryDirect(Goal goal) {
		super(PDTSearchQuery.toPredicateGoal(goal), PDTCommonUtil.cropText(goal.getTermString(), 50), true);
		setSearchType("References to");
		String filePath = QueryUtils.quoteAtomIfNeeded(goal.getFilePath());
		int line = goal.getLine();
		updateContextDependentGoal(filePath, line, goal);
	}

	private void updateContextDependentGoal(String filePath, int line, Goal editorGoal) {
		String query = bT(PDTCommonPredicates.UPDATE_PREDICATE_REFERENCE_SEARCH_TERM_TO_CONTEXT,
				getGoal(),
				filePath == null ? "_" : filePath,
				line == -1 ? "_" : Integer.toString(line),
				"NewModule",
				"NewGoal");
		try {
			PrologProcess process = PDTCommonUtil.getActivePrologProcess();
			Map<String, Object> result = process.queryOnce(query);
			updatedContextDependentGoal = (String) result.get("NewGoal");
			String newModule = (String) result.get("NewModule");
			if (newModule != null) {
				PrologSearchResult prologSearchResult = (PrologSearchResult) getSearchResult();
				String newLabel;
				String editorTermString = editorGoal.getTermString();
				String editorModule = editorGoal.getModule();
				if (editorModule == null) {
					newLabel = newModule + ":" + editorTermString;
				} else {
					newLabel = newModule + ":" + editorTermString.substring(editorTermString.indexOf(editorModule) + editorModule.length() + 1);
				}
				prologSearchResult.setSearchGoalLabel(PDTCommonUtil.cropText(newLabel, 50));
			}
		} catch (Exception e) {
			Debug.report(e);
		}
	}

	@Override
	protected String buildSearchQuery() {
		String query = bT(PDTCommonPredicates.FIND_PREDICATE_REFERENCE,
				updatedContextDependentGoal != null ? updatedContextDependentGoal : getGoal(),
				Boolean.toString(isExactMatch()),
				getProjectPath() == null ? "_" : getProjectPath(),
				"RefModule",
				"RefName",
				"RefArity",
				"RefFile",
				"RefLine",
				"PropertyList");
		return query;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {

		String module = m.get("RefModule").toString();
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		
		IFile file = findFile(m.get("RefFile").toString());
		if (getProject() != null && !getProject().equals(file.getProject())) {
			return null;
		}
		
		String offsetOrLine = m.get("RefLine").toString();
		
		Match match = null;
		
		String name = (String) m.get("RefName");
		int arity = -1;
		try {
			// if RefArity is not bound queryAllAtOnce returns a generated variable name as value instead of null
			// thus the check for null cannot be used, instead it is checked for a positive integer value 
			arity = Integer.parseInt(m.get("RefArity").toString());
		} catch (Exception e) {
		}
		if (name != null && arity >= 0) {
			if (offsetOrLine.indexOf("-") >= 0) {
				String[] positions = offsetOrLine.split("-");
				int offset = Integer.parseInt(positions[0]);
				int length = Integer.parseInt(positions[1]) - offset;
				match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, offset, length, properties, null, "definition");
			} else {
				int line = Integer.parseInt(offsetOrLine);
				match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, line, properties, null, "definition");
			}
		} else {
			if (offsetOrLine.indexOf("-") >= 0) {
				String[] positions = offsetOrLine.split("-");
				int offset = Integer.parseInt(positions[0]);
				int length = Integer.parseInt(positions[1]) - offset;
				match = createUniqueMatch(module, file, offset, length, properties);
			} else {
				int line = Integer.parseInt(offsetOrLine);
				match = createUniqueMatch(module, file, line, properties);
			}
		}
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}


