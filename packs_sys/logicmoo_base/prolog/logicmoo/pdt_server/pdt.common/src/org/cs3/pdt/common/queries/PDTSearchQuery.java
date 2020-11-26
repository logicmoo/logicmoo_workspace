/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.text.BadLocationException;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.search.PrologSearchResult;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.common.structureElements.PredicateMatch;
import org.cs3.pdt.common.structureElements.PrologDefinitionMatch;
import org.cs3.pdt.common.structureElements.DirectiveMatch;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.common.structureElements.PrologReferenceMatch;
import org.cs3.pdt.common.structureElements.SearchDirectiveElement;
import org.cs3.pdt.common.structureElements.SearchMatchElement;
import org.cs3.pdt.common.structureElements.SearchPredicateElement;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;

public abstract class PDTSearchQuery implements ISearchQuery {
	
	protected final int PROLOG_MATCH_KIND_DEFAULT = 0;
	protected final int PROLOG_MATCH_KIND_DEFINITION = 1;
	protected final int PROLOG_MATCH_KIND_REFERENCE = 2;

	private String goal;
	private String searchGoalLabel;
	private boolean isExactMatch;

	private PrologSearchResult result;
	private LinkedHashMap<String, SearchMatchElement> matchElements = new LinkedHashMap<String, SearchMatchElement>();
	private LinkedHashMap<String, SearchPredicateElement> predicateElements = new LinkedHashMap<String, SearchPredicateElement>();
	private LinkedHashMap<String, SearchDirectiveElement> directiveElements = new LinkedHashMap<>();
	private IProject project;
	private String projectPath;

	public PDTSearchQuery(String goal, String searchGoalLabel) {
		this(goal, searchGoalLabel, true);
	}
	
	public PDTSearchQuery(String goal, String searchGoalLabel, boolean isExactMatch) {
		this.goal = goal;
		this.searchGoalLabel = searchGoalLabel;
		this.isExactMatch = isExactMatch;
		result = new PrologSearchResult(this, searchGoalLabel);
	}
	
	public String getGoal() {
		return goal;
	}
	
	public boolean isExactMatch() {
		return isExactMatch;
	}
    
	// Adapt the text in the header of the search result view:
	protected void setSearchType(String s)  {
		result.setSearchType(s);
	}
	
	@Override
	public IStatus run(IProgressMonitor monitor) {
		try {
			return run_impl(monitor);
		} catch (Throwable t) {
			Debug.report(t);
			return new Status(IStatus.ERROR, PDTCommonPlugin.PLUGIN_ID, 42, "Exception caught during search.", t);
		}
	}
	
	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException, PrologException, PrologProcessException {
		result.removeAll();

		return doSearch(monitor); 
	}

	/**
	 * @return
	 * @throws PrologProcessException
	 * @throws PrologException
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private IStatus doSearch(IProgressMonitor monitor) throws PrologProcessException, PrologException, IOException, NumberFormatException {
		PrologSession session = PDTCommonUtil.getActivePrologProcess().getSession();
		monitor.beginTask("Searching...", 2);
		monitor.subTask("Running Prolog query");
		List<Map<String, Object>> results = findReferencedClauses(session, new SubProgressMonitor(monitor, 1));
		monitor.subTask("Processing results");
		processFoundClauses(results, new SubProgressMonitor(monitor, 1));
		monitor.done();
		return Status.OK_STATUS;
	}

	/**
	 * @param session
	 * @return
	 * @throws PrologException
	 * @throws PrologProcessException
	 */
	private List<Map<String, Object>> findReferencedClauses(PrologSession session, IProgressMonitor monitor)
			throws PrologException, PrologProcessException {
		
		monitor.beginTask("", 1);
//		String module;               
//		if (goal.getModule() != null && !goal.getModule().isEmpty()) {
//			module = Util.quoteAtomIfNeeded(goal.getModule());
//		} else {
//			module = "Module";                  // Modul is free variable
//		}

		String query = buildSearchQuery();
		
		List<Map<String, Object>> clauses = getResultForQuery(session, query);
		
		// Bindung der Modulvariablen aus vorheriger Query abfragen und im Goal setzen.
//		if (clauses.size() > 0 && goal.getModule() == null){
//			goal.setModule(clauses.get(0).get("Module").toString());
//		}
		monitor.done();
		return clauses;
	}
	
	abstract protected String buildSearchQuery();

	protected List<Map<String, Object>> getResultForQuery(PrologSession session, String query) 
			throws PrologProcessException {
		Debug.info(query);
				
		List<Map<String, Object>> clauses = session.queryAll(query);
        return clauses;
	}

	/**
	 * @param clauses
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private void processFoundClauses(List<Map<String, Object>> clauses, IProgressMonitor monitor)
	throws IOException, NumberFormatException {
		Match match;
		matchElements.clear();
		predicateElements.clear();
		directiveElements.clear();
		monitor.beginTask("", clauses.size());
		for (Iterator<Map<String,Object>> iterator = clauses.iterator(); iterator.hasNext();) {
			Map<String,Object> m = iterator.next();
			Debug.info(m.toString());
			match = constructPrologMatchForAResult(m);
			if ((result != null) && (match != null)) {
				result.addMatch(match);
			}
			monitor.worked(1);
			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}
		}
		monitor.done();
	}
	
	protected abstract Match constructPrologMatchForAResult(Map<String,Object> m) throws IOException;

	protected PrologMatch createUniqueMatch(int matchKind, String definingModule, String functor, int arity, IFile file, int line, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + line + file;
		SearchMatchElement searchMatchElement = matchElements.get(signature);
		if (searchMatchElement == null) {
			searchMatchElement = new SearchMatchElement();
			matchElements.put(signature, searchMatchElement);
		}
		PrologMatch match;
		switch (matchKind) {
		case PROLOG_MATCH_KIND_DEFINITION:
			match = new PrologDefinitionMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, line, declOrDef, signature);
			break;
		case PROLOG_MATCH_KIND_REFERENCE:
			match = new PrologReferenceMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, line, declOrDef, signature);
			break;
		case PROLOG_MATCH_KIND_DEFAULT:
		default:
			match = new PrologMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, line, declOrDef, signature);
			break;
		}
		match.createLabel();
		return match;
	}

	protected PrologMatch createUniqueMatch(int matchKind, String definingModule, String functor, int arity, IFile file, int offset, int length, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + offset + "#" + length + file;
		SearchMatchElement searchMatchElement = matchElements.get(signature);
		if (searchMatchElement == null) {
			searchMatchElement = new SearchMatchElement();
			matchElements.put(signature, searchMatchElement);
		}
		PrologMatch match;
		switch (matchKind) {
		case PROLOG_MATCH_KIND_DEFINITION:
			match = new PrologDefinitionMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, offset, length, declOrDef, signature);
			break;
		case PROLOG_MATCH_KIND_REFERENCE:
			match = new PrologReferenceMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, offset, length, declOrDef, signature);
			break;
		case PROLOG_MATCH_KIND_DEFAULT:
		default:
			match = new PrologMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, offset, length, declOrDef, signature);
			break;
		}
		match.createLabel();
		return match;
	}
	
	protected PredicateMatch createUniqueMatch(String definingModule, String functor, int arity, List<String> properties, String visibility, String declOrDef) {
		String predicateSignature = visibility + definingModule + functor + arity;
		SearchPredicateElement searchPredicateElement = predicateElements.get(predicateSignature);
		if (searchPredicateElement == null) {
			searchPredicateElement = new SearchPredicateElement(null, definingModule, functor, arity, properties);
			predicateElements.put(predicateSignature, searchPredicateElement);
		}
		PredicateMatch match = new PredicateMatch(searchPredicateElement, visibility, definingModule, functor, arity, properties, declOrDef);
		return match;
	}
	
	protected DirectiveMatch createUniqueMatch(String module, IFile file, int offset, int length, List<String> properties) {
		String directiveSignature = module + "#" + file + offset + "#" + length;
		SearchDirectiveElement searchDirectiveElement = directiveElements.get(directiveSignature);
		if (searchDirectiveElement == null) {
			searchDirectiveElement = new SearchDirectiveElement();
			directiveElements.put(directiveSignature, searchDirectiveElement);
		}
		DirectiveMatch match = new DirectiveMatch(searchDirectiveElement, module, properties, file, offset, length, directiveSignature);
		match.createLabel();
		return match;
	}

	protected DirectiveMatch createUniqueMatch(String module, IFile file, int line, List<String> properties) {
		String directiveSignature = module + "#" + file + line;
		SearchDirectiveElement searchDirectiveElement = directiveElements.get(directiveSignature);
		if (searchDirectiveElement == null) {
			searchDirectiveElement = new SearchDirectiveElement();
			directiveElements.put(directiveSignature, searchDirectiveElement);
		}
		DirectiveMatch match = new DirectiveMatch(searchDirectiveElement, module, properties, file, line, directiveSignature);
		match.createLabel();
		return match;
	}
	
	@Override
	public String getLabel() {
		return "Prolog Query: " + searchGoalLabel;
	}

	@Override
	public boolean canRerun() {
		return true;
	}

	@Override
	public boolean canRunInBackground() {
		return false;
	}

	@Override
	public ISearchResult getSearchResult() {
		return result;
	}

//	protected void setGoal(Goal goal) {
//		this.goal = goal;
//	}
//
//	protected Goal getGoal() {
//		return goal;
//	}
//	
	protected IFile findFile(String fileName) throws IOException {
		if (fileName == null || SearchConstants.RESULT_KIND_DYNAMIC.equals(fileName) || SearchConstants.RESULT_KIND_FOREIGN.equals(fileName)) {
			return null;
		} else {
			return FileUtils.findFileForLocation(fileName);
		}
	}
	
	protected static String toPredicateGoal(Goal goal) {
		return bT(SearchConstants.PREDICATE_GOAL_FUNCTOR,
				(goal.getModule() != null && !ParserUtils.isVarPrefix(goal.getModule())) ? QueryUtils.quoteAtomIfNeeded(goal.getModule()) : "_",
				"_",
				QueryUtils.quoteAtomIfNeeded(goal.getFunctor()),
				"_",
				goal.getArity() >= 0 ? goal.getArity() : "_");
	}
	
	public void setProjectScope(IProject project) {
		this.project = project;
		if (project != null) {
			projectPath = QueryUtils.prologFileNameQuoted(project.getLocation().toFile());
		} else {
			projectPath = null;
		}
	}
	
	public IProject getProject() {
		return project;
	}
	
	public String getProjectPath() {
		return projectPath;
	}

}


