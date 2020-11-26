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

package org.cs3.pdt.console.internal.views.completion;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;

public class PrologCompletionProvider {

	private static final PredicateCompletionProposal[] EMPTY_COMPLETION_PROPOSAL = new PredicateCompletionProposal[0];

	private PrologProcess process;

	@SuppressWarnings("unchecked")
	public IContentProposal[] getCompletionProposals(String line, int pos) {
//		String head = line.substring(0, pos);
//
//		String[] split = head.split("[^\\w^$]");
//		if (split.length == 0) {
//			return EMPTY_COMPLETION_PROPOSAL;
//		}
//		String prefix = split[split.length - 1];
		
		// Line is empty
		if (line.isEmpty()) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		// Can't calculate a proposal if the position is the beginning of the line
		if (pos < 1){
			return EMPTY_COMPLETION_PROPOSAL;
		}
		
		String searchPrefix;
		Prefix prefix = calculatePrefix(line, pos - 1);
		String splittingOperator = findSplittingOperator(line, prefix.begin - 1);
		String module = null;
		if (splittingOperator != null) {
			module = retrievePrefixedModule(line, prefix.begin - splittingOperator.length());
		} else if (prefix.prefix.isEmpty()) {
			return EMPTY_COMPLETION_PROPOSAL;
		}
		if (module == null || module.isEmpty()) {
			if (prefix.prefix.isEmpty()) {
				return EMPTY_COMPLETION_PROPOSAL;
			}
			searchPrefix = QueryUtils.quoteAtomIfNeeded(prefix.prefix);
		} else {
			if (ParserUtils.isVarPrefix(module)){
				module = "_";
			} else {
				module = QueryUtils.quoteAtomIfNeeded(module);
			}
			searchPrefix = module + splittingOperator + QueryUtils.quoteAtomIfNeeded(prefix.prefix);
		}
		
		
		ArrayList<ComparableCompletionProposal> proposals = new ArrayList<ComparableCompletionProposal>();
		String query = bT(PDTCommonPredicates.FIND_COMPLETION,
				searchPrefix,
				"_",
				"_",
				"Kind",
				"Module",
				"Name",
				"Arity",
				"Visibility",
				"Builtin",
				"IsDeprecated",
				"ArgNames",
				"DocKind",
				"Doc",
				"NeedsQuotes");
		List<Map<String, Object>> results;
		try {
			results = process.queryAll(query);
			for (Map<String,Object> result : results) {
				String kind = result.get("Kind").toString();
				String name = result.get("Name").toString();
				boolean needsQuotes = Boolean.parseBoolean((String) result.get("NeedsQuotes"));
				if (SearchConstants.COMPLETION_KIND_PREDICATE.equals(kind)) {
					String resultModule = result.get("Module").toString();
					int arity = Integer.parseInt(result.get("Arity").toString());
					String visibility = result.get("Visibility").toString();
					boolean isBuiltin = Boolean.parseBoolean(result.get("Builtin").toString());
					boolean isDeprecated = Boolean.parseBoolean(result.get("IsDeprecated").toString());
					Object argNamesValue = result.get("ArgNames");
					List<String> argNames = null;
					if (argNamesValue instanceof List<?>) {
						argNames = (List<String>) argNamesValue;
					}
					String docKind = (String) result.get("DocKind");
					String doc = (String) result.get("Doc");
					proposals.add(new PredicateCompletionProposal(resultModule, name, arity, prefix.length, visibility, isBuiltin, isDeprecated, argNames, docKind, doc, prefix.startsWithSingleQuote, needsQuotes));
				} else if (SearchConstants.COMPLETION_KIND_MODULE.equals(kind)){
					proposals.add(new ModuleCompletionProposal(name, prefix.length, prefix.startsWithSingleQuote, needsQuotes));
				} else if (SearchConstants.COMPLETION_KIND_ATOM.equals(kind)){
					proposals.add(new AtomCompletionProposal(name, prefix.length, prefix.startsWithSingleQuote, needsQuotes));
				}
			}
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
		Collections.sort(proposals);
		return proposals.toArray(new ComparableCompletionProposal[proposals.size()]);
	}
	
	private class Prefix {
		int begin;
		int length;
		String prefix;
		boolean startsWithSingleQuote;
				
		Prefix(int begin, String prefix, boolean startsWithSingleQuote) {
			this.begin = begin;
			this.prefix = prefix;
			this.length = prefix.length();
			this.startsWithSingleQuote = startsWithSingleQuote;
		}
	}
		
	private Prefix calculatePrefix(String line, int offset) {
		int begin = offset;
		int length = 0;
		char c = line.charAt(begin);
		if (c == '\'') {
			return new Prefix(offset, "", false);
		}
		boolean isPredicateChar = ParserUtils.isPredicateNameChar(c);
		
		if (c == ':' || !isPredicateChar) {
			return new Prefix(offset + 1, "", false);
		}
		
		while (isPredicateChar){
			length++;
			int test = begin - 1;
			if(test >= 0){
				c = line.charAt(test);
				if (c == '\'') {
					return new Prefix(begin - 1, line.substring(begin, begin + length), true);
				}
				isPredicateChar = ParserUtils.isPredicateNameChar(c);
				if (c == ':' || !isPredicateChar) {
					break;
				}
			} else {
				break;
			}
			begin = test;
		}
		return new Prefix(begin, line.substring(begin, begin + length), false);
	}
	
	private String findSplittingOperator(String line, int begin) {
		if (begin <= 0) {
			return null;
		}
		char c = line.charAt(begin);
		char c2 = line.charAt(begin - 1);
		switch (c) {
		case ':':
			switch (c2) {
			case ':':
				return "::";
			default:
				return ":";
			}
		case '<':
			if (c2 == '<') {
				return "<<";
			} else {
				return null;
			}
		}
		return null;
	}
    
    private String retrievePrefixedModule(String line, int begin) {
        int moduleEnd = begin;
        int moduleBegin = begin - 1;
        char firstChar = line.charAt(moduleBegin);
        if (firstChar == '\'') {
            return retrieveQualifiedPrefixedModule(line, moduleBegin);
        }
        while (moduleBegin >= 0 && ParserUtils.isNonQuotedPredicateNameChar(line.charAt(moduleBegin))) {
            moduleBegin--;
        }
        String moduleName = line.substring(moduleBegin + 1, moduleEnd);
        return moduleName;
    }
    
    private String retrieveQualifiedPrefixedModule(String line, int begin) {
        int moduleEnd = begin + 1;
        int moduleBegin = begin - 1;
        while (moduleBegin >= 0) {
        	char c = line.charAt(moduleBegin);
        	if (c == '\'') {
        		return line.substring(moduleBegin, moduleEnd);
        	}
        	moduleBegin--;
        }
        return null;
    }
    
	public void setPrologProcess(PrologProcess process) {
		this.process = process;
	}

	public PrologProcess getPrologProcess() {
		return process;
	}
}


