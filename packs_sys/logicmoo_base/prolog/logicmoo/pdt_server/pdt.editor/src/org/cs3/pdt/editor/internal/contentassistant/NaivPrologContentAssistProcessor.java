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

package org.cs3.pdt.editor.internal.contentassistant;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;

public abstract class NaivPrologContentAssistProcessor extends PrologContentAssistProcessor implements IContentAssistProcessor {

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len, String prefix, List<ComparableTemplateCompletionProposal> proposals) throws BadLocationException {
		Set<String> unique = new HashSet<String>();

		int l = begin == 0 ? begin : begin - 1;
		String proposal = null;
		while (l > 0 && !PLEditor.predicateDelimiter(document, l)) {
			ITypedRegion region = document.getPartition(l);
			if (isComment(region))
				l = region.getOffset();
			else {
				char c = document.getChar(l);
				if (ParserUtils.isVarChar(c)) {
					if (proposal == null)
						proposal = "";
					proposal = c + proposal;
				} else if (proposal != null) {
					if (ParserUtils.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /* && !proposal.equals("_")*/) {
						unique.add(proposal);
						//							int cursorPos = proposal.length();
						proposals.add(new VariableCompletionProposal(document, proposal, begin, len));
					}
					proposal = null;
				}
			}
			l--;
		}
		l = begin == document.getLength() ? begin : begin + 1;
		proposal = null;
		while (l < document.getLength() && !PLEditor.predicateDelimiter(document, l)) {
			ITypedRegion region = document.getPartition(l);
			if (isComment(region)) {
				l = region.getOffset() + region.getLength();
			} else {
				char c = document.getChar(l);
				if (ParserUtils.isVarChar(c)) {
					if (proposal == null)
						proposal = "";
					proposal = proposal + c;
				} else if (proposal != null) {
					if (ParserUtils.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /*&& !proposal.equals("_")*/) {
						unique.add(proposal);
						//							int cursorPos = proposal.length();
						proposals.add(new VariableCompletionProposal(document, proposal, begin, len));
					}
					proposal = null;
				}
			}
			l++;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void addPredicateProposals(IDocument document, int begin, int len, boolean startsWithSingleQuote, String prefix, String searchPrefixForDefault, List<ComparableTemplateCompletionProposal> proposals) throws PrologProcessException,
			CoreException {

		PrologSession session = null;
		try {
			String enclFile = UIUtils.getFileFromActiveEditor();
			session = PDTCommonUtil.getActivePrologProcess().getSession();
			String query = bT(PDTCommonPredicates.FIND_COMPLETION,
					prefix,
					QueryUtils.quoteAtom(enclFile),
					document.getLineOfOffset(begin) + 1,
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
			List<Map<String, Object>> results = session.queryAll(query);
			Debug.info("find predicates with prefix: " + query);
			for (Map<String, Object> result : results) {
				String kind = result.get("Kind").toString();
				String name = (String) result.get("Name");
				
				boolean needsQuotes = Boolean.parseBoolean((String) result.get("NeedsQuotes"));
				
				if (SearchConstants.COMPLETION_KIND_PREDICATE.equals(kind)) {
					
					String module = result.get("Module").toString();
					if (module.isEmpty()) {
						module = null;
					}
					
					String strArity = (String) result.get("Arity");
					int arity = Integer.parseInt(strArity);
					
					List<String> argNames = null;
					
					if (result.get("ArgNames") instanceof List<?>) {
						argNames = (List<String>) result.get("ArgNames");
					}
					
					String visibility = result.get("Visibility").toString();
					
					boolean isBuiltin = Boolean.parseBoolean(result.get("Builtin").toString());

					boolean isDeprecated = Boolean.parseBoolean(result.get("IsDeprecated").toString());

					String docKind = result.get("DocKind").toString();
					
					String doc = null;
					if (result.get("Doc") != null) {
						doc = result.get("Doc").toString();
					}
					
					proposals.add(PredicateCompletionProposal.createProposal(document, begin, len, module, name, arity, argNames, visibility, isBuiltin, isDeprecated, docKind, doc, startsWithSingleQuote || needsQuotes));
				} else if (SearchConstants.COMPLETION_KIND_MODULE.equals(kind)) {
					proposals.add(new ModuleCompletionProposal(document, name, begin, len, startsWithSingleQuote || needsQuotes));
				} else if (SearchConstants.COMPLETION_KIND_ATOM.equals(kind)) {
					proposals.add(new AtomCompletionProposal(document, name, begin, len, startsWithSingleQuote || needsQuotes));
				}
			}
			if (searchPrefixForDefault != null) {
				DefaultCompletion.addDefaultCompletions(enclFile, document, begin, len, searchPrefixForDefault, proposals);
			}
			return;
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null)
				session.dispose();
		}
		return;

	}

}


