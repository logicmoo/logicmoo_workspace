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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.cs3.pdt.editor.internal.editors.PLPartitionScanner;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

public abstract class PrologContentAssistProcessor {

	public PrologContentAssistProcessor() {
		super();
	}

	protected abstract IFile getFile() throws CoreException;
	
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
		
	protected abstract void addPredicateProposals(IDocument document, int begin,
			int len, boolean startsWithSingleQuote, String prefix, String searchPrefixForDefault, List<ComparableTemplateCompletionProposal> proposals)
			throws PrologProcessException, CoreException;

	protected abstract void addVariableProposals(IDocument document, int begin,
			int len, String prefix, List<ComparableTemplateCompletionProposal> proposals) throws BadLocationException, PrologProcessException, CoreException;

	private Prefix calculatePrefix(IDocument document, int offset) throws BadLocationException {
		
		int begin=offset;
		int length=0;
		char c = document.getChar(begin);
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
			if (test >= 0) {
				c = document.getChar(test);
				if (c == '\'') {
					return new Prefix(begin - 1, document.get(begin, length), true);
				}
				isPredicateChar = ParserUtils.isPredicateNameChar(c);
				if (c == ':' || !isPredicateChar){
					break;
				}
			} else {
				break;
			}
			begin = test;
		}
		String pre = document.get(begin, length);
		
		Prefix prefix = new Prefix(begin, pre, false);
		return prefix;
	}

	private String findSplittingOperator(IDocument document, int begin) throws BadLocationException {
		if (begin <= 0) {
			return null;
		}
		char c = document.getChar(begin);
		char c2 = document.getChar(begin - 1);
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
		case '^':
			if (c2 == '^') {
				return "^^";
			}
		}
		return null;
	}
	
//	private String retrievePrefixedModule(IDocument document, int begin)
//			throws BadLocationException {
//		int moduleEnd = begin;
//		int moduleBegin = begin - 1;
//		while (moduleBegin >= 0 && ParserUtils.isNonQuotedPredicateNameChar(document.getChar(moduleBegin)))
//			moduleBegin--;
//		String moduleName = document.get(moduleBegin + 1, moduleEnd - moduleBegin - 1);
//		return moduleName;
//	}
    
    private String retrievePrefixedModule(IDocument document, int begin) throws BadLocationException {
        int moduleEnd = begin;
        int moduleBegin = begin - 1;
        char firstChar = document.getChar(moduleBegin);
        if (firstChar == '\'') {
            return retrieveQualifiedPrefixedModule(document, moduleBegin);
        }
        while (moduleBegin >= 0 && ParserUtils.isNonQuotedPredicateNameChar(document.getChar(moduleBegin))) {
            moduleBegin--;
        }
        String moduleName = document.get(moduleBegin + 1, moduleEnd - moduleBegin - 1);
        return moduleName;
    }
    
    private String retrieveQualifiedPrefixedModule(IDocument document, int begin) throws BadLocationException {
        int moduleEnd = begin + 1;
        int moduleBegin = begin - 1;
        while (moduleBegin >= 0) {
        	char c = document.getChar(moduleBegin);
        	if (c == '\'') {
        		if (moduleBegin > 0 && (document.getChar(moduleBegin - 1) == '\\' || document.getChar(moduleBegin - 1) == '\'')) {
        			moduleBegin--;
        		} else {
        			return document.get(moduleBegin, moduleEnd - moduleBegin);
        		}
        	}
        	moduleBegin--;
        }
        return null;
    }
    

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
	
		try {
			IDocument document = viewer.getDocument();
	
			documentOffset = documentOffset == 0 ? documentOffset
					: documentOffset - 1;
	
			Prefix pre = calculatePrefix(document,documentOffset);
			
			ArrayList<ComparableTemplateCompletionProposal> proposals = new ArrayList<ComparableTemplateCompletionProposal>();
			if (ParserUtils.isVarPrefix(pre.prefix)) {
				addVariableProposals(document, pre.begin, pre.length, pre.prefix, proposals);
			} else {
				String splittingOperator = findSplittingOperator(document, pre.begin - 1);
				
				String module = null;
				if (splittingOperator != null) {
					module = retrievePrefixedModule(document, pre.begin - splittingOperator.length());
				}
				String searchPrefix;
				String searchPrefixForDefault = null;
				
				if (module == null || module.equals("")) {
					if (pre.prefix.equals("")) {
						return null;
					}
					if (splittingOperator != null) {
						searchPrefix = splittingOperator + QueryUtils.quoteAtomIfNeeded(pre.prefix);
					} else {
						searchPrefix = QueryUtils.quoteAtomIfNeeded(pre.prefix);
						searchPrefixForDefault = pre.prefix;
					}
				} else {
					if (ParserUtils.isVarPrefix(module)){
						module = "_";
					} else {
						module = QueryUtils.quoteAtomIfNeeded(module);
					}
					searchPrefix = module + splittingOperator + QueryUtils.quoteAtomIfNeeded(pre.prefix);
				}
				addPredicateProposals(document, pre.begin, pre.length, pre.startsWithSingleQuote, searchPrefix, searchPrefixForDefault, proposals);
			}
	
			if (proposals.size() == 0)
				return null;
			Collections.sort(proposals);
			return proposals
					.toArray(new ICompletionProposal[proposals.size()]);
		} catch (BadLocationException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_COMPLETION_BAD_LOCATION,
//					PDT.CX_COMPLETION, e);
			return null;
		} catch (PrologProcessException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_PROCESS, PDT.CX_COMPLETION, e);
			return null;
		} catch (CoreException e) {
			Debug.report(e);
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
//					.getErrorMessageProvider(), viewer.getTextWidget()
//					.getShell(), PDT.ERR_CORE_EXCEPTION, PDT.CX_COMPLETION, e);
			return null;
		} finally {
	
		}
	}

	protected boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
	}

	public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
	
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
	
		return new char[0];
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return new char[0];
	}

	public IContextInformationValidator getContextInformationValidator() {
		class Validator implements IContextInformationValidator {
	
			@Override
			public boolean isContextInformationValid(int position) {
				return true;
			}
	
			@Override
			public void install(IContextInformation info, ITextViewer viewer,
					int documentPosition) {
				;
	
			}
		}
		return new Validator();
	}

	public String getErrorMessage() {
		return "Error Message?";
	}

}


