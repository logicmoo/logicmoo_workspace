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

package org.cs3.pdt.editor.internal.editors;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.net.URI;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.editor.PDTPlugin;
import org.cs3.pdt.editor.PDTPredicates;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;

public class PLScanner extends RuleBasedScanner implements IPropertyChangeListener{
	private ColorManager manager;
	private IFile file;

	public PLScanner(PLEditor editor, ColorManager manager) 
	throws CoreException, PrologProcessException {

		this.manager = manager;
		
		assert(editor!=null);
		
		IEditorInput input = editor.getEditorInput();

		if (input instanceof IFileEditorInput) {
			IFileEditorInput editorInput = (IFileEditorInput) input;
			if (editorInput != null) {
				file = editorInput.getFile();
			}
		}
		else if (input instanceof FileStoreEditorInput) {
			FileStoreEditorInput storeEditorInput = (FileStoreEditorInput) input;
			if (storeEditorInput != null) {
				URI uri = storeEditorInput.getURI();
				String path = uri.getPath();
				try {
					file = FileUtils.findFileForLocation(path);
				} catch (IOException e) {

				}
			}
		}
		
		IPreferenceStore store = PDTPlugin.getDefault().getPreferenceStore();
		store.addPropertyChangeListener(this);
		
		initHighlighting();
	}

	void initHighlighting()
			throws PrologProcessException, CoreException {
		// "Tokens" indicate the desired highlighting
		IToken variableToken    = tokenFor(manager.getVariableColor());
		IToken stringToken      = tokenFor(manager.getStringColor());
		IToken wordToken        = tokenFor(manager.getDefaultColor());

        // Create rules for syntax highlighting of ...
		IRule[] rules = new IRule[6];		
        // - variables:
		rules[0] = new VarRule(variableToken);
		// - double quotes:
		rules[1] = new MultiLineRule("\"", "\"", stringToken, '\\');
		// - single quotes:
		rules[2] = new MultiLineRule("'", "'", stringToken, '\\');
		// - whitespace:
		rules[3] = new WhitespaceRule(new PLWhitespaceDetector());
		// - special words: 
		rules[4] = new WordRule(new WordDetector(), wordToken);
		addWordsTo((WordRule)rules[4]);
		
		rules[5] = new CharacterCodeRule(wordToken);

		// Activate the defined rules.
		setRules(rules);
	}

	/**
	 * Add to the special words of wordRule all names of predicates 
	 * that should be highlighted in a specific way and associate
	 * them to tokens that indicate the desired highlighting.
	 * 
     * @param wordRule -- The WordRule to which we add words.
	 * @param manager -- The ColorManager 
	 * @param plProject -- The PDT Metadata process.
	 * @throws PrologProcessException
	 */
	private void addWordsTo(WordRule wordRule) throws PrologProcessException, CoreException {
		
		// The order of the following definitions is important!
		// The latter ones overrule the previous ones. E.g. a predicate
		// that is transparent AND a metapredicate will be highlighted
		// as a metapredicate because the metapredicates are added later.
		// This makes sense since being a metapredicate is more specific
		// (each metapredicate is transparent but not every transparent
		// predicate is a metapredicate). -- GK
		
		addWordsWithProperty("undefined", tokenFor(manager.getUndefinedColor()), wordRule);
		addWordsWithProperty("built_in", tokenFor(manager.getKeywordColor()), wordRule);
		addWordsWithProperty("dynamic",  tokenFor(manager.getDynamicColor()), wordRule);
		addWordsWithProperty("transparent", tokenFor(manager.getTransparentColor()), wordRule);
		addWordsWithProperty("meta_predicate(_)", tokenFor(manager.getMetaColor()), wordRule);
	}


	private Token tokenFor(RGB color) {
		return new Token(new TextAttribute(manager.getColor(color), null, 1)); /*SWT.NORMAL | SWT.ITALIC | SWT.BOLD | TextAttribute.UNDERLINE)*/
	}
	
	/**
	 * Add to the special words of wordRule all names of predicates 
	 * that have a certain property and associate each with the 
	 * token that indicates the desired highlighting.
	 * 
	 * @param property -- The desired property 
	 * @param keywordToken -- The desired highlighting for words with that property
	 * @param wordRule -- The WordRule to which to add the words.
	 * @param plProject -- The PDT Metadata process.
	 * @throws PrologProcessException
	 * @throws CoreException 
	 */
	private void addWordsWithProperty(String property, IToken keywordToken, WordRule wordRule) throws PrologProcessException, CoreException {
		String[] plBuiltInPredicates = getPredicatesWithProperty(property);
		for (int i = 0; plBuiltInPredicates!=null&&i < plBuiltInPredicates.length; i++){
			wordRule.addWord(plBuiltInPredicates[i], keywordToken);
		}
	}


	/**
	 * getPredicatesWithProperty(String property)
	 * 
	 * Get names of predicates that have a certain property. This implementation
	 * (by Tobias Rho) is for projects that DO NOT have the PDT nature.
	 * 
	 */
	private String[] getPredicatesWithProperty(String property) throws PrologProcessException, CoreException {
		PrologSession session = null;
		try {
			session = PDTCommonUtil.getActivePrologProcess().getSession();
			Map<String, Object> solutions = session
					.queryOnce(bT(PDTPredicates.PREDICATES_WITH_PROPERTY, property, QueryUtils.quoteAtom(FileUtils.prologFileName(file)), "Predicates")); 

			if (solutions == null)
				return null;
			String predicatesStr = (String) solutions.get("Predicates");
			// swipl 5.8.x adds ", " between list elements when writing
			// Strings/Streams:
			predicatesStr = predicatesStr.replaceAll(" ", "");
			return predicatesStr.substring(1, predicatesStr.length() - 1)
					.split(",");
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null)
				session.dispose();
		}
		return null;
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (event.getProperty().startsWith("pdt.editor.colors")) {
			try {
				initHighlighting();
			} catch (CoreException e) {
				Debug.report(e);
			} catch (PrologProcessException e) {
				Debug.report(e);
			}	
		}
	}
}


