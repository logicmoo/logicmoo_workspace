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

import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

public class PLPartitionScanner extends RuleBasedPartitionScanner {
	public final static String PL_DEFAULT = "__pl_default";
	public final static String PL_COMMENT = "__pl_comment";
	public final static String PL_MULTI_COMMENT = "__pl_multi_line_comment";
	public final static String PL_SINGLE_QUOTED_STRING = "__pl__single_quoted_stringt";
	public final static String PL_DOUBLE_QUOTED_STRING = "__pl__double_quoted_stringt";
	public static final String PL_CHARACTER_CODE = "__pl__character_code";

	public PLPartitionScanner() {

		IToken plComment = new Token(PL_COMMENT);
		IToken plMultiComment = new Token(PL_MULTI_COMMENT);
		IToken plSingleQuotedString = new Token(PL_SINGLE_QUOTED_STRING);
		IToken plDoubleQuotedString = new Token(PL_DOUBLE_QUOTED_STRING);
		IToken characterCode = new Token(PL_CHARACTER_CODE);
		//IToken plPredicate = new Token(PL_DEFAULT);
		
		IPredicateRule[] rules = new IPredicateRule[5];

		rules[0] = new EndOfLineRule("%", plComment);
		rules[1] = new MultiLineRule("/*", "*/", plMultiComment);
		rules[2] = new MultiLineRule("\"", "\"", plDoubleQuotedString, '\\');
		// Add a rule for single quotes
		rules[3] = new MultiLineRule("'", "'", plSingleQuotedString, '\\'); // The escape character is not correct. Putting ' in there fails rule parsing.
		// Add generic whitespace rule.
//        rules[2] = new PredicateRule(plPredicate);
		rules[4] = new CharacterCodeRule(characterCode);

		setPredicateRules(rules);
	}
}


