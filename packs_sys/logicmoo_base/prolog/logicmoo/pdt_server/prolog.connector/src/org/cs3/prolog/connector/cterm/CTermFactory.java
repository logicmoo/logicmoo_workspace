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

package org.cs3.prolog.connector.cterm;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.internal.cterm.parser.ASTAtom;
import org.cs3.prolog.connector.internal.cterm.parser.ASTCompound;
import org.cs3.prolog.connector.internal.cterm.parser.ASTFloat;
import org.cs3.prolog.connector.internal.cterm.parser.ASTInteger;
import org.cs3.prolog.connector.internal.cterm.parser.ASTNil;
import org.cs3.prolog.connector.internal.cterm.parser.ASTString;
import org.cs3.prolog.connector.internal.cterm.parser.ASTVariable;
import org.cs3.prolog.connector.internal.cterm.parser.CanonicalTermParser;
import org.cs3.prolog.connector.internal.cterm.parser.Node;

/**
 * Contains helper methods to create CTerms.
 */
public class CTermFactory {

	public static CTerm createCTerm(Object data) {
		CanonicalTermParser parser=null;
		if(data instanceof InputStream){
			parser = new CanonicalTermParser((InputStream) data); 
		}
		else if (data instanceof Reader){
			parser = new CanonicalTermParser((Reader) data);
		}
		else{
			String input = data.toString();
			Reader reader = new StringReader(input);
			parser = new CanonicalTermParser(reader);
		}
		try {
			parser.Start();
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		return create(parser.getASTRoot());
	}

	static CTerm create(Node root) {
		if(root instanceof ASTAtom){
			return new CAtom((ASTAtom)root);
		} 
		if(root instanceof ASTString){
			return new CString((ASTString)root);
		}
		if(root instanceof ASTVariable){
			return new CVariable((ASTVariable)root);
		} 
		if(root instanceof ASTCompound){
			return new CCompound((ASTCompound)root);
		} 
		if(root instanceof ASTInteger){
			return new CInteger((ASTInteger)root);
		}
		if(root instanceof ASTFloat){
			return new CFloat((ASTFloat)root);
		}
		if(root instanceof ASTNil){
			return new CEmptyList((ASTNil)root);
		}
		throw new IllegalArgumentException("bad node type: "+root.getClass().getName());
	}

}


