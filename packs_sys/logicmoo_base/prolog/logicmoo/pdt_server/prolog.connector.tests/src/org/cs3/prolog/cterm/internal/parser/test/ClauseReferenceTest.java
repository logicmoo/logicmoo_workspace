/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Lukas Degener, Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.cterm.internal.parser.test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.TestCase;

import org.cs3.prolog.connector.internal.cterm.parser.ASTAtom;
import org.cs3.prolog.connector.internal.cterm.parser.CanonicalTermParser;
import org.cs3.prolog.connector.internal.cterm.parser.Node;

public class ClauseReferenceTest extends TestCase {
    CanonicalTermParser termParser;
    InputStream inputStream;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
//		termParser = new CanonicalTermParser(inputStream);
	}
	
	public void testClauseReference() throws Exception{
		termParser=new CanonicalTermParser(mockStream("<clause>(02D193FC)"));
		termParser.Term();
		Node n =termParser.getASTRoot();
		
		
		ASTAtom expectedAtom = (ASTAtom)n;
		assertEquals("<clause>(02D193FC)",expectedAtom.getString());
	}
	
	
	private InputStream mockStream(String inputText){
		try {
			return new ByteArrayInputStream(inputText.getBytes());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
}


