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

/*
 * Created on 02.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.editor.internal.editors;

import junit.framework.TestCase;

import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.editor.metadata.GoalProvider;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;

/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PLEditorTest extends TestCase {
	public void testGetPrologDataFromOffset() throws BadLocationException {
		Document document = new Document("  , ahaha(a,b,c).");
		Goal data = GoalProvider.getPrologDataFromOffset(null,document, 6, 0);
		assertEquals("ahaha",data.getFunctor());
		assertEquals(3,data.getArity());

		document = new Document("  , ahah1a(a,b(a),c).");
		data = GoalProvider.getPrologDataFromOffset(null,document, 6, 0);
		assertEquals("ahah1a",data.getFunctor());
		assertEquals(3,data.getArity());

		document = new Document("  , ahaha(\"asdf\\\"\'\",b(a),c).");
		data = GoalProvider.getPrologDataFromOffset(null,document, 6, 0);
		assertEquals("ahaha",data.getFunctor());
		assertEquals(3,data.getArity());
	
		document = new Document("  , ahaha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = GoalProvider.getPrologDataFromOffset(null,document, 6, 0);
		assertEquals("ahaha",data.getFunctor());
		assertEquals(3,data.getArity());

		document = new Document("  , aha_ha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = GoalProvider.getPrologDataFromOffset(null,document, 6, 0);
		assertEquals("aha_ha",data.getFunctor());
		assertEquals(3,data.getArity());

		document = new Document(" test/12");
		data = GoalProvider.getPrologDataFromOffset(null,document, 3, 0);
		assertEquals("test",data.getFunctor());
		assertEquals(12,data.getArity());

//		document = new Document("type: ");
//		data = GoalProvider.getPrologDataFromOffset(null,document, 3, 0);
//		assertEquals("type",data.getName());
//		assertEquals(-1,data.getArity());
	}
}


