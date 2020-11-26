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

package org.cs3.prolog.connector.internal.test;

import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;

import org.cs3.pdt.connector.registry.DefaultSAXPrologProcessRegistry;

public class RegistryHandlerTest extends TestCase {
	public void testHandler() throws Exception {
		Reader reader = new StringReader(
				"<registry>"+
				"   <subscription"+
				"      hostid=\"org.cs3.pdt.core\""+
				"      persistent=\"true\""+
				"      class=\"org.cs3.pdt.core.internal.natures.RuntimeSubscription\""+
				"      processkey=\"JTEngine\""+
				"      description=\"used as default runtime for projectJTEngine\""+
				"      project=\"JTEngine\""+
				"      bundle=\"org.cs3.pdt.core\""+
				"      name=\"JTEngine - runtime\""+
				"      id=\"JTEngine.runtime_subscription\""+
				"   />"+
				"   <subscription"+
				"      hostid=\"org.cs3.pdt.core\""+
				"      persistent=\"true\""+
				"      class=\"org.cs3.pdt.core.internal.natures.MetadataSubscription\""+
				"      processkey=\"JTEngine\""+
				"      description=\"used to store and process meta information on prologsource code found in project JTEngine\""+
				"      project=\"JTEngine\""+
				"      bundle=\"org.cs3.pdt.core\""+
				"      name=\"JTEngine - metadata\""+
				"      id=\"JTEngine.metadata_subscription\""+
				"   />"+
				"</registry>");
			new DefaultSAXPrologProcessRegistry().load(reader);		
		}
}


