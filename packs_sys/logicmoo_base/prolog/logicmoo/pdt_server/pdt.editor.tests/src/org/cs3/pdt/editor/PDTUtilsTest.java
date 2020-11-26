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

package org.cs3.pdt.editor;

import junit.framework.TestCase;

import org.cs3.prolog.connector.common.Util;

public class PDTUtilsTest extends TestCase {

	public void testLogicalToPhysicalOffset01() throws Throwable{
		String data = "0"+"\r\n"+"2"+"3"+"\n"+"5";
		assertEquals(0,Util.logicalToPhysicalOffset(data,0));
		assertEquals(1,Util.logicalToPhysicalOffset(data,1));
		assertEquals(3,Util.logicalToPhysicalOffset(data,2));
		assertEquals(4,Util.logicalToPhysicalOffset(data,3));
		assertEquals(5,Util.logicalToPhysicalOffset(data,4));
		assertEquals(6,Util.logicalToPhysicalOffset(data,5));
		
	}
}


