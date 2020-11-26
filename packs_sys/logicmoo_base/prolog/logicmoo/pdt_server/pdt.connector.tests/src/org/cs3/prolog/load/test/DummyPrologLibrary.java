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

package org.cs3.prolog.load.test;

import java.util.HashSet;
import java.util.Set;

import org.cs3.pdt.connector.load.PrologLibrary;
import org.cs3.prolog.connector.common.Util;

class DummyPrologLibrary implements PrologLibrary{
	String id;
	Set<String> deps;
	

	public DummyPrologLibrary(String id, String dependenciess){
		this.id=id;
		this.deps=new HashSet<String>();
		for (int i = 0; i < dependenciess.length(); i++) {
			deps.add(String.valueOf(dependenciess.charAt(i)));
		}
	}
	@Override
	public String toString() {
	
		return "Lib "+id+" -> "+Util.prettyPrint(deps.toArray());
	}
	public DummyPrologLibrary(String id) {
		this.id=id;
		this.deps=new HashSet<String>();
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getPath() {
		return "path";
	}

	@Override
	public String getAlias() {
		return "alias";
	}

	@Override
	public Set<String> getDependencies() {
		return deps;
	}
	@Override
	public String getAttributeValue(String attr) {
		return null;
	}
	
}

