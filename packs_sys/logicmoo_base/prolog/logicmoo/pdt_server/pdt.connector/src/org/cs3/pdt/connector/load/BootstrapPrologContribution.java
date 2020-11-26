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

package org.cs3.pdt.connector.load;

import java.util.Set;

abstract public class BootstrapPrologContribution {

	private Set<String> dependencies;
	public Set<String> getDependencies() {
		return dependencies;
	}

	private String id;

	public String getId() {
		return id;
	}


	public BootstrapPrologContribution(String id,Set<String> dependencies) {
		this.id = id;
		this.dependencies = dependencies;
	}

	@Override
	public String toString() {
		String contribString = "bootstrap contribution " + id;
		if(dependencies == null || dependencies.size() == 0){
			return contribString;
		}
		return contribString + " depends on [" + dependencies+ "]";
	}
	
	public abstract String getPrologInitStatement();
	
	@Override
	public int hashCode() {
		return getId().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return ((BootstrapPrologContribution)obj).getId().equals(id);
	}
}


