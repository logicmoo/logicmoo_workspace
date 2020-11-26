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


public class BootstrapPrologContributionFile extends BootstrapPrologContribution{

	private String file;
	public String getFile() {
		return file;
	}

	public BootstrapPrologContributionFile(String id, String file, Set<String> dependencies) {
		super(id, dependencies);
		this.file = file;
	}

	@Override
	public String getPrologInitStatement() {
		return "['" + file + "']";
	}
	
	

	
}


