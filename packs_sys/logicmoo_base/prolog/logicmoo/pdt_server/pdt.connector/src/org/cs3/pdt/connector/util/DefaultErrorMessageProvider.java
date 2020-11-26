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


package org.cs3.pdt.connector.util;

import org.eclipse.core.runtime.Plugin;


public class DefaultErrorMessageProvider implements ErrorMessageProvider{

	private String id;
	private Plugin plugin;

	public DefaultErrorMessageProvider(Plugin plugin) {
		this.id =plugin.getBundle().getSymbolicName();
		this.plugin=plugin;
	}

	@Override
	public String getErrorMessage(int errCode) {
		return "unknown error("+errCode+")";
	}

	@Override
	public String getContextMessage(int cxCode) {
		return "unknown error context("+cxCode+")";
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public Plugin getPlugin() {
		return plugin;
	}

}


