/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.connector.internal.service;

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.registry.PrologProcessRegistryEvent;
import org.cs3.pdt.connector.registry.PrologProcessRegistryListener;
import org.cs3.pdt.connector.registry.RegistryHook;

public class ServiceRegistryHook implements RegistryHook {
	
	public static PrologProcessRegistryListener listener;

	@Override
	public void addSubscriptions(PrologProcessRegistry registry) {
		listener = new PrologProcessRegistryListener() {
			
			@Override
			public void subscriptionRemoved(PrologProcessRegistryEvent e) {
			}
			
			@Override
			public void subscriptionAdded(PrologProcessRegistryEvent e) {
			}
			
			@Override
			public void processRemoved(PrologProcessRegistryEvent e) {
				if (e.process == ((PrologProcessService) PDTConnectorPlugin.getDefault().getPrologProcessService()).getProcess()) {
					PDTConnectorPlugin.getDefault().getPrologProcessService().setActivePrologProcess(null);
				}
			}
			
			@Override
			public void processAdded(PrologProcessRegistryEvent e) {
			}
		};
		registry.addPrologProcessRegistryListener(listener);
	}

}
