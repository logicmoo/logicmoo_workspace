/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common;

import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.registry.PrologProcessRegistryEvent;
import org.cs3.pdt.connector.registry.PrologProcessRegistryListener;
import org.cs3.pdt.connector.registry.RegistryHook;
import org.cs3.prolog.connector.process.LifeCycleHook;

public class CommonRegistryHoook implements RegistryHook {

	private static final String[] EMPTY_STRING_ARRAY = new String[0];

	@Override
	public void addSubscriptions(PrologProcessRegistry registry) {
		final LifeCycleHook lifeCycleHook = PDTCommonPlugin.getDefault().getLifeCycleHook();
		registry.addPrologProcessRegistryListener(new PrologProcessRegistryListener() {
			@Override public void subscriptionRemoved(PrologProcessRegistryEvent e) {}
			@Override public void subscriptionAdded(PrologProcessRegistryEvent e) {}
			@Override public void processRemoved(PrologProcessRegistryEvent e) {}
			
			@Override
			public void processAdded(PrologProcessRegistryEvent e) {
				e.process.addLifeCycleHook(lifeCycleHook, PDTCommonPlugin.LIFE_CYCLE_HOOK_ID, EMPTY_STRING_ARRAY);
			}
		});
		for (String key : registry.getRegisteredKeys()) {
			registry.getPrologProcess(key).addLifeCycleHook(lifeCycleHook, PDTCommonPlugin.LIFE_CYCLE_HOOK_ID, EMPTY_STRING_ARRAY);
		}
	}

}
