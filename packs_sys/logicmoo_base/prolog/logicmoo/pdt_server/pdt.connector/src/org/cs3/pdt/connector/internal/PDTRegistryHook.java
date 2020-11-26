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

package org.cs3.pdt.connector.internal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.registry.RegistryHook;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * FIXME: Storing the registry in one central file should be replaced or at least complemented
 * by storing the subscription relevant data in the corresponding projects, e.g. in a file .pdtProperties. 
 *
 * Information like the factbase name is not available project import, checkout or after an Eclipse crash. 
 *   
 * See also /jtransformer/src/org/cs3/jtransformer/hook/JTransformerRegistryHook.java
 * 
 * @author trho
 *
 */
public class PDTRegistryHook implements RegistryHook {


	@Override
	public void addSubscriptions(PrologProcessRegistry registry) {
		try {
			PDTConnectorPlugin runtimeUIPlugin = PDTConnectorPlugin.getDefault();
			String pluginID = PDTConnectorPlugin.getPluginId();
			ISavedState lastState =null;
			lastState = ResourcesPlugin.getWorkspace().addSaveParticipant(pluginID, new _SaveParticipant());

			if (lastState != null) {
				IPath location = lastState.lookup(new Path("registry"));
				location = runtimeUIPlugin.getStateLocation().append(location);
				File file = location.toFile();
				if (file.canRead()) {
					Debug.info("Reading registry file " + file.getCanonicalPath());
					Reader r = new BufferedReader(new FileReader(file));
					PDTConnectorPlugin.getDefault().getPrologProcessRegistry().load(r);
				} else {
					Debug.warning("Registry file " + file.getCanonicalPath() + " could not be read. A new file will be created on exit.");
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		} catch (IOException e) {
			Debug.rethrow(e);

		}
	}
	private static final class _SaveParticipant implements ISaveParticipant {
		@Override
		public void saving(ISaveContext context) throws CoreException {
			switch (context.getKind()) {
			case ISaveContext.FULL_SAVE:
				PDTConnectorPlugin myPluginInstance = PDTConnectorPlugin.getDefault();
				// save the plug-in state
				int saveNumber = context.getSaveNumber();
				String saveFileName = "registry-" + Integer.toString(saveNumber);
				File f = myPluginInstance.getStateLocation().append(saveFileName).toFile();
				// if we fail to write, an exception is
				// thrown and we do not update the path
				Writer w = null;
				try {
					Debug.info("writing registry to " + f.getCanonicalPath());
					w = new BufferedWriter(new FileWriter(f));
					PDTConnectorPlugin.getDefault().getPrologProcessRegistry().save(w);
					w.close();
				} catch (IOException e) {
					Debug.rethrow(e);
				}
				context.map(new Path("registry"), new Path(saveFileName));
				context.needSaveNumber();
				break;
			case ISaveContext.PROJECT_SAVE:
				break;
			case ISaveContext.SNAPSHOT:
				break;
			}

		}

		@Override
		public void rollback(ISaveContext context) {
			PDTConnectorPlugin myPluginInstance = PDTConnectorPlugin.getDefault();

			// since the save operation has failed, delete
			// the saved state we have just written
			int saveNumber = context.getSaveNumber();
			String saveFileName = "registry-" + Integer.toString(saveNumber);
			File f = myPluginInstance.getStateLocation().append(saveFileName).toFile();
			f.delete();

		}

		@Override
		public void prepareToSave(ISaveContext context) throws CoreException {
			;
		}

		@Override
		public void doneSaving(ISaveContext context) {
			PDTConnectorPlugin myPluginInstance = PDTConnectorPlugin.getDefault();

			// delete the old saved state since it is not
			// necessary anymore
			int previousSaveNumber = context.getPreviousSaveNumber();
			String oldFileName = "registry-" + Integer.toString(previousSaveNumber);
			File f = myPluginInstance.getStateLocation().append(oldFileName).toFile();
			f.delete();

		}
	}

}


