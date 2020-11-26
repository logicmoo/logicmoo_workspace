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

package org.cs3.pdt.connector;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.connector.internal.DefaultPrologContextTrackerService;
import org.cs3.pdt.connector.internal.preferences.PreferenceConfiguration;
import org.cs3.pdt.connector.internal.service.PrologProcessService;
import org.cs3.pdt.connector.internal.service.ServiceRegistryHook;
import org.cs3.pdt.connector.load.BootstrapPrologContribution;
import org.cs3.pdt.connector.load.BootstrapPrologContributionAlias;
import org.cs3.pdt.connector.load.BootstrapPrologContributionFile;
import org.cs3.pdt.connector.load.BootstrapStartupStrategy;
import org.cs3.pdt.connector.load.DefaultPrologLibrary;
import org.cs3.pdt.connector.load.PrologLibrary;
import org.cs3.pdt.connector.load.PrologLibraryManager;
import org.cs3.pdt.connector.registry.DefaultSAXPrologProcessRegistry;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.registry.RegistryHook;
import org.cs3.pdt.connector.service.IPrologProcessService;
import org.cs3.pdt.connector.subscription.DefaultSubscription;
import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.pdt.connector.util.EclipsePreferenceProvider;
import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PDTConnectorPlugin extends AbstractUIPlugin implements IStartup {
	private final static String PLUGIN_ID = "org.cs3.pdt.connector";

	// The shared instance.
	private static PDTConnectorPlugin plugin;
	// Resource bundle.

	private PrologContextTrackerService contextTrackerService;
	private final static Object contextTrackerMux = new Object();
	private static final Object preferencesMux = new Object();
	

	private Map<String, BootstrapPrologContribution> allBootStrapLists = new HashMap<String, BootstrapPrologContribution>();
	private Map<String, List<BootstrapPrologContribution>> bootStrapContribForKey;
	private PrologProcessRegistry registry;
	private static PrologLibraryManager libraryManager;
	private final static Object libraryManagerMux = new Object();
	private final static Object registryMux = new Object();

	
	public PDTConnectorPlugin() {
		super();
		plugin = this;
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PDTConnectorPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * Returns the id of the PDTConnectorPlugin
	 * @return id of the PDTConnectorPlugin
	 */
	public static String getPluginId() {
		return PLUGIN_ID;
	}

	public PrologContextTrackerService getContextTrackerService() {
		synchronized (contextTrackerMux) {
			if (contextTrackerService == null) {
				contextTrackerService = new DefaultPrologContextTrackerService();
				registerStaticTrackers();
			}
			return contextTrackerService;
		}
	}

	@Override
	public void earlyStartup() {
		/*
		 * if the context tracker service was not requested yet, it does not
		 * contain any then we do not have to initialize the registered
		 * trackers. (the trackers will be initialized when they get registered)
		 * 
		 * But if trackers were registered before the ui was up, we have to
		 * initialize them now.
		 */
		if (contextTrackerService == null) {
			return;
		}
		final IWorkbench workbench = PlatformUI.getWorkbench();
		workbench.getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				PrologContextTracker[] contextTrackers = contextTrackerService.getContextTrackers();
				for (int i = 0; i < contextTrackers.length; i++) {
					contextTrackers[i].init(workbench);
				}
			}
		});
	}


	
	private PrologProcess createPrologProcess(String name, String configuration) {
		PrologProcess prologProcess = null;
		
		prologProcess = Connector.newUninitializedPrologProcess(name);
		prologProcess.setAttribute(PDTConnector.CONFIGURATION_ATTRIBUTE, configuration);
		prologProcess.initOptions(new EclipsePreferenceProvider(this, configuration));
		prologProcess.setStartupStrategy(new BootstrapStartupStrategy());
		
		return prologProcess;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		synchronized (preferencesMux) {

			String value = getPreferenceStore().getString(key);

			return System.getProperty(key, value);
		}
	}

	/**
	 * register all trackers that are defined using the extension point
	 * org.cs3.pdt.runtime.prologContextTracker.
	 * 
	 */
	protected void registerStaticTrackers() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PDTConnector.PLUGIN_ID, Connector.EP_TRACKERS);
		if (point == null) {
			Debug.error("could not find the extension point " + Connector.EP_TRACKERS);
			throw new RuntimeException("could not find the extension point " + Connector.EP_TRACKERS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IConfigurationElement[] celem = extensions[i].getConfigurationElements();
				for (int j = 0; j < celem.length; j++) {

					if (!celem[j].getName().equals("tracker")) {
						Debug.warning("hmmm... asumed a tracker, but got a " + celem[j].getName());
					} else {
						AbstractPrologContextTracker tracker = (AbstractPrologContextTracker) celem[j].createExecutableExtension("class");

						String id = celem[j].getAttribute("id");
						String label = celem[j].getAttribute("label");
						tracker.setLabel(label);
						tracker.setId(id);
						getContextTrackerService().registerPrologContextTracker(tracker);
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}


	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		// remove pdt temp files
		for (File tempFile : Util.getTempFiles()) {
			try {
				tempFile.delete();
			} catch (SecurityException e) {}
		}
		
		try {
			PrologProcessRegistry registry = getPrologProcessRegistry();
			registry.removePrologProcessRegistryListener(ServiceRegistryHook.listener);
			Set<String> keys = new HashSet<String>(registry.getRegisteredKeys()); // clone this. see
			// PDT-194
			Iterator<String> it = keys.iterator();
			while ( it.hasNext()) {
				String key = it.next();
				PrologProcess process = registry.getPrologProcess(key);
				try {
					process.stop();
					registry.removePrologProcess(key);
				} catch (Throwable e) {
					Debug.warning("problems during shutdown of process " + key);
					Debug.report(e);
				}

			}

		} finally {
			super.stop(context);
		}
	}

	/**
	 * get a PrologProcess to a given key. This will create the PrologProcess if
	 * the registry does not contain it, and register it with the registry. No
	 * subscription will be added to the history.
	 * 
	 * @param key
	 * @return
	 * @throws PrologProcessException
	 */
	public PrologProcess getPrologProcess(String key) {
		return getPrologProcess(key, getPreferenceStore().getString(PDTConnector.PREF_CONFIGURATION));
	}
	
	public PrologProcess getPrologProcess(String key, String configuration) {
		DefaultSubscription defaultSubscription = new DefaultSubscription(null, key, null, null);
		return getPrologProcess(defaultSubscription, configuration);
	}

	/**
	 * Subscribe to a PrologProcess. If the registry does not contain a process
	 * for key the subscription is for, this method will create a new process and
	 * register it with the registry.
	 * 
	 * @param s
	 *            The subscription to use. If a subscription with the same id
	 *            exists, it is replaced. Must not be null.
	 * @return the ProlotInterface instance, either from registry, or freshly
	 *         created.
	 * @throws PrologProcessException
	 */
	public PrologProcess getPrologProcess(Subscription s) {
		return getPrologProcess(s, getPreferenceStore().getString(PDTConnector.PREF_CONFIGURATION));
	}
	
	public PrologProcess getPrologProcess(Subscription s, String configuration) {
		PrologProcessRegistry r = getPrologProcessRegistry();
		String processKey = s.getProcessKey();
		PrologProcess process = r.getPrologProcess(processKey);
		boolean addProcessToRegistry = false;
		if (process == null) {
			process = createPrologProcess(processKey, configuration);
			addProcessToRegistry = true;
//			PrologRuntimePlugin.getDefault().addGlobalHooks(processKey, process);
		}
		List<String> contributionKeys = new ArrayList<String>();
		contributionKeys.addAll(s.getBootstrapConstributionKeys());
		if(!contributionKeys.contains("")){
			contributionKeys.add("");
		}
		BootstrapStartupStrategy startupStrategy = null;
		if (process.getStartupStrategy() instanceof BootstrapStartupStrategy) {
			startupStrategy = (BootstrapStartupStrategy) process.getStartupStrategy();
		} else {
			Debug.error("startup strategy is " + process.getStartupStrategy().getClass() + ", but has to be an instance of BootstrapStartupStrategy");
			startupStrategy = new BootstrapStartupStrategy();
			process.setStartupStrategy(startupStrategy);
		}
		for (String contributionKey : contributionKeys) {
			List<BootstrapPrologContribution> libraryList = getBootstrapList(contributionKey);
			for (BootstrapPrologContribution library : libraryList) {
				if (!startupStrategy.contains(library)) {
					startupStrategy.add(library);
					if (process.isUp()) {
						PrologSession session = null;
						try {
							session = process.getSession(PrologProcess.DEFAULT);
							
							String consult = library.getPrologInitStatement();
							Debug.debug("consult " + consult + ", from " + library);
							session.queryOnce(consult);
						} catch (PrologProcessException e) {
							Debug.report(e);
							if (session != null)
								session.dispose();
						}
					}
				}
			}
		}
		if (addProcessToRegistry) {
			r.addPrologProcess(processKey, process);
		}
		if (s.getId() != null) {
			r.addSubscription(s);
		}

		return process;
	}

	/**
	 * Checks if a PrologProcess is registered for the given key.
	 * 
	 * This method may be used by clients who want to check for the existence of
	 * a key in the registry without actually creating a PrologProcess (yet).
	 * 
	 * This is equivalent to calling
	 * <code>getPrologProcessRegistry().getRegisteredKeys().contains(processKey)</code>
	 * 
	 * @param processKey
	 * @return
	 */
	public boolean hasPrologProcess(String processKey) {
		return getPrologProcessRegistry().getRegisteredKeys().contains(processKey);
	}
	
	private IPrologProcessService prologProcessService;
	
	public IPrologProcessService getPrologProcessService() {
		if (prologProcessService == null) {
			prologProcessService = new PrologProcessService();
		}
		return prologProcessService;
	}
	
	public List<String> getPreferenceConfigurations() {
		return PreferenceConfiguration.getInstance().getConfigurations();
	}
	
	
	// the following methods were moved from the prolog connector project
	/*********
	 * Prolog Library related
	 *********/
	public static PrologLibraryManager getLibraryManager() {
		synchronized (libraryManagerMux) {
			if (libraryManager == null) {

				libraryManager = new PrologLibraryManager();
				registerStaticLibraries();
			}

			return libraryManager;
		}
	}
	
	public static String guessFileSearchPath(String libraryId) {
		PrologLibraryManager mgr = getLibraryManager();
		if (mgr == null) {
			return null;
		}
		PrologLibrary lib = mgr.resolveLibrary(libraryId);
		if (lib == null) {
			return null;
		}
		return "library=" + lib.getPath();
	}
	
	private static void registerStaticLibraries() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PDTConnector.PLUGIN_ID, PDTConnector.EP_PROLOG_LIBRARY);
		if (point == null) {
			Debug.error("could not find the extension point " + PDTConnector.EP_PROLOG_LIBRARY);
			throw new RuntimeException("could not find the extension point " + PDTConnector.EP_PROLOG_LIBRARY);
		}
		IExtension[] extensions = point.getExtensions();

		for (int i = 0; i < extensions.length; i++) {
			IExtension ext = extensions[i];
			IConfigurationElement[] configurationElements = ext.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement elm = configurationElements[j];

				String id = elm.getAttribute("id");
				String alias = elm.getAttribute("alias");
				String resName = elm.getAttribute("path");
				Debug.debug("got this resname: " + resName);
				String namespace = ext.getContributor().getName();

				Debug.debug("got this namespace: " + namespace);

				URL url = FileLocator.find(Platform.getBundle(namespace), new Path(resName),null);
				try {

					Debug.debug("trying to resolve this url: " + url);
					url = FileLocator.toFileURL(url);
				} catch (Exception e) {
					Debug.rethrow("Problem resolving url: " + url.toString(), e);
				}
				File file = new File(url.getFile());
				String path = QueryUtils.prologFileName(file);

				IConfigurationElement[] childElms = elm.getChildren();
				Map<String, String> libAttrs = new HashMap<String, String>();
				Set<String> deps = new HashSet<String>();
				for (int k = 0; k < childElms.length; k++) {
					IConfigurationElement childElm = childElms[k];
					if ("dependency".equals(childElm.getName())) {
						deps.add(childElm.getAttribute("library"));
					} else if ("attribute".equals(childElm.getName())) {
						libAttrs.put(childElm.getAttribute("id"), childElm.getAttribute("value"));
					} else {
						Debug.warning("within <library> element, i found an unexpected child element: " + childElm.getName());
					}
				}

				PrologLibrary lib = new DefaultPrologLibrary(id, deps, alias, path, libAttrs);
				getLibraryManager().addLibrary(lib);

			}
		}
	}


//	public void addGlobalHooks(String processKey, PrologProcess process) {
//		Map hooks = getGlobalHooks().get(processKey);
//		if (hooks != null) {
//			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
//				_HookRecord record = it.next();
//				process.addLifeCycleHook(record.hook, record.hookId, record.deps);
//			}
//		}
//		hooks = getGlobalHooks().get("");
//		if (hooks != null) {
//			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
//				_HookRecord record = it.next();
//				process.addLifeCycleHook(record.hook, record.hookId, record.deps);
//			}
//		}
//	}

	/**
	 *  
	 * @return prolog registry
	 */
	public PrologProcessRegistry getPrologProcessRegistry() {
		synchronized (registryMux) {
			if (this.registry == null) {
				this.registry = new DefaultSAXPrologProcessRegistry();
				initRegistry();
			}
			return this.registry;
		}

	}

	private void initRegistry() {
		registerStaticHooks();
	}
	

//	private HashMap<String, Map> getGlobalHooks() {
//		synchronized (globalHooksMux) {
//			if (globalHooks == null) {
//				globalHooks = new HashMap<String, Map>();
//				registerStaticHooks();
//			}
//			return globalHooks;
//		}
//	}
	
//	private static class _HookRecord {
//		LifeCycleHook hook;
//
//		String hookId;
//
//		String[] deps;
//
//		public _HookRecord(LifeCycleHook hook, String hookId, String[] deps) {
//			super();
//			this.hook = hook;
//			this.hookId = hookId;
//			this.deps = deps;
//		}
//
//	}
	protected void registerStaticHooks() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PDTConnector.PLUGIN_ID, PDTConnector.EP_HOOKS);
		if (point == null) {
			Debug.error("could not find the extension point " + PDTConnector.EP_HOOKS);
			throw new RuntimeException("could not find the extension point " + PDTConnector.EP_HOOKS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				IConfigurationElement[] celems = extension.getConfigurationElements();
				for (int j = 0; j < celems.length; j++) {

					final IConfigurationElement celem = celems[j];
					if (celem.getName().equals("registryHook")) {
						RegistryHook hook = (RegistryHook) celem.createExecutableExtension("class");
						hook.addSubscriptions(this.registry);
					} else {
						Debug.warning("hmmm... asumed a registryHook, but got a " + celem.getName());
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}
	

	private Map<String, List<BootstrapPrologContribution>> getBootStrapLists() {
		if (bootStrapContribForKey == null) {
			bootStrapContribForKey = new HashMap<String, List<BootstrapPrologContribution>>();
			registerStaticBootstrapContributions();
		}
		return bootStrapContribForKey;
	}

	private void registerStaticBootstrapContributions() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PDTConnector.PLUGIN_ID, PDTConnector.EP_BOOTSTRAP_CONTRIBUTION);

		if (point == null) {
			Debug.error("could not find the extension point " + PDTConnector.EP_BOOTSTRAP_CONTRIBUTION);
			throw new RuntimeException("could not find the extension point " + PDTConnector.EP_BOOTSTRAP_CONTRIBUTION);
		}

		for (IExtension extension : point.getExtensions()) {
			registerBootstrapContribution(extension);
		}

		for (String key : bootStrapContribForKey.keySet()) {
			HashSet<BootstrapPrologContribution> contributions = new HashSet<BootstrapPrologContribution>(bootStrapContribForKey.get(key));
			
			int lastLen;
			do {
				lastLen = contributions.size();
				for (BootstrapPrologContribution contribution : new ArrayList<BootstrapPrologContribution>(contributions)) {
					for (String dependency : contribution.getDependencies()) {
						BootstrapPrologContribution depContribution = allBootStrapLists.get(dependency );
						if(depContribution == null){
							Debug.error("dependency does not exist: " + dependency +", for contribution " + contribution.getId());
						} else {
							contributions.add(depContribution);
						}
					}
				}
			} while(lastLen < contributions.size());
			
			
			List<BootstrapPrologContribution> sortedContributions = new ArrayList<BootstrapPrologContribution>();
			List<BootstrapPrologContribution> fileContribs = new ArrayList<BootstrapPrologContribution>();

			separateContributions(contributions,sortedContributions,fileContribs);

			topologicalSort(fileContribs);
			sortedContributions.addAll(fileContribs);
			Debug.info("==== Sorted bsc list for key: " + key + " =====");
			for (BootstrapPrologContribution bootstrapPrologContribution : sortedContributions) {
				Debug.info(" - " + bootstrapPrologContribution);
			}
			bootStrapContribForKey.put(key, sortedContributions);
		}
	}
	
		private void separateContributions(
			HashSet<BootstrapPrologContribution> allContribs, List<BootstrapPrologContribution> pathContribs, List<BootstrapPrologContribution> fileContribs) {
		for (BootstrapPrologContribution contribution : allContribs) {
			if(contribution instanceof BootstrapPrologContributionFile){
				fileContribs.add(contribution);
			} else {
				pathContribs.add(contribution);
			}
		}
	}
	

	private class ContributionPredecessors {

		BootstrapPrologContribution contribution;
		int numPredecessors = 0;

		public ContributionPredecessors(BootstrapPrologContribution contribution) {
			this.contribution = contribution;
		}

	}

	private void topologicalSort(List<BootstrapPrologContribution> contributions) {
		Map<String, ContributionPredecessors> predecessors = new HashMap<String, ContributionPredecessors>();
		initPredecessors(contributions, predecessors);
		List<String> remove = new ArrayList<String>();
		topologicalSorting(contributions, predecessors, remove);
		if (predecessors.keySet().size() > 0) {
			throw new RuntimeException("cycle found in bootstrap contribution dependencies: " + contributions);
		}

	}

	private void initPredecessors(
			List<BootstrapPrologContribution> contributions,
			Map<String, ContributionPredecessors> predecessors) {
		for (BootstrapPrologContribution contribution : contributions) {
			ContributionPredecessors current = predecessors.get(contribution.getId());
			if (current != null) {
				throw new RuntimeException("Two bootstrap contributions have the same id: " + contribution.getId());
			}
			current = new ContributionPredecessors(contribution);
			predecessors.put(contribution.getId(), current);
		}
		for (BootstrapPrologContribution contribution : contributions) {
			for (String dependencyId : contribution.getDependencies()) {
				if(allBootStrapLists.get(dependencyId) instanceof BootstrapPrologContributionFile){
					ContributionPredecessors contributionPredecessors = predecessors.get(dependencyId);
					contributionPredecessors.numPredecessors++;
				}
			}
		}
	}
	
	private void topologicalSorting(
			List<BootstrapPrologContribution> contributions,
			Map<String, ContributionPredecessors> predecessors,
			List<String> remove) {
		int counter = 0;
		contributions.clear();
		while (!predecessors.isEmpty() && counter <= contributions.size()) {
			counter++;
			for (String id : predecessors.keySet()) {
				ContributionPredecessors contrib = predecessors.get(id);
				if (contrib.numPredecessors == 0) {
					contributions.add(0, contrib.contribution);
					remove.add(id);
				}
			}
			for (String id : remove) {
				for (String depId : predecessors.get(id).contribution.getDependencies()) {
					if(predecessors.get(depId) != null)// otherwise it will be an alias
						predecessors.get(depId).numPredecessors--;
				}
				predecessors.remove(id);
			}
			remove.clear();
		}
	}

	private void registerBootstrapContribution(IExtension extension) {
		for (IConfigurationElement element : extension.getConfigurationElements()) {
			try {
				addBootstrap(extension, element);
			} catch (RuntimeException e) {
				Debug.report(e);
				/*
				 * DO nothing. Throwing here breaks the "Safe Platform Rule".
				 * The exception is already entered into the PDT log!
				 */
			}
		}
	}

	private void addBootstrap(IExtension extension, IConfigurationElement element) {

		String contributionKey = element.getAttribute("key");
		String contributionId = element.getAttribute("id");
		contributionKey = (contributionKey == null) ? "" : contributionKey;

		List<BootstrapPrologContribution> contribs = createCachedContribsForPrologProcess(contributionKey);

		String resource = element.getAttribute("path");
		String alias = element.getAttribute("alias");

		Set<String> dependencies = getContributionDependencies(element);

		addBootstrapResource(extension, resource, alias,contribs, dependencies, contributionId);
	}

	private Set<String> getContributionDependencies(IConfigurationElement element) {
		Set<String> dependencies = new HashSet<String>();
		for (IConfigurationElement childElm : element.getChildren()) {
			if ("dependency".equals(childElm.getName())) {
				dependencies.add(childElm.getAttribute("contribution"));
			}
		}
		return dependencies;
	}

	private List<BootstrapPrologContribution> createCachedContribsForPrologProcess(String contributionKey) {
		List<BootstrapPrologContribution> contribs = bootStrapContribForKey.get(contributionKey);
		if (contribs == null) {
			contribs = new ArrayList<BootstrapPrologContribution>();
			bootStrapContribForKey.put(contributionKey, contribs);
		}
		return contribs;
	}

	private void addBootstrapResource(IExtension ext, String resource, String alias, List<BootstrapPrologContribution> contribs,
			Set<String> dependencies, String contributionId) {
		Debug.debug("got this resname: " + resource);
		IContributor contributor = ext.getContributor();
		Debug.debug("got this contributor: " + contributor.toString());
		URL url = FileLocator.find(Platform.getBundle(contributor.getName()), new Path(resource), null);
		try {
			Debug.debug("trying to resolve this url: " + url);
			url = FileLocator.toFileURL(url);
		} catch (Exception e) {
			Debug.rethrow("Problem resolving path extension for bootstrapContribution, contributor: " + contributor.getName()
					+ " and resource: " + resource, e);
		}
		File file = new File(url.getFile());
		BootstrapPrologContribution contrib;
		if(alias != null){
			contrib = new BootstrapPrologContributionAlias(contributionId, alias, QueryUtils.prologFileName(file), dependencies);
		} else {
			contrib = new BootstrapPrologContributionFile(contributionId, QueryUtils.prologFileName(file), dependencies);
		}
		contribs.add(contrib);
		allBootStrapLists.put(contributionId, contrib);
	}


	public List<BootstrapPrologContribution> getBootstrapList(String contributionKey) {
		List<BootstrapPrologContribution> r = getBootStrapLists().get(contributionKey);
		return r == null ? new ArrayList<BootstrapPrologContribution>() : r;
	}
	
	
}


