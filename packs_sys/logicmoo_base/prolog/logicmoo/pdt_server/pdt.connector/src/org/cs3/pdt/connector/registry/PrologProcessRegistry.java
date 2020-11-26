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

package org.cs3.pdt.connector.registry;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Set;

import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;

/**
 * Central registry for managing PrologProcess instances and Subscriptions.
 * 
 * The registry keeps track of every PrologProcess created by the pdt.runtime
 * plugin. In addition, clients can register Subscriptions to particular
 * PrologProcess instances, thereby publically announcing what they intend to use them for.
 * 
 * This registry is ment to be a kind of "forum" for clients that need to share one and the same PrologProcess instance.
 * It is also intended to provide a model for ui components that need to provide the user with a choice of available Prolog runtimes.
 * 
 * Note that adding a Subscription means subscribing to a particular PrologProcess _KEY_ rather than to the instance itself.
 * Among other things this allows clients to express ther instance in a particular PrologProcess before it actually exists.
 * This is important since PrologProcess instances typically get created in a lazy fashion, whereas the ui should be able 
 * to reflect subscriptions much earlier to help the user understand her environment better. 
 * 
 * @author lukas
 */
public interface PrologProcessRegistry {

	/**
	 * @return all keys to which PrologProcesss are registered.
	 */
	public Set<String> getRegisteredKeys();

	/**
	 * @return all process keys that are known to the registry, including keys for which
	 *         no process is registered.
	 */
	public Set<String> getAllKeys();

	/**
	 * @return the IDs of all subscriptions registered with the registry..
	 */
	public Set<String> getAllSubscriptionIDs();

	/**
	 * 
	 * @return all subscriptions to a given process key
	 */
	public Set<Subscription> getSubscriptionsForProcess(String key);

	

	
	/**
	 * retrieve the registry key of a registered PrologProcess. *
	 */
	public String getKey(PrologProcess prologProcess);

	/**
	 * retrieve the PrologProcess instance registered for the given key.
	 */
	public PrologProcess getPrologProcess(String key);

	/**
	 * add a listener to this registry.
	 * 
	 * Listeners get notified whenever a PrologProcess instance or
	 * Subscription is registered or unregistered.
	 * 
	 * @param l
	 */
	public void addPrologProcessRegistryListener(
			PrologProcessRegistryListener l);

	/**
	 * remove a listener from this registry.
	 * 
	 * @param l
	 */
	public void removePrologProcessRegistryListener(
			PrologProcessRegistryListener l);

	
	/**
	 * Register a PrologProcess with this registry.
	 * 
	 * If another PrologProcess is already registered with this key,
	 * it is removed first.
	 * 
	 * If the same PrologInterace is already registered with this key,
	 * this method has no effect.
	 * 
	 * This method will cause a call to the method configure() on any waiting
	 * subscriptions that are already registered for the given processkey.
	 * 
	 * For each distinct tuple ( Data, Hook descriptor) where
	 *  - Data is the return value of the getData() method called on a subscription 
	 *    that is registered for the given key
	 *  - Hook Descriptor is a registered hook descriptor that has at least one
	 *    tag in common with this subscription
	 * a hook will be created using named hook descriptor, it will be parameterized with 
	 * the user data and will be registered with the prolog interface instance.
	 * @param key
	 * @param process
	 */
	public void addPrologProcess(String key, PrologProcess process) ;

	/**
	 * Remove a PrologProcess from this registry.
	 * 
	 * Removes the PrologProcess with the given key. If no PrologProcess was
	 * registered for that key, this method has no effect. Subscriptions will
	 * NOT be removed.
	 * 
	 * This method will cause a call to the method deconfigure() on any
	 * subscription registered for the given process key.
	 * All hooks that were added by the registry are removed from the prolog interface.
	 * @param key
	 * 
	 */
	public void removePrologProcess(String key) throws PrologProcessException;

	/**
	 * Add a subscription to the registry.
	 * 
	 * If there is already a Subscription with the same key, it will be removed
	 * first.
	 * 
	 * If the same subscription is already registered, this method has no effect.
	 * 
	 * If there is already a PrologProcess instance registered for the
	 * subscriptions processKey, this method will cause a call to the method
	 * configure() on the argument Subscription instance. 
	 * In addition, for each registered Hook Descriptor that has at least one
	 * tag in common with the subscription, said descriptor will be used to 
	 * create a hook, parameterize it with the return value of the method
	 * getData() called on the subscription and add register it with the process. 
	 * 
	 * If the argument Subscription is an instance of PersistableSubscription,
	 * the registry will take the neccesary steps to save the subscription on
	 * workbench shutdown and restore it on the next startup.
	 * 
	 * @param s
	 */
	public void addSubscription(Subscription s) ;

	/**
	 * Remove a subscription from the registry.
	 * 
	 * If there is currently a PrologProcess instance registered for the
	 * subscriptions processKey, this method will cause a call to the method
	 * deconfigure() on the argument Subscription instance.
	 * In addition all hooks that were registered to this prolog interface 
	 * because of this subscription will be removed, unless they are still
	 * required because of some other registered subscription.
	 * 
	 * @param s
	 */
	public void removeSubscription(Subscription s);

	/**
	 * Remove a subscription from the registry.
	 * 
	 * Removes the subscription with the given subscription id.
	 * 
	 * @see removeSubscription(Subscription)
	 * @param id
	 */
	public void removeSubscription(String id);
	
	

	
	/**
	 * Find the Subscription for a given subscription id;
	 * 
	 * @param id
	 * @return the Subscription or null if none was registered with this id.
	 */
	public Subscription getSubscription(String id);

	/**
	 * Load a saved registry.
	 * @param r
	 */
	void load(Reader r) throws IOException;

	/**
	 * Save the current registry.
	 * @param w
	 */
	public void save(Writer w) throws IOException;
}


