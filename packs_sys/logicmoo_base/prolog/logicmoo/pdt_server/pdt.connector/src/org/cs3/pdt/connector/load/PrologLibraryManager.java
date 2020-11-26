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

package org.cs3.pdt.connector.load;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;



/**
 * @author lukas
 *
 */
public class PrologLibraryManager {
	/**
	 * maps ids to libraries
	 */
	private HashMap<String, PrologLibrary> libraries = new HashMap<String, PrologLibrary>();

	
	/**
	 * Contains unresolved dependencies
	 * i.e., ids on which other libs depend, but for which
	 * no library is registered.
	 */
	private HashSet<String> unresolvedDependencies = new HashSet<String>();
	
	/**
	 * Contains ids of libs with broken dependencies.
	 * A library is broken, if it has unresolved dependencies
	 * or depends on a broken library.
	 */
	private HashSet<String> brokenLibraries = new HashSet<String>();
	
	public void  check(){
		Set<String> done = new HashSet<String>();
		
		brokenLibraries.clear();
		unresolvedDependencies.clear();		
		Set<String> todo = libraries.keySet();
		for (Iterator<String> it = todo.iterator(); it.hasNext();) {
			String key = it.next();
			check(key, done);
		}
	}
	
	private void check(String key,Set<String> done) {
		if(done.contains(key)){
			return;
		}
		PrologLibrary lib = resolveLibrary(key);
		if(lib==null){
			unresolvedDependencies.add(key);
			return;
		}
		Set<String> dependencies = lib.getDependencies();
		done.add(key);
		for (Iterator<String> it = dependencies.iterator(); it.hasNext();) {
			String dep = it.next();
			check(dep,done);
			if(brokenLibraries.contains(dep)
			||unresolvedDependencies.contains(dep)){
				brokenLibraries.add(key);
			}
		}
	}

	public void addLibrary(PrologLibrary nlib){
		libraries.put(nlib.getId(),nlib);
		check();
	}
	
	public void removeLibrary(PrologLibrary lib){
		libraries.remove(lib.getId());
		check();
	}
	
	public Set<String> getUnresolvedDependencies(){
		return Collections.unmodifiableSet(unresolvedDependencies);
	}

	public Set<String> getBrokenLibraries(){
		return brokenLibraries;
	}
	
	public PrologLibrary resolveLibrary(String id){
		return libraries.get(id);
	}
}


