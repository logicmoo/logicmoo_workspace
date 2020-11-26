/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: null (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.common;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.connector.process.PrologProcess;

/**
 * Contains utility methods for simplifying the creation of Prolog queries.
 */
public class QueryUtils {
	
	/**
	 * Build a term with the given functor and arguments. Iterables are
	 * automatically converted to Prolog lists.
	 * 
	 * @param functor
	 *            functor of the term
	 * @param args
	 *            arguments of the term
	 * @return query String ready to handover to
	 *         {@link PrologProcess#queryOnce(String...)} or
	 *         {@link PrologProcess#queryAll(String...)}
	 */
	public static String bT(String functor, Object...args) {
		return buildTerm(functor, args);
	}
	
	/**
	 * Build a term with the given functor and arguments. Iterables are
	 * automatically converted to Prolog lists.
	 * 
	 * @param functor
	 *            functor of the term
	 * @param args
	 *            arguments of the term
	 * @return query String ready to handover to
	 *         {@link PrologProcess#queryOnce(String...)} or
	 *         {@link PrologProcess#queryAll(String...)}
	 */
	public static String buildTerm(String functor, Object...args) {
		
		if(args == null || args.length == 0 || (args.length == 1 && args.toString().equals(""))) {
			return functor;
		}
		
		StringBuffer puffer = new StringBuffer();
		puffer.append(functor);
		puffer.append("(");
		
		if(args.length == 1) {
			puffer.append(args[0].toString());
		} else {
			for(Object arg:args) {
				if(arg instanceof Iterable<?>) {
					puffer.append(listToArgList((Iterable<?>)arg));
				} else {
					puffer.append(arg.toString());
				}
				puffer.append(",");
			}
			puffer.delete(puffer.length()-1, puffer.length());
		}
		puffer.append(")");
		return puffer.toString();
	}
	
	/**
	 * Converts an Iterable to a Prolog list like "[ , , ]"
	 * 
	 * @param elements
	 *            Iterable or subclass of Iterable
	 * @return elements of the Iterable wrapped in "[]" and separated by ","
	 */
	public static String listToArgList(Iterable<?> elements) {
		if (elements == null) {
			return "[]";
		} 
		
		StringBuilder buf = new StringBuilder();
		buf.append('[');
		
		boolean first = true;
		for (Object element : elements) {
			if (first) {
				first = false;
			} else {
				buf.append(',');
			}
			buf.append(element);
		}
		buf.append(']');
		return buf.toString();
	}
	
	/**
	 * Converts a given amount of Objects to a single String representing a list
	 * in Prolog notation.
	 * 
	 * @param objects
	 *            an arbitrary number of objects
	 * @return objects wrapped in "[]" and separated by ","
	 */
	public static String objectsToArgList(Object...objects) {
		
		List<Object> temp = new ArrayList<Object>();
		
		for(Object obj:objects) {
			temp.add(obj);
		}
		
		return listToArgList(temp);
	}

	/**
	 * Normalizes String for a Windows system.
	 * 
	 * @param s
	 *            the String
	 * @return normalized String
	 */
	public static String normalizeOnWindows(String s) {
		boolean windowsPlattform = Util.isWindows();
		if (windowsPlattform) {
			s = s.replace('\\', '/').toLowerCase();
		}
		return s;
	}

	/**
	 * Converts a given file into a Prolog filename.
	 * 
	 * @param f
	 *            the file
	 * @return normalized path to the file
	 */
	public static String prologFileName(File f) {
		try {
			return normalizeOnWindows(f.getCanonicalPath());
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage());
		}
	}
	
	/**
	 * Converts a given file into a Prolog filename and wraps the filename with
	 * single quotes.
	 * 
	 * @param f
	 *            the file
	 * @return normalized and quoted path to the file
	 */
	public static String prologFileNameQuoted(File f) {
		return quoteAtom(prologFileName(f));
	}

	/**
	 * Wraps an atom with single quotes and escapes all single quotes contained
	 * in the atom.
	 * 
	 * @param term
	 *            unquoted atom
	 * @return quoted atom
	 */
	public static String quoteAtom(String term) {
	
		return "'" + term.replace("'", "\\'") + "'";
	}

	/**
	 * Wraps an atom with single quotes (if it is not already quoted) and
	 * escapes all single quotes contained in the atom.
	 * 
	 * @param term
	 *            atom (quoted or unquoted)
	 * @return quoted atom
	 */
	
	public static String quoteAtomIfNeeded(String term) {
		if (term.startsWith("'") && term.endsWith("'")) {
			return term;
		} else {
			return "'" + term.replace("'", "\\'") + "'";
		}
	}


}

