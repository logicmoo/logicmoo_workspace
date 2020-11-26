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

package org.cs3.plunit.matcher;

import static org.hamcrest.core.IsEqual.equalTo;

import java.util.Collection;
import java.util.List;

import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.collection.IsCollectionWithSize;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;

public class CultivateMatcher {

	
	/**
     *  Matches if all arguments are contained in any order in the list
     */
	@Factory
	public static Matcher<List<List<String>>> has(Object[]... strings) {
		return PrologResultsInAnyOrder.has(strings);
	}

	/**
	 * matches if the List<List<String>> object has size n; means prolog returned n answers for the query
	 * @param size
	 */
	@Factory
	public static <E> Matcher<? super List<List<String>>> hasAnswers(int n) {
		return PrologResultsInAnyOrder.<List<String>>hasAnswers(n);
	}

	
	/**
	 * matches if the List<List<String>> object has size n;
	 * @param size
	 */
	@Factory
	public static <E> Matcher<? super Collection<? extends E>> hasSize(int n) {
		return new IsCollectionWithSize<E>(equalTo(n));
	}
	
	/**
     * Matches an empty query result, the prolog query should fail.
     */
	@Factory
	public static <E> Matcher<E> fails() {
		return IsNotSuccessful.<E>fails();
	}
	
	  /**
     * Matches an not empty query result, the prolog query should succeed.
     */
	@Factory
	public static <E> Matcher<E> succeeds() {
		return IsSuccessful.<E>succeeds();
	}
	
	
	/**
	 * This is useful for fluently combining matchers that must both pass.  For example:
	 * <pre>
	 *   assertThat(string, both(containsString("a")).and(containsString("b")));
	 * </pre>
	 */
	public static <T> CombinableMatcher<T> both(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(matcher);
	}

	/**
	 * This is useful for fluently combining matchers where either may pass, for example:
	 * <pre>
	 *   assertThat(string, both(containsString("a")).and(containsString("b")));
	 * </pre>
	 */
	public static <T> CombinableMatcher<T> either(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(matcher);
	}

	@Factory
	public static <T> Matcher<List<List<String>>> hasValues(Object... o){
		return has(values(o));
	}

	@Factory
	public static <T> Matcher<List<List<String>>> hasValue(Object o){
		return has(values(o));
	}

	public static String[] values(Object... objects){
		String[] strings=new String[objects.length];
		for(int i=0;i<objects.length;i++){
			strings[i]=objects[i].toString();
		}
		return strings;
	}

	public static Iterable<String> values(Iterable<Object> objects){
		Iterable<String> strings = Iterables.transform(objects, new Function<Object, String>(){
			@Override
			public String apply(Object from){
				return from.toString();
			}
		});
		return strings;
	}	
	

}


