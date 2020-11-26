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

import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

/**
 * Tests if collection is empty.
 */
public class IsSuccessful<E> extends TypeSafeMatcher<E> {


	@Override
	protected void describeMismatchSafely(E item, Description mismatchDescription) {
		mismatchDescription.appendText("query failed");
	}
	
	@Override
	public void describeTo(Description description) {
		description.appendText("query should succeed");
	}

    /**
     * Matches an not empty query result, the prolog query should succeed.
     */
	@Factory
	public static <E> Matcher<E> succeeds() {
		return new IsSuccessful<E>();
	}

	@Override
	protected boolean matchesSafely(E items) {
		// 22.10.10, dsp, Prolog fail <-> null
        return (items != null);
		// return !Iterables.isEmpty(item);
	}

}


