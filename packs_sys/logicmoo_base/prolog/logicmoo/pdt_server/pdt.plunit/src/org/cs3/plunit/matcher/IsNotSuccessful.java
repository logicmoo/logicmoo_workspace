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

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;

/**
 * Tests that an Iterable<String> representing a Prolog Result is null
 * which means by conventions that it was not successful.
 */
public class IsNotSuccessful<E> extends BaseMatcher<E> {

	@Override
	public boolean matches(Object items) {
		return (items == null);
	}
	
	@Override
	public void describeTo(Description description) {
		description.appendText("query should fail");
	}

    /**
     * Matches a null representing a failing Prolog query.
     */
	@Factory
	public static <E> Matcher<E> fails() {
		return new IsNotSuccessful<E>();
	}

}


