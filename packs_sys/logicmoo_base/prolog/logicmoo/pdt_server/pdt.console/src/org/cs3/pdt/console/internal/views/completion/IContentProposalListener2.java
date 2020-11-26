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

package org.cs3.pdt.console.internal.views.completion;
/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

/**
 * This interface is used to listen to additional notifications from a
 * {@link ContentProposalAdapter}.
 * 
 * @since 3.3
 */
public interface IContentProposalListener2 {
	/**
	 * A content proposal popup has been opened for content proposal assistance.
	 * 
	 * @param adapter
	 *            the ContentProposalAdapter which is providing content proposal
	 *            behavior to a control
	 */
	public void proposalPopupOpened(ContentProposalAdapter adapter);

	/**
	 * A content proposal popup has been closed.
	 * 
	 * @param adapter
	 *            the ContentProposalAdapter which is providing content proposal
	 *            behavior to a control
	 */
	public void proposalPopupClosed(ContentProposalAdapter adapter);
}


