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

package org.cs3.pdt.editor.internal.views.lightweightOutline;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.custom.BusyIndicator;

class LexicalSortingAction extends Action {

		/**
		 * 
		 */
//		private final PrologOutlineInformationControl prologOutlineInformationControl;

//		private static final String STORE_LEXICAL_SORTING_CHECKED= "LexicalSortingAction.isChecked"; //$NON-NLS-1$

		private TreeViewer fOutlineViewer;

		LexicalSortingAction(/*PrologOutlineInformationControl prologOutlineInformationControl,*/ TreeViewer outlineViewer) {
			super("lexicalsorting"/*TextMessages.JavaOutlineInformationControl_LexicalSortingAction_label*/, IAction.AS_CHECK_BOX);
//			this.prologOutlineInformationControl = prologOutlineInformationControl;
			setToolTipText("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_tooltip);
			setDescription("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_description);


			fOutlineViewer= outlineViewer;

//			boolean checked=getDialogSettings().getBoolean(STORE_LEXICAL_SORTING_CHECKED);
			//TODO
			setChecked(true);
//			PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.LEXICAL_SORTING_BROWSING_ACTION);
		}

		@Override
		public void run() {
			valueChanged(isChecked(), true);
		}

		private void valueChanged(final boolean on, boolean store) {
			setChecked(on);
			BusyIndicator.showWhile(fOutlineViewer.getControl().getDisplay(), new Runnable() {
				@Override
				public void run() {
					fOutlineViewer.refresh(false);
				}
			});

//			if (store)
//				this.prologOutlineInformationControl.getDialogSettings().put(STORE_LEXICAL_SORTING_CHECKED, on);
		}
	}


