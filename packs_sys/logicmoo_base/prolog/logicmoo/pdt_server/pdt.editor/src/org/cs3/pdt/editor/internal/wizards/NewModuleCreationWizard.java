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

package org.cs3.pdt.editor.internal.wizards;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.part.ISetSelectionTarget;


public class NewModuleCreationWizard extends Wizard implements INewWizard {

	private NewModuleCreationWizardPage fPage = null;

	public NewModuleCreationWizard() {
		super();
//		setDefaultPageImageDescriptor(AspectJImages.W_NEW_ASPECT
//				.getImageDescriptor());
//		setDialogSettings(AspectJUIPlugin.getDefault().getDialogSettings());
		setWindowTitle("Prolog Module Wizard");
	}

	/**
	 * Adds the NewAspectWizardCreationPage. If the page has already been added,
	 * this method does nothing (extra pages can be added by calling
	 * <code> addPage()
	 * </code>
	 */
	@Override
	public void addPages() {
		setNeedsProgressMonitor(true);

		// Only initialse if the member is currently null - necessary for
		// testing - spyoung
		if (fPage == null) {
			fPage = new NewModuleCreationWizardPage("New Module",getSelection());
			addPage(fPage);			
			//fPage.init(getSelection());
		}
	}
	
	   /**
     * The workbench.
     */
    private IWorkbench workbench;

    /**
     * The current selection.
     */
    protected IStructuredSelection selection;

    /**
     * Returns the selection which was passed to <code>init</code>.
     *
     * @return the selection
     */
    public IStructuredSelection getSelection() {
        return selection;
    }

    /**
     * Returns the workbench which was passed to <code>init</code>.
     *
     * @return the workbench
     */
    public IWorkbench getWorkbench() {
        return workbench;
    }

	/**
	 * Complete generation of the new file, open it in the associated editor,
	 * and open the Cross References view, if desired.
	 */
	@Override
	public boolean performFinish() {
		
//		try {
			IFile file = fPage.createNewFile();
			
			//selectAndReveal(file);
				if (file  != null) {
					try {
						PDTCommonUtil.openInEditor(file, true);
					} catch (PartInitException e) {
						e.printStackTrace();
					}
				}
			return true;
//		} catch (CoreException e) {
//			e.printStackTrace();
//			return false;
//		} 
//		
	}


    @Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
        this.workbench = workbench;
        this.selection = currentSelection;

        initializeDefaultPageImageDescriptor();
    }
    
    /**
     * Initializes the default page image descriptor to an appropriate banner.
     * By calling <code>setDefaultPageImageDescriptor</code>.
     * The default implementation of this method uses a generic new wizard image.
     * <p>
     * Subclasses may reimplement.
     * </p>
     */
    protected void initializeDefaultPageImageDescriptor() {
		ImageDescriptor desc = IDEWorkbenchPlugin.getIDEImageDescriptor("wizban/new_wiz.png");//$NON-NLS-1$
        setDefaultPageImageDescriptor(desc);
    }

    /**
     * Selects and reveals the newly added resource in all parts
     * of the active workbench window's active page.
     *
     * @see ISetSelectionTarget
     */
    protected void selectAndReveal(IResource newResource) {
        selectAndReveal(newResource, getWorkbench().getActiveWorkbenchWindow());
    }

    /**
     * Attempts to select and reveal the specified resource in all
     * parts within the supplied workbench window's active page.
     * <p>
     * Checks all parts in the active page to see if they implement <code>ISetSelectionTarget</code>,
     * either directly or as an adapter. If so, tells the part to select and reveal the
     * specified resource.
     * </p>
     *
     * @param resource the resource to be selected and revealed
     * @param window the workbench window to select and reveal the resource
     * 
     * @see ISetSelectionTarget
     */
    public static void selectAndReveal(IResource resource,
            IWorkbenchWindow window) {
        // validate the input
        if (window == null || resource == null) {
			return;
		}
        IWorkbenchPage page = window.getActivePage();
        if (page == null) {
			return;
		}

        // get all the view and editor parts
        List<IWorkbenchPart> parts = new ArrayList<IWorkbenchPart>();
        IWorkbenchPartReference refs[] = page.getViewReferences();
        for (int i = 0; i < refs.length; i++) {
            IWorkbenchPart part = refs[i].getPart(false);
            if (part != null) {
				parts.add(part);
			}
        }
        refs = page.getEditorReferences();
        for (int i = 0; i < refs.length; i++) {
            if (refs[i].getPart(false) != null) {
				parts.add(refs[i].getPart(false));
			}
        }

        final ISelection selection = new StructuredSelection(resource);
        Iterator<IWorkbenchPart> itr = parts.iterator();
        while (itr.hasNext()) {
            IWorkbenchPart part = (IWorkbenchPart) itr.next();

            // get the part's ISetSelectionTarget implementation
            ISetSelectionTarget target = null;
            if (part instanceof ISetSelectionTarget) {
				target = (ISetSelectionTarget) part;
			} else {
				target = (ISetSelectionTarget) part
                        .getAdapter(ISetSelectionTarget.class);
			}

            if (target != null) {
                // select and reveal resource
                final ISetSelectionTarget finalTarget = target;
                window.getShell().getDisplay().asyncExec(new Runnable() {
                    @Override
					public void run() {
                        finalTarget.selectReveal(selection);
                    }
                });
            }
        }
    }
}


