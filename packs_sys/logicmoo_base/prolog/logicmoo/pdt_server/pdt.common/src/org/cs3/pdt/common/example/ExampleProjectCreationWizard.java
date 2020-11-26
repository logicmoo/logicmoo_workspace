/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.example;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;
import org.osgi.framework.Bundle;

public class ExampleProjectCreationWizard extends Wizard implements INewWizard, IExecutableExtension {

    private IConfigurationElement fConfigElement;
    private ExampleProjectCreationWizardPage fPage;

    public ExampleProjectCreationWizard() {
        super ();
        setDialogSettings(PDTCommonPlugin.getDefault().getDialogSettings());
        setWindowTitle("PDT Tutorial Project");
        setNeedsProgressMonitor(true);
    }

    private void initializeDefaultPageImageDescriptor(
            IConfigurationElement pageConfigElement) {
        String banner = pageConfigElement.getAttribute("banner"); //$NON-NLS-1$
        if (banner != null) {
            Bundle bundle = Platform.getBundle(pageConfigElement
                    .getContributor().getName());
            setDefaultPageImageDescriptor(createImageDescriptor(bundle, new Path(banner)));
        }
    }

	protected ImageDescriptor createImageDescriptor(Bundle bundle, IPath path) {
		URL url = FileLocator.find(bundle, path, null);
		return ImageDescriptor.createFromURL(url);
	}

    /*
     * @see Wizard#addPages
     */
    @Override
	public void addPages() {
        super .addPages();
        if (fPage != null) {
            addPage(fPage);
        }
    }

    /*
     * @see Wizard#performFinish
     */
    @Override
	public boolean performFinish() {
        if (fPage != null) {
        	ExampleProjectCreationWizardPage[] pages = new ExampleProjectCreationWizardPage[1];
        	pages[0] = fPage; 
            ExampleProjectCreationOperation runnable = new ExampleProjectCreationOperation(
                    pages, new ImportOverwriteQuery());

            IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation(
                    runnable);
            try {
                getContainer().run(false, true, op);
            } catch (InvocationTargetException e) {
                handleException(e.getTargetException());
                return false;
            } catch (InterruptedException e) {
                return false;
            }
            BasicNewProjectResourceWizard
                    .updatePerspective(fConfigElement);
            IResource res = runnable.getFileToOpen();
            if (res != null) {
                openResource(res);
            }
            
            String url = runnable.getUrlToOpen();
            if (url != null) {
            	try {
            		PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser().openURL(new URL(url));
            	} catch (PartInitException e ) {
            		e.printStackTrace();
            	} catch (MalformedURLException e) {
            		e.printStackTrace();
            	}
		    }

        }
        return true;
    }

    private void handleException(Throwable target) {
        String title = "Example Project Creation Failed";
        String message = "Could not create PDT Tutorial Project";
        if (target instanceof  CoreException) {
            IStatus status = ((CoreException) target).getStatus();
            ErrorDialog.openError(getShell(), title, message, status);
            Debug.error(status.getMessage());
        } else {
            MessageDialog.openError(getShell(), title, target
                    .getMessage());
            Debug.error(target.getMessage());
        }
    }

    private void openResource(final IResource resource) {
        if (resource.getType() != IResource.FILE) {
            return;
        }
        IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        if (window == null) {
            return;
        }
        final IWorkbenchPage activePage = window.getActivePage();
        if (activePage != null) {
            final Display display = getShell().getDisplay();
            display.asyncExec(new Runnable() {
                @Override
				public void run() {
                    try {
                        IDE.openEditor(activePage, (IFile) resource,
                                true);
                    } catch (PartInitException e) {
                    	Debug.debug(e.getMessage());
                    }
                }
            });
            BasicNewResourceWizard.selectAndReveal(resource, activePage
                    .getWorkbenchWindow());
        }
    }

    /*
     * Stores the configuration element for the wizard.  The config element will be used
     * in <code>performFinish</code> to set the result perspective.
     */
    @Override
    public void setInitializationData(IConfigurationElement cfig,
    		String propertyName, Object data) {
    	fConfigElement = cfig;
    	fPage = new ExampleProjectCreationWizardPage();
    	initializeDefaultPageImageDescriptor(cfig);
    }

    // overwrite dialog

    private class ImportOverwriteQuery implements  IOverwriteQuery {
        protected static final String CREATE_TUTORIAL_PROJECT = "Create PDT Tutorial Project";

		@Override
		public String queryOverwrite(String file) {
            String[] returnCodes = { YES, NO, ALL, CANCEL };
            int returnVal = openDialog(file);
            return returnVal < 0 ? CANCEL : returnCodes[returnVal];
        }

        private int openDialog(final String file) {
            final int[] result = { IDialogConstants.CANCEL_ID };
            getShell().getDisplay().syncExec(new Runnable() {
                @Override
				public void run() {
                    String title = CREATE_TUTORIAL_PROJECT;
                    String msg = MessageFormat
                            .format(
                            		CREATE_TUTORIAL_PROJECT,
                                    new Object[] { file });
                    String[] options = { IDialogConstants.YES_LABEL,
                            IDialogConstants.NO_LABEL,
                            IDialogConstants.YES_TO_ALL_LABEL,
                            IDialogConstants.CANCEL_LABEL };
                    MessageDialog dialog = new MessageDialog(
                            getShell(), title, null, msg,
                            MessageDialog.QUESTION, options, 0);
                    result[0] = dialog.open();
                }
            });
            return result[0];
        }
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
	public void init(IWorkbench workbench,
            IStructuredSelection selection) {

    }
}
