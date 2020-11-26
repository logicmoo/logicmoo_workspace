/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.connector.internal.preferences;

import org.eclipse.jface.dialogs.DialogMessageArea;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.IPreferencePageContainer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class EditConfigurationDialog extends TrayDialog implements IPreferencePageContainer {

	private IPreferencePage page;

	private DialogMessageArea messageArea;

	/**
	 * The OK button.
	 */
	private Button okButton;

	/**
	 * The Composite in which a page is shown.
	 */
	private Composite pageContainer;

	/**
	 * Flag for the presence of the error message.
	 */
	private boolean showingError = false;

	/**
	 * Preference store, initially <code>null</code> meaning none.
	 * 
	 * @see #setPreferenceStore
	 */
	private IPreferenceStore preferenceStore;

	private Composite titleArea;

    /**
     *  Composite with a FormLayout to contain the title area
     */
    Composite formTitleComposite;

	private ScrolledComposite scrolled;

	/**
	 * Creates a new preference dialog under the control of the given preference
	 * manager.
	 * 
	 * @param parentShell
	 *            the parent shell
	 * @param manager
	 *            the preference manager
	 */
	public EditConfigurationDialog(Shell parentShell, IPreferenceStore preferenceStore, String configurationId) {
		super(parentShell);
		this.preferenceStore = preferenceStore;
		setHelpAvailable(false);
		page = new ConfigurationPreferencePage(configurationId);
		page.setContainer(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		switch (buttonId) {
		case IDialogConstants.OK_ID: {
			okPressed();
			return;
		}
		case IDialogConstants.CANCEL_ID: {
			cancelPressed();
			return;
		}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
	 */
	@Override
	protected void cancelPressed() {
		if (page.performCancel()) {
			super.cancelPressed();
		}
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Edit configuration");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		okButton = createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		getShell().setDefaultButton(okButton);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.window.Window#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(final Composite parent) {
		Control control = super.createContents(parent);
		page.createControl(pageContainer);
		update();
		return control;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		GridLayout parentLayout = ((GridLayout) composite.getLayout());
		parentLayout.numColumns = 4;
		parentLayout.marginHeight = 0;
		parentLayout.marginWidth = 0;
		parentLayout.verticalSpacing = 0;
		parentLayout.horizontalSpacing = 0;
		
		composite.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_LIST_BACKGROUND));
		
		Composite pageAreaComposite = new Composite(composite, SWT.NONE);
		pageAreaComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		GridLayout layout = new GridLayout(1, true);
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.verticalSpacing = 0;
		pageAreaComposite.setLayout(layout);
		
		formTitleComposite = new Composite(pageAreaComposite, SWT.NONE);
		FormLayout titleLayout = new FormLayout();
		titleLayout.marginWidth = 0;
		titleLayout.marginHeight = 0;
		formTitleComposite.setLayout(titleLayout);
		
		GridData titleGridData = new GridData(GridData.FILL_HORIZONTAL);
		titleGridData.horizontalIndent = IDialogConstants.HORIZONTAL_MARGIN;
		formTitleComposite.setLayoutData(titleGridData);
		
		// Build the title area and separator line
		Composite titleComposite = new Composite(formTitleComposite, SWT.NONE);
		layout = new GridLayout();
		layout.marginBottom = 5;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.horizontalSpacing = 0;
		titleComposite.setLayout(layout);
		
		FormData titleFormData = new FormData();
	   	titleFormData.top = new FormAttachment(0,0);
    	titleFormData.left = new FormAttachment(0,0);
    	titleFormData.right = new FormAttachment(100,0);
    	titleFormData.bottom = new FormAttachment(100,0);
		
		titleComposite.setLayoutData(titleFormData);
		createTitleArea(titleComposite);
		
		Label separator = new Label(pageAreaComposite, SWT.HORIZONTAL | SWT.SEPARATOR);

		separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL));
		
		
		// Build the Page container
		pageContainer = createPageContainer(pageAreaComposite);
		GridData pageContainerData = new GridData(GridData.FILL_BOTH);
		pageContainerData.horizontalIndent = IDialogConstants.HORIZONTAL_MARGIN;
		pageContainer.setLayoutData(pageContainerData);
		// Build the separator line
		Label bottomSeparator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
		bottomSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL));
		return composite;
	}


	/**
	 * Creates the inner page container.
	 * 
	 * @param parent
	 * @return Composite
	 */
	protected Composite createPageContainer(Composite parent) {
	
		Composite outer = new Composite(parent, SWT.NONE);
		
		GridData outerData = new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL);
		outerData.horizontalIndent = IDialogConstants.HORIZONTAL_MARGIN;
				
		outer.setLayout(new GridLayout());
		outer.setLayoutData(outerData);
		
		//Create an outer composite for spacing
		scrolled = new ScrolledComposite(outer, SWT.V_SCROLL | SWT.H_SCROLL);

		// always show the focus control
		scrolled.setShowFocusedControl(true);
		scrolled.setExpandHorizontal(true);
		scrolled.setExpandVertical(true);
		
		GridData scrolledData = new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL);
				
		scrolled.setLayoutData(scrolledData);
		
		Composite result = new Composite(scrolled, SWT.NONE);
		
		GridData resultData = new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL
				| GridData.GRAB_VERTICAL);
				
		result.setLayout(new FillLayout());
		result.setLayoutData(resultData);
		
		scrolled.setContent(result);
		
		return result;
	}

	protected Composite createTitleArea(Composite parent) {
		// Create the title area which will contain
		// a title, message, and image.
		int margins = 2;
		titleArea = new Composite(parent, SWT.NONE);
		FormLayout layout = new FormLayout();
		layout.marginHeight = 0;
		layout.marginWidth = margins;
		titleArea.setLayout(layout);

		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.verticalAlignment = SWT.TOP;
		titleArea.setLayoutData(layoutData);

		// Message label
		messageArea = new DialogMessageArea();
		messageArea.createContents(titleArea);

		titleArea.addControlListener(new ControlAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
			 */
			@Override
			public void controlResized(ControlEvent e) {
				updateMessage();
			}
		});

		messageArea.setTitleLayoutData(createMessageAreaData());
		messageArea.setMessageLayoutData(createMessageAreaData());
		return titleArea;
	}

	/**
	 * Create the layout data for the message area.
	 * 
	 * @return FormData for the message area.
	 */
	private FormData createMessageAreaData() {
		FormData messageData = new FormData();
		messageData.top = new FormAttachment(0);
		messageData.bottom = new FormAttachment(100);
		messageData.right = new FormAttachment(100);
		messageData.left = new FormAttachment(0);
		return messageData;
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.IPreferencePageContainer#getPreferenceStore()
	 */
	@Override
	public IPreferenceStore getPreferenceStore() {
		return preferenceStore;
	}

	/**
	 * Notifies that the window's close button was pressed, the close menu was
	 * selected, or the ESCAPE key pressed.
	 * <p>
	 * The default implementation of this framework method sets the window's
	 * return code to <code>CANCEL</code> and closes the window using
	 * <code>close</code>. Subclasses may extend or reimplement.
	 * </p>
	 */
	@Override
	protected void handleShellCloseEvent() {
		// handle the same as pressing cancel
		cancelPressed();
	}

	/**
	 * The preference dialog implementation of this <code>Dialog</code>
	 * framework method sends <code>performOk</code> to all pages of the
	 * preference dialog, then calls <code>handleSave</code> on this dialog to
	 * save any state, and then calls <code>close</code> to close this dialog.
	 */
	@Override
	protected void okPressed() {
		if (page.performOk()) {
			super.okPressed();
		}
	}

	/**
	 * Display the given error message. The currently displayed message is saved
	 * and will be redisplayed when the error message is set to
	 * <code>null</code>.
	 * 
	 * @param newErrorMessage
	 *            the errorMessage to display or <code>null</code>
	 */
	public void setErrorMessage(String newErrorMessage) {
		if (newErrorMessage == null) {
			messageArea.clearErrorMessage();
		} else {
			messageArea.updateText(newErrorMessage, IMessageProvider.ERROR);
		}
	}

	/**
	 * Set the message text. If the message line currently displays an error,
	 * the message is stored and will be shown after a call to clearErrorMessage
	 * <p>
	 * Shortcut for <code>setMessage(newMessage, NONE)</code>
	 * </p>
	 * 
	 * @param newMessage
	 *            the message, or <code>null</code> to clear the message
	 */
	public void setMessage(String newMessage) {
		setMessage(newMessage, IMessageProvider.NONE);
	}

	public void setMessage(String newMessage, int newType) {
		messageArea.updateText(newMessage, newType);
	}


	/**
	 * Updates this dialog's controls to reflect the current page.
	 */
	protected void update() {
		// Update the title bar
		updateTitle();
		// Update the message line
		updateMessage();
		// Update the buttons
		updateButtons();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.IPreferencePageContainer#updateButtons()
	 */
	@Override
	public void updateButtons() {
		okButton.setEnabled(page.isValid());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.preference.IPreferencePageContainer#updateMessage()
	 */
	@Override
	public void updateMessage() {
		String message = null;
		String errorMessage = null;
		if(page != null){
			message = page.getMessage();
			errorMessage = page.getErrorMessage();
		}
		int messageType = IMessageProvider.NONE;
		if (message != null && page instanceof IMessageProvider) {
			messageType = ((IMessageProvider) page).getMessageType();
		}

		if (errorMessage == null){
			if (showingError) {
				// we were previously showing an error
				showingError = false;
			}
		}
		else {
			message = errorMessage;
			messageType = IMessageProvider.ERROR;
			if (!showingError) {
				// we were not previously showing an error
				showingError = true;
			}
		}
		messageArea.updateText(message,messageType);
	}

    @Override
	protected boolean isResizable() {
    	return true;
    }

	@Override
	public void updateTitle() {
		messageArea.showTitle(page.getTitle(), page.getImage());
	}

}
