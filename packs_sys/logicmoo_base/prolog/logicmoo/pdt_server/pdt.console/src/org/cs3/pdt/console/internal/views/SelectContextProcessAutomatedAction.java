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

package org.cs3.pdt.console.internal.views;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.connector.PDTConnector;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.PrologContextTracker;
import org.cs3.pdt.connector.PrologContextTrackerListener;
import org.cs3.pdt.connector.PrologContextTrackerService;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;

/**
 * @author morales
 * 
 */
public abstract class SelectContextProcessAutomatedAction extends Action implements
		IMenuCreator, IWorkbenchWindowPulldownDelegate2,
		PrologContextTrackerListener {

	private Menu createdMenu;
	private Set<String> activeTrackers;
	private IAction fAction;
	private IWorkbenchWindow window;
	private boolean unifiedTrackerEnabled;

	protected abstract void setPrologProcess(PrologProcess prologProcess);
	protected abstract PrologProcess getPrologProcess();
	protected abstract void trackerActivated(PrologContextTracker tracker);
	protected abstract void trackerDeactivated(PrologContextTracker tracker);

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.IMenuCreator#dispose()
	 */
	@Override
	public void dispose() {
		if (createdMenu != null) {
			createdMenu.dispose();
		}
	}

	public SelectContextProcessAutomatedAction() {
		super();

		setText(null);
		setImageDescriptorSensitive();
		setMenuCreator(this);
	}

	void setImageDescriptorSensitive() {
		Set<String> trackers = getActiveTrackers();
		if (trackers.isEmpty()) {
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE_FREE));
		} else {
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.FOLLOW_MODE));
		}
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
	 * .Control)
	 */
	@Override
	public Menu getMenu(Control parent) {
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
		setCreatedMenu(new Menu(parent));
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
		setCreatedMenu(new Menu(parent));
		PrologContextTracker[] trackers = PDTConnectorPlugin.getDefault()
				.getContextTrackerService().getContextTrackers();

		createContextUnifiedAction(getCreatedMenu(), trackers);

		new MenuItem(createdMenu, SWT.SEPARATOR);
		fillMenu();
		initMenu();
		return getCreatedMenu();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
	 * .Menu)
	 */
	@Override
	public Menu getMenu(Menu parent) {
		if (getCreatedMenu() != null) {
			getCreatedMenu().dispose();
		}
		setCreatedMenu(new Menu(parent));
		PrologContextTracker[] trackers = PDTConnectorPlugin.getDefault()
				.getContextTrackerService().getContextTrackers();

		createContextUnifiedAction(getCreatedMenu(), trackers);

		new MenuItem(createdMenu, SWT.SEPARATOR);

		fillMenu();
		initMenu();
		return getCreatedMenu();
	}

	private void initMenu() {
		// Add listener to repopulate the menu each time
		// it is shown to reflect changes in selection or active perspective

		createdMenu.addMenuListener(new MenuAdapter() {
			@Override
			public void menuShown(MenuEvent e) {
				Menu m = (Menu) e.widget;
				MenuItem[] items = m.getItems();
//				for loop starts with index 2 because first two elements of menu are not to be processed here
				for (int i = 2; i < items.length; i++) {
					items[i].dispose();
				}
				fillMenu();
			}
		});

	}

	// process selection part
	private void fillMenu() {
		PrologProcessRegistry reg = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();
		Set<String> keys = reg.getAllKeys();
		List<String> sortedKeys = new ArrayList<String>();
		
		for (String key : keys) {
			if (key != null) {
				sortedKeys.add(key);
			}
		}
		
		ArrayList<ActionContributionItem> actionItems = new ArrayList<ActionContributionItem>();
		for (String key : sortedKeys) {
			ActionContributionItem actionItem = createProcessAction(reg, key);
			if (actionItem != null) {
				actionItems.add(actionItem);
			}
		}
		Collections.<ActionContributionItem>sort(actionItems, new ActionItemComparator());
		Menu menu = getCreatedMenu();
		for (ActionContributionItem actionItem : actionItems) {
			actionItem.fill(menu, -1);
		}
	}

	private void createContextUnifiedAction(Menu parent,
			final PrologContextTracker[] trackers) {
		final MenuItem item = new MenuItem(parent, SWT.CHECK);

		item.setText("Follow Mode");
		unifiedTrackerEnabled = !getActiveTrackers().isEmpty();
		item.setSelection(unifiedTrackerEnabled);
		
		item.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {

			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				MenuItem item = (MenuItem) e.getSource();
				if (item.getSelection()) {
					for (int i = 0; i < trackers.length; i++) {
						final String trackerId = trackers[i].getId();

						getActiveTrackers().add(trackerId);
						trackers[i]
								.addPrologContextTrackerListener(SelectContextProcessAutomatedAction.this);

						trackerActivated(trackers[i]);

						PrologConsolePlugin.getDefault().setPreferenceValue(
								PDTConsole.PREF_CONTEXT_TRACKERS,
								Util.splice(getActiveTrackers(), ","));

					}
					setImageDescriptor(ImageRepository
							.getImageDescriptor(ImageRepository.FOLLOW_MODE));
				} else {
					for (int i = 0; i < trackers.length; i++) {
						final String trackerId = trackers[i].getId();

						if (getActiveTrackers().contains(trackerId)) {
							getActiveTrackers().remove(trackerId);
							trackers[i]
									.removePrologContextTrackerListener(SelectContextProcessAutomatedAction.this);

							trackerDeactivated(trackers[i]);
						}

						PrologConsolePlugin.getDefault().setPreferenceValue(
								PDTConsole.PREF_CONTEXT_TRACKERS,
								Util.splice(getActiveTrackers(), ","));
					}
					setImageDescriptor(ImageRepository
							.getImageDescriptor(ImageRepository.MANUAL_MODE_FREE));
				}

			}
		});

	}

	private ActionContributionItem createProcessAction(PrologProcessRegistry reg, final String key) {
		Set<Subscription> subs = reg.getSubscriptionsForProcess(key);
		if (subs.size() == 0) {
			return null;
		}
		// remove hidden subscriptions from the set
		IAction action = new Action(key, IAction.AS_RADIO_BUTTON) {
			@Override
			public void run() {
				if (this.isChecked())
					setPrologProcess(PDTConnectorPlugin.getDefault()
							.getPrologProcess(key));
				if(!unifiedTrackerEnabled) SelectContextProcessAutomatedAction.this.setImageDescriptor(ImageRepository
						.getImageDescriptor(ImageRepository.MANUAL_MODE));
			}

		};
		action.setChecked(key.equals(reg.getKey(getPrologProcess())));
		action.setText(getLabelForProcess(key, reg));
		action.setEnabled(!unifiedTrackerEnabled);
		ActionContributionItem item = new ActionContributionItem(action);
		return item;
	}

	private void setCreatedMenu(Menu menu) {
		createdMenu = menu;

	}

	private Menu getCreatedMenu() {
		return createdMenu;

	}
	
	Set<String> getActiveTrackers() {
		if (activeTrackers == null) {
			String trackers = PrologConsolePlugin.getDefault().getPreferenceValue(PDTConsole.PREF_CONTEXT_TRACKERS, "");
			activeTrackers = new HashSet<String>();
			Util.split(trackers, ",", activeTrackers);
			
			PrologContextTrackerService trackerService = PDTConnectorPlugin.getDefault().getContextTrackerService();
			for (Iterator<String> iter = activeTrackers.iterator(); iter.hasNext();) {
				String id = iter.next();
				PrologContextTracker contextTracker = trackerService.getContextTracker(id);
				if (contextTracker != null) {
					contextTracker.addPrologContextTrackerListener(this);
				}
			}
		}
		return activeTrackers;
	}

	/**
	 * @see IAction#run()
	 */
	@Override
	public void run() {
		// do nothing, this action just creates a cascading menu.
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		// do nothing - this is just a menu
	}

	@Override
	public void init(IWorkbenchWindow window) {
		this.window = window;

	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if (fAction == null) {
			initialize(action);
		}
	}

	/**
	 * Set the enabled state of the underlying action based on whether there are
	 * any registered launch shortcuts for this launch mode.
	 */
	private void initialize(IAction action) {
		fAction = action;
	}

	public PrologProcess getCurrentPrologProcess() {

		for (Iterator<String> it = getActiveTrackers().iterator(); it.hasNext();) {
			String trackerId = it.next();
			PrologContextTracker tracker = PDTConnectorPlugin.getDefault()
					.getContextTrackerService().getContextTracker(trackerId);
			if (tracker == null) {
				return null;
			}
			PrologProcess process = null;
			process = tracker.getCurrentPrologProcess();
			if (process != null) {
				return process;
			}
		}
		return null;
	}

	public void update() {
		if (window == null) {
			return;
		}
		Display display = window.getShell().getDisplay();
		if (display != Display.getCurrent()) {
			display.asyncExec(new Runnable() {

				@Override
				public void run() {
					update();

				}

			});
			return;
		}
		PrologProcessRegistry reg = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();
		PrologProcess process = getPrologProcess();
		if (process == null) {
			setToolTipText("no process selected");
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE_FREE));
			return;
		}
		String key = reg.getKey(process);
		if (key == null) {
			setToolTipText("unregisterd Prolog process???");
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE_FREE));
			return;
		}
		setToolTipText(key);
		Set<String> trackers = getActiveTrackers();
		if (trackers.isEmpty()) {
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE));
		} else {
			setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.FOLLOW_MODE));
		}

	}
	
	public static String getLabelForProcess(String key, PrologProcessRegistry reg) {
		Set<Subscription> subs = reg.getSubscriptionsForProcess(key);
		
		StringBuffer buf = new StringBuffer();

		Object configuration = reg.getPrologProcess(key).getAttribute(PDTConnector.CONFIGURATION_ATTRIBUTE);
		if (configuration != null) {
			buf.append(configuration.toString().replaceAll("&", "&&"));
			buf.append(": ");
		}
		
		buf.append(key);
		
		if (!subs.isEmpty()) {
			buf.append(" (");
			
			for (Iterator<Subscription> it = subs.iterator(); it.hasNext();) {
				Subscription sub = it.next();
				buf.append(sub.getName());
				if (it.hasNext()) {
					buf.append(", ");
				}
			}
			buf.append(")");
		}
		return buf.toString();
	}
	
	private static class ActionItemComparator implements Comparator<ActionContributionItem> {
		@Override
		public int compare(ActionContributionItem paramT1, ActionContributionItem paramT2) {
			return paramT1.getAction().getText().compareTo(paramT2.getAction().getText());
		}
	}
}

