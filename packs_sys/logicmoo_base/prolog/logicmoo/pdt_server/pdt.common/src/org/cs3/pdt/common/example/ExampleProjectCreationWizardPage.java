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

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

public class ExampleProjectCreationWizardPage extends WizardPage {

	private static final String EXAMPLES_URL = "http://sewiki.iai.uni-bonn.de/_media/research/pdt/examples/examples.xml";
	private Text txDescription;
	private ArrayList<ExampleProject> examples;
	private ArrayList<ExampleProject> selectedProjects = new ArrayList<ExampleProject>();
	private Tree tree;

	public ExampleProjectCreationWizardPage() {
		super("PDT Example Project");

		setTitle("Create PDT Example Project");
		setDescription("Create a project containing Prolog sample files.");

		setPageComplete(false);
	}

	@Override
	public void createControl(Composite parent) {
		setPageComplete(false);
		setErrorMessage(null);
		setMessage(null);

		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(1, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		setControl(composite);
		Group groupSelectProject = new Group(composite, SWT.NONE);
		groupSelectProject.setLayout(new GridLayout());
		groupSelectProject.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		groupSelectProject.setText("Select project set(s)");

		initExamples();

		tree = new Tree(groupSelectProject, SWT.CHECK | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		tree.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				if (event.detail == SWT.CHECK) {

					TreeItem item = (TreeItem) event.item;
					if (item.getGrayed()) {
						item.setChecked(false);
						return;
					}
					setChildrenChecked(item, item.getChecked());
					setPageComplete(!selectedProjects.isEmpty());

				} else {
					Object data = event.item.getData();
					if (data instanceof ExampleProject) {
						String desc = ((ExampleProject) data).getDescription();
						desc = desc.replace("\\n", "\n");
						txDescription.setText(desc);
					} else {
						txDescription.setText("");
					}
				}
			}
			
			private void setChildrenChecked(TreeItem item, boolean checked) {
				item.setChecked(checked);
				Object data = item.getData();
				if (data instanceof ExampleProject) {
					if (checked) {
						selectedProjects.add((ExampleProject) data);
					} else {
						selectedProjects.remove(data);
					}
				} else {
					for (TreeItem child : item.getItems()) {
						setChildrenChecked(child, checked);
					}
				}
			}
		});

		for (ExampleProject p : examples) {
			insertExampleIntoTree(p);
		}
		expandTree();

		Group groupDescription = new Group(composite, SWT.NONE);
		groupDescription.setLayout(new GridLayout());
		groupDescription.setText("Description");
		groupDescription.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		txDescription = new Text(groupDescription, SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
		txDescription.setEditable(false);
		txDescription.setBackground(parent.getBackground());
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.heightHint = 30;
		txDescription.setLayoutData(gd);
	}
	
	private void insertExampleIntoTree(ExampleProject project) {
		if (project.getTreePath().length == 0) {
			TreeItem item = new TreeItem(tree, SWT.NONE);
			setTreeItemTextAndData(item, project);
			item.setData(project);
		} else {
			TreeItem parent = null;
			boolean first = true;
			for (String segment : project.getTreePath()) {
				if (first) {
					TreeItem i = findTreeItem(tree.getItems(), segment);
					if (i == null) {
						i = new TreeItem(tree, SWT.NONE);
						i.setText(segment);
						i.setData(segment);
					}
					parent = i;
				} else {
					TreeItem i = findTreeItem(parent.getItems(), segment);
					if (i == null) {
						i = new TreeItem(parent, SWT.NONE);
						i.setText(segment);
						i.setData(segment);
					}
					parent = i;
				}
			}
			TreeItem item = new TreeItem(parent, SWT.NONE);
			setTreeItemTextAndData(item, project);
			item.setData(project);
		}
	}
	
	private TreeItem findTreeItem(TreeItem[] items, String data) {
		for (TreeItem item : items) {
			if (data.equals(item.getData())) {
				return item;
			}
		}
		return null;
	}

	private void setTreeItemTextAndData(TreeItem item, ExampleProject p) {
		String append = "";
		if (!p.downloadAvailable()) {
			if (p.needsDownload()) {
				item.setGrayed(true);
			}
		} else {
			if (p.needsDownload()) {
				append = " (Download)";
			} else if (p.needsUpdate()) {
				append = " (Update)";
			}
		}
		item.setData(p);
		item.setText(p.getName() + append);
	}
	
	private void expandTree() {
		for (TreeItem child : tree.getItems()) {
			expandItemAndChildren(child);
		}
	}
	
	private void expandItemAndChildren(TreeItem item) {
		item.setExpanded(true);
		for (TreeItem child : item.getItems()) {
			expandItemAndChildren(child);
		}
	}

	private void initExamples() {
		examples = new ArrayList<ExampleProject>();

		XmlReader xmlReader = new XmlReader(EXAMPLES_URL, examples);
		xmlReader.parse();
	}

	/*
	 * Set the default project name that is to appear on the initialPage page of
	 * this wizard.
	 */
	protected String calculateInitialProjectName(String projectName) {
		IProject projectHandle = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		if (!projectHandle.exists()) {
			return projectName;
		}
		// Change the name until it doesn't exists. Try 9 times and then
		// give up.
		for (int i = 1; i < 10; i++) {
			projectHandle = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName + i);
			if (!projectHandle.exists()) {
				return projectName + i;
			}
		}
		return projectName + "9";

	}

	protected boolean validatePage() {
		return true;
	}

	public ArrayList<ExampleProject> getSelectedExampleProjects() {
		return new ArrayList<ExampleProject>(selectedProjects);
	}

}
