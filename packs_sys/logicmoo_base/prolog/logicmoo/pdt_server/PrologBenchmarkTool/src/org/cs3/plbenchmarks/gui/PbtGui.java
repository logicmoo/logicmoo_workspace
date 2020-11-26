package org.cs3.plbenchmarks.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.cs3.plbenchmarks.PbtController;
import org.cs3.plbenchmarks.PbtRunner;
import org.cs3.plbenchmarks.data.Configuration;
import org.cs3.plbenchmarks.data.FactbaseVersionPair;
import org.cs3.plbenchmarks.utils.IOUtils;

public class PbtGui extends JFrame implements MouseListener {

	private static final long serialVersionUID = 1L;
	
	private JPanel contentPane;
	protected JTextField tfLoadFile;
	protected JTextField tfOutputFile;
	private JEditorPane tfBenchmarkCode;
	protected DefaultListModel<Configuration> batchModel;
	protected JTable table;
	private boolean valuesChanged;

	private PbtController controller;
	private JTextField tfName;

	protected PbtTableModel tableModel;
	
	/**
	 * Create the frame.
	 * @throws IOException 
	 */
	public PbtGui() {
		valuesChanged = false;
		controller = new PbtController();
		
	    try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}
		 
		setTitle("Prolog Benchmark Tool");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 703, 646);
		contentPane = new JPanel();
		
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);

		JPanel panel_2 = new JPanel();
		
		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(null, "Configuration", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		
		tfLoadFile = new JTextField();
		tfLoadFile.setColumns(10);
		
		JLabel lbLoadFile = new JLabel("Load file:");
		
		JButton button = new JButton("...");
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				selectFile(tfLoadFile);
			}
		});
		
		tfOutputFile = new JTextField();
		tfOutputFile.setText(controller.getOutputFile());
		tfOutputFile.setColumns(10);
		
		JButton button_1 = new JButton("...");
		button_1.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				selectFile(tfOutputFile);
			}
		});
		
		JLabel lblOutputFile = new JLabel("Output file:");
		
		tableModel = new PbtTableModel(controller);
		table = new JTable( tableModel );
		table.addMouseListener(this);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		JScrollPane tableScroll = new JScrollPane(table);
		
		tfName = new JTextField();
		tfName.setColumns(10);
		
		JLabel lblName = new JLabel("Name:");
		
		chckbxJt = new JCheckBox("JT");
		
		chckbxPdt = new JCheckBox("PDT");
		chckbxPdt.setSelected(true);
		chckbxPdt.setEnabled(false);
		GroupLayout gl_panel = new GroupLayout(panel);
		gl_panel.setHorizontalGroup(
			gl_panel.createParallelGroup(Alignment.TRAILING)
				.addGroup(gl_panel.createSequentialGroup()
					.addContainerGap()
					.addGroup(gl_panel.createParallelGroup(Alignment.TRAILING)
						.addComponent(tableScroll, Alignment.LEADING, GroupLayout.DEFAULT_SIZE, 437, Short.MAX_VALUE)
						.addGroup(gl_panel.createSequentialGroup()
							.addGroup(gl_panel.createParallelGroup(Alignment.LEADING)
								.addComponent(lblOutputFile)
								.addComponent(lblName)
								.addComponent(lbLoadFile))
							.addGap(21)
							.addGroup(gl_panel.createParallelGroup(Alignment.TRAILING)
								.addComponent(tfOutputFile, GroupLayout.DEFAULT_SIZE, 306, Short.MAX_VALUE)
								.addComponent(tfName, Alignment.LEADING, GroupLayout.DEFAULT_SIZE, 306, Short.MAX_VALUE)
								.addGroup(gl_panel.createSequentialGroup()
									.addComponent(tfLoadFile, GroupLayout.DEFAULT_SIZE, 161, Short.MAX_VALUE)
									.addPreferredGap(ComponentPlacement.UNRELATED)
									.addComponent(button)
									.addPreferredGap(ComponentPlacement.UNRELATED)
									.addComponent(chckbxJt)
									.addPreferredGap(ComponentPlacement.UNRELATED)
									.addComponent(chckbxPdt)))
							.addPreferredGap(ComponentPlacement.UNRELATED)
							.addComponent(button_1, GroupLayout.PREFERRED_SIZE, 45, GroupLayout.PREFERRED_SIZE)))
					.addContainerGap())
		);
		gl_panel.setVerticalGroup(
			gl_panel.createParallelGroup(Alignment.TRAILING)
				.addGroup(gl_panel.createSequentialGroup()
					.addGap(12)
					.addComponent(tableScroll, GroupLayout.DEFAULT_SIZE, 298, Short.MAX_VALUE)
					.addPreferredGap(ComponentPlacement.UNRELATED)
					.addGroup(gl_panel.createParallelGroup(Alignment.BASELINE)
						.addComponent(tfName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
						.addComponent(lblName))
					.addPreferredGap(ComponentPlacement.RELATED)
					.addGroup(gl_panel.createParallelGroup(Alignment.LEADING)
						.addGroup(gl_panel.createParallelGroup(Alignment.BASELINE)
							.addComponent(tfLoadFile, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
							.addComponent(chckbxPdt)
							.addComponent(chckbxJt)
							.addComponent(button))
						.addComponent(lbLoadFile))
					.addGap(3)
					.addGroup(gl_panel.createParallelGroup(Alignment.BASELINE)
						.addComponent(tfOutputFile, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
						.addComponent(lblOutputFile)
						.addComponent(button_1)))
		);
		panel.setLayout(gl_panel);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBorder(new TitledBorder(null, "Benchmark code", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_1.setLayout(new BorderLayout(0, 0));
		
		JScrollPane scrollPane = new JScrollPane();
		panel_1.add(scrollPane);
		
		tfBenchmarkCode = new JEditorPane();
		scrollPane.setViewportView(tfBenchmarkCode);
		
		JButton btnAddToBatch = new JButton("Add to batch");
		btnAddToBatch.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				addToBatch(-1);
			}
		});
		
		JButton btnUpdateCurrent = new JButton("Update current");
		btnUpdateCurrent.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCurrent();
			}
		});
		GroupLayout gl_panel_2 = new GroupLayout(panel_2);
		gl_panel_2.setHorizontalGroup(
			gl_panel_2.createParallelGroup(Alignment.TRAILING)
				.addGroup(gl_panel_2.createSequentialGroup()
					.addGroup(gl_panel_2.createParallelGroup(Alignment.TRAILING)
						.addComponent(panel, GroupLayout.DEFAULT_SIZE, 469, Short.MAX_VALUE)
						.addGroup(gl_panel_2.createSequentialGroup()
							.addContainerGap(279, Short.MAX_VALUE)
							.addComponent(btnUpdateCurrent)
							.addPreferredGap(ComponentPlacement.RELATED)
							.addComponent(btnAddToBatch))
						.addComponent(panel_1, Alignment.LEADING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
					.addContainerGap())
		);
		gl_panel_2.setVerticalGroup(
			gl_panel_2.createParallelGroup(Alignment.TRAILING)
				.addGroup(gl_panel_2.createSequentialGroup()
					.addContainerGap()
					.addComponent(panel, GroupLayout.DEFAULT_SIZE, 419, Short.MAX_VALUE)
					.addPreferredGap(ComponentPlacement.RELATED)
					.addComponent(panel_1, GroupLayout.PREFERRED_SIZE, 111, GroupLayout.PREFERRED_SIZE)
					.addPreferredGap(ComponentPlacement.RELATED)
					.addGroup(gl_panel_2.createParallelGroup(Alignment.BASELINE)
						.addComponent(btnAddToBatch)
						.addComponent(btnUpdateCurrent))
					.addContainerGap())
		);
		panel_2.setLayout(gl_panel_2);
		
		JPanel panel_3 = new JPanel();
		panel_3.setBorder(new TitledBorder(null, "Batch", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_3.setLayout(new BorderLayout(0, 0));
		
		batchList = new JList<>();
		batchList.setMinimumSize(new Dimension(200, 100));
		batchList.addListSelectionListener(new ListSelectionListener() {
			
			@Override
			public void valueChanged(ListSelectionEvent evt) {
				openFromBatch(batchList.getSelectedValue());
			}
		});
		panel_3.add(batchList);
		batchModel = new DefaultListModel<Configuration>();
		batchList.setModel(batchModel);
		GroupLayout gl_contentPane = new GroupLayout(contentPane);
		gl_contentPane.setHorizontalGroup(
			gl_contentPane.createParallelGroup(Alignment.LEADING)
				.addGroup(gl_contentPane.createSequentialGroup()
					.addComponent(panel_2, GroupLayout.PREFERRED_SIZE, 479, GroupLayout.PREFERRED_SIZE)
					.addPreferredGap(ComponentPlacement.UNRELATED)
					.addComponent(panel_3, GroupLayout.PREFERRED_SIZE, 188, Short.MAX_VALUE)
					.addContainerGap())
		);
		gl_contentPane.setVerticalGroup(
			gl_contentPane.createParallelGroup(Alignment.LEADING)
				.addGroup(Alignment.TRAILING, gl_contentPane.createSequentialGroup()
					.addGroup(gl_contentPane.createParallelGroup(Alignment.TRAILING)
						.addComponent(panel_2, Alignment.LEADING, GroupLayout.DEFAULT_SIZE, 587, Short.MAX_VALUE)
						.addComponent(panel_3, GroupLayout.DEFAULT_SIZE, 313, Short.MAX_VALUE))
					.addContainerGap())
		);
		
		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		panel_3.add(toolBar, BorderLayout.SOUTH);
		
		JButton btnStart = new JButton(new ImageIcon("resources\\icons\\run.gif"));
		toolBar.add(btnStart);
		
		JButton btnRemove = new JButton(new ImageIcon("resources\\icons\\delete.gif"));
		btnRemove.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent evt) {
				int index = batchList.getSelectedIndex();
				if (index >= 0) {
					batchModel.remove(index);
				}
			}
		});
		toolBar.add(btnRemove);
		
		JButton btnLoad = new JButton(new ImageIcon("resources\\icons\\load.gif"));
		btnLoad.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent evt) {
				loadConfigs();
			}
		});
		toolBar.add(btnLoad);
		
		JButton btnSave = new JButton(new ImageIcon("resources\\icons\\save.gif"));
		btnSave.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent evt) {
				saveConfigs();
			}
		});
		toolBar.add(btnSave);
		btnStart.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent evt) {
				runBenchmark();
			}
		});
		contentPane.setLayout(gl_contentPane);
	}

	protected void updateCurrent() {
		int index = batchList.getSelectedIndex();
//		if (index > -1) {
//			batchModel.remove(index);
//		}
		addToBatch(index);
		
		// if something is selected in the batch list
		// ... replace this configuration
		// else
		// ... add new configuration to batch
		
	}

	protected void saveConfigs() {
		JFileChooser fc = new JFileChooser(controller.getBaseDir());
		int result = fc.showSaveDialog(this);
		if (result == JFileChooser.APPROVE_OPTION) {
			saveConfigsToFile(fc.getSelectedFile());
		}
	}

	protected void loadConfigs() {
		JFileChooser fc = new JFileChooser(controller.getBaseDir());
		int result = fc.showOpenDialog(this);
		if (result == JFileChooser.APPROVE_OPTION) {
			batchModel.removeAllElements();
			loadConfigFromFile(fc.getSelectedFile());
		}
	}

	public boolean saveConfigsToFile(File f) {
		try {
			FileOutputStream fos = new FileOutputStream(f);
			ObjectOutputStream oos = new ObjectOutputStream(fos);
			oos.writeInt(batchModel.size());
			for (int i=0; i<batchModel.size(); i++) {
				oos.writeObject(batchModel.get(i));
			}
			oos.close();
			fos.close();
		} catch (IOException ex) {
			return false;
		}
		return true;
	}

	private void loadConfigFromFile(File f) {
		List<Configuration> configs = IOUtils.loadConfigsFromFile(f);
		for (Configuration c : configs) {
			batchModel.addElement(c);
		}
	}

	protected void openFromBatch(Configuration conf) {
		if (conf != null) {
			if (!valuesChanged || JOptionPane.showConfirmDialog(this, "Beim Laden einer Konfiguration werden die aktuellen Werte überschrieben. Fortfahren?", "Daten laden", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				tfName.setText(conf.getName());
				chckbxJt.setSelected(conf.isLoadJT());
				tfLoadFile.setText(conf.getLoadFile());
				tfOutputFile.setText(conf.getOutputFile());
				tfBenchmarkCode.setText(conf.getBenchmarkCommandsAsString());
				tableModel.setSelection(conf.getPairs());
				table.updateUI();
			}
		}
	}
	
	private boolean running = false;
	protected void runBenchmark() {
		if (running) {
			JOptionPane.showMessageDialog(this, "Batch is already running", "Error", JOptionPane.WARNING_MESSAGE);
		} else {
			running = true;

			for (int i = 0; i < batchModel.size(); i++) {
				Configuration conf = batchModel.get(i);
				PbtRunner runner = new PbtRunner(controller, conf, this);
				runner.run();
			}
			
			running = false;
			JOptionPane.showMessageDialog(this, "Batch is finished", "Finished", JOptionPane.INFORMATION_MESSAGE);
		}

	}

	protected void addToBatch(int index) {
		String name = tfName.getText();
		List<FactbaseVersionPair> selectedPairs = tableModel.getSelectedPairs();
		
		if (name.isEmpty()) {
			JOptionPane.showMessageDialog(this, "Name must not be empty", "Error", JOptionPane.WARNING_MESSAGE);
		} else if (selectedPairs.size() == 0) {
			JOptionPane.showMessageDialog(this, "Select at least one factbase/version pair", "Error", JOptionPane.WARNING_MESSAGE);
		} else {
			Configuration c = new Configuration(name,
					selectedPairs,
					tfLoadFile.getText(),
					tfOutputFile.getText(),
					tfBenchmarkCode.getText(),
					chckbxPdt.isSelected(),
					chckbxJt.isSelected()
					);
			if (index > -1) {
				batchModel.remove(index);
				batchModel.add(index, c);
				batchList.setSelectedIndex(index);
			} else {
				batchModel.addElement(c);
			}
		}
	}

	protected void selectFile(JTextField textField) {
		String prev = textField.getText();
		JFileChooser fc;
		if (prev.isEmpty()) {
			fc = new JFileChooser(controller.getBaseDir());
		} else {
			fc = new JFileChooser(prev);
		}
		int result = fc.showOpenDialog(this);
		if (result == JFileChooser.APPROVE_OPTION) {
			textField.setText(fc.getSelectedFile().getPath());
		}
	}

	private JPopupMenu popup = new JPopupMenu();

	private JCheckBox chckbxPdt;

	private JCheckBox chckbxJt;

	protected JList<Configuration> batchList;
	
	@Override
	public void mouseClicked(MouseEvent arg0) {}

	@Override
	public void mouseEntered(MouseEvent arg0) {}

	@Override
	public void mouseExited(MouseEvent arg0) {}

	@Override
	public void mousePressed(MouseEvent evt) {
		showPopup(evt);
	}

	@Override
	public void mouseReleased(MouseEvent evt) {
		showPopup(evt);
	}

	private void showPopup(final MouseEvent evt) {
		if (evt.isPopupTrigger()) {
			popup = new JPopupMenu();

			JMenuItem selectRow = new JMenuItem();
			selectRow.setText("(De-)Select row");
			
			selectRow.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					tableModel.selectRow(table.rowAtPoint(evt.getPoint()));
				}
			});
			
			popup.add(selectRow);
			{
				JMenuItem selectCol = new JMenuItem();
				selectCol.setText("(De-)Select column");
				
				selectCol.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						tableModel.selectCol(table.columnAtPoint(evt.getPoint()));
					}
				});
				
				popup.add(selectCol);}
			
			{
				JMenuItem selectAll = new JMenuItem();
				selectAll.setText("(De-)Select all");
				
				selectAll.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						tableModel.selectAll();
					}
				});
				
				popup.add(selectAll);}
			popup.show(evt.getComponent(), evt.getX(), evt.getY());
		}

	}
}
