package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLayeredPane;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

public class TrainingPane extends JFrame {
	 private RegulusGUI regulusWindow = null;
	 private JInternalFrame trainingpane = null;
	 private GridBagLayout gblayout;
	 private GridBagConstraints gbConstraints;
	 private JMenuBar bar = new JMenuBar();  // create menubar
     private JMenuItem HelpMenu;
     private JMenuItem New_Example_Menu;
     private JMenuItem Corpus_Example_Menu;
     private JMenuItem Browse_Corpus_Menu;
     private JMenuItem Browse_Library_Menu;
     private JPanel displayPanel = new JPanel();
     private JPanel inputPanel = new JPanel(new BorderLayout());
     private JTextField inputField = new JTextField(36);
     public JTextArea text = new JTextArea(20, 40);
     private String inputsentence = "";
	 
	  // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return trainingpane;
	  }
	  // get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
	  
	  public TrainingPane() {
		  
		  setJMenuBar( bar );  // set the menubar for the JInternalFrame	
			  
		  trainingpane = new JInternalFrame("Training Specialised Grammars",true,true,true,true);
		  Container c2 = trainingpane.getContentPane();
		  
		  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		  
//		 set up the layout
		  gblayout = new GridBagLayout();
		 
		  displayPanel.setLayout(gblayout);
		  
		  // instansiate the gridbag constraints
		  gbConstraints = new GridBagConstraints();
		  
		  // create Help  menu item and sub menu items
		  JMenu helpMenu = new JMenu( "Help" );
		  helpMenu.setMnemonic( 'H');
		  HelpMenu = new JMenuItem( "Display HelpText" );
		  HelpMenu.setToolTipText("Reading Help Text");
		  HelpMenu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				CreateAndLinkhelp();
		  				// put in name of code which reads help file
		  				
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  helpMenu.add(HelpMenu);
		  bar.add(helpMenu);
	 
		  // create input  menu item and sub menu items
		  JMenu inputMenu = new JMenu( "Input" );
		  inputMenu.setMnemonic( 'I');
		  New_Example_Menu = new JMenuItem( "Enter Example Manually" );
		  New_Example_Menu.setToolTipText("Enter Example");
		  New_Example_Menu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				inputsentence = inputField.getText();
		  				//CreateAndLinktree(inputsentence);
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  inputMenu.add(New_Example_Menu);
		  bar.add(inputMenu);
		  
		  Corpus_Example_Menu = new JMenuItem( "Enter Example From Corpus" );
		  Corpus_Example_Menu.setToolTipText("Enter Example from Corpus");
		  Corpus_Example_Menu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  inputMenu.add(Corpus_Example_Menu);
		  bar.add(inputMenu);
		  
		  Browse_Corpus_Menu = new JMenuItem( "Browse Corpus" );
		  Browse_Corpus_Menu.setToolTipText("Browse Corpus");
		  Browse_Corpus_Menu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  inputMenu.add(Browse_Corpus_Menu);
		  bar.add(inputMenu);
		  
		  Browse_Library_Menu = new JMenuItem( "Browse Training Library" );
		  Browse_Library_Menu.setToolTipText("Browse Training Library");
		  Browse_Library_Menu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  inputMenu.add(Browse_Library_Menu);
		  bar.add(inputMenu);
		  
	  // textfield
	  inputField.setToolTipText("Input sentence or word to be parsed");
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = 0;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 9;
	  displayPanel.add(inputField,gbConstraints);
	  
	  //	 add a JScrollPane containing the JList
	  // to the contentpane
	  
	  gbConstraints.fill = GridBagConstraints.BOTH;
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = 3;
	  gbConstraints.gridheight = 3;
	  gbConstraints.gridwidth = 9;
	  gbConstraints.weightx = 1;
	  gbConstraints.weighty = 1;
	  gbConstraints.ipady = 0;
	  displayPanel.add(new JScrollPane(text),gbConstraints);
	  
		trainingpane.setDefaultCloseOperation(
				  WindowConstants.DISPOSE_ON_CLOSE);
	 inputPanel.add(bar,BorderLayout.NORTH);
	 c2.add(inputPanel,BorderLayout.NORTH  );
	 c2.add(displayPanel);
	// frame2.setSize(450,650);
	 trainingpane.pack();
	 
	 trainingpane.setLocation(520,20);
	  }
	  public void CreateAndLinkhelp()
	  {
		    HelpPane helpPane = new HelpPane(this, getRegulusGUI());
		  	helpPane.setRegulusGUI(getRegulusGUI());
		  	
		  	
			 //  display new internal window
		    JInternalFrame HelpPaneInternalFrame = helpPane.getInternalFrame();
		  //add the internal frame to the desktop
			regulusWindow.desktop.add(HelpPaneInternalFrame,JLayeredPane.DEFAULT_LAYER);
			HelpPaneInternalFrame.setVisible(true); 
	  }
	  
	
}
