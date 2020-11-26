package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import RegulusGUI.progressLoadRecognition.progressListPane;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;


public class Frame2 extends JFrame 
  		implements ActionListener, VetoableChangeListener,InternalFrameListener,MouseListener {
 	
// pointer to the reguluswindow which has created stepper window 
	  RegulusTree regulustree;
	  private RegulusGUI regulusWindow = null;
	  private UnavailableCommandsForFrame2 unavailablecommandsforframe2 = null;
	  private AvailableMenusForFrame2 availablemenusforframe2 = null;
	  private JTextArea textArea = new JTextArea(17,20);
	  private JTextArea HelptextArea = new JTextArea(25,30);
	  private String output = "";
	  public  JTextField inputField = new JTextField(65);
	  public  JButton parse =      new JButton("       Phrase       ");
	  private JButton lex =       new JButton("Lexical Item   ");
	  private JButton gap =       new JButton("     Gap     ");
	  private JButton generate =  new JButton("   Generate  ");
	  private JButton delete =    new JButton("    Delete   ");
	  private JButton reset =     new JButton("   Reset     ");
	  private JButton ShowTree  = new JButton("  Show Tree  ");
	  private JButton join   =    new JButton(" Join Trees  ");
	  private JButton combine =   new JButton("Combine Items");
	  private JButton show = new JButton("Semantic Rep");
	  private JRadioButton RbtnEdit, RbtnDebug;
	  private GridBagLayout gblayout;
	  private GridBagConstraints gbConstraints;
	  private JInternalFrame frame2 = null;
	  private String ParsecommandPart1 = "PARSE ";
	  private String LexcommandPart1 = "LEX ";
	  private String ParsecommandFull = "";
	  private String LexcommandFull = "";
	  private String parseval = "";
	  private String lexval = "";
	  private String gapval = "GAP";
	  private JList summaryList ; 
	  private String[] holdSummaryTable = new String[50];
	  private DefaultListModel listModel;
	  private int saveIndex = 0;
	  private String DeletecommandPart1 = "DELETE ";
	  private String DeletecommandFull = "";
	  private String GeneratecommandPart1 = "GENERATE ";
	  private String GeneratecommandFull = "";
	  private String deleteval = "";
	  private String generateval = "";
	  private Object thisElement;
	  private Object FirstJoinElement;
	  private Object SecondJoinElement;
	  private String YesNoDelete = "";
	  private String YesNoGenerate = "";
	  private String holdSentence = "";
	  private String elementno = "";
	  private String saveItemNo1 = "";
	  public int Treeval = 0;
	  public String whichTree = "";
	  private String JoincommandPart1 = "JOIN ";
	  private String JoincommandPart2 = "";
	  private String JoincommandPart3 = "";
	  private String JoincommandFull = "";
	  private String ShowcommandPart1 = "SHOW ";
	  private String ShowcommandPart2 = "";
	  private String ShowcommandPart3 = "";
	  private String ShowcommandFull = "";
	  private boolean firstItemContainsCut;
	  private boolean secondItemContainsCut;
	  private int firstItem = 0;
	  private int deleteItem;
	  private int generateItem = 0;
	  private int minIndex = 0; 
      private int maxIndex = 0;
      private int [] IndexTable = new int[20];
      private int OldmaxIndex = 0;
      private int NewmaxIndex = 0;
      public JInternalFrame[] allFrames = null;
      private String input = "";
      private boolean InputIsNumeric = true;
      private String [] temp = null;
      private String holdCombineInput = ""; 
      private boolean InputIsInSummaryTable = true;
      private String CombineHoldSentence = "";
      private String CombinecommandPart1 = "COMBINE ";
      private String CombinecommandFull = "";
      private String InputText = "";
      private int showindex = 0;
      public int  boundsIndex = 0;
      private int boundsAddx = 0;
      private int boundsAddy = 0;
      public int setx = 0;
      public int sety = 0;
      private int ItemsBeforeGenerate = 0;
      private int ItemsAfterGenerate = 0;
      private int ItemsGenerated = 0;
      private JMenuBar bar = new JMenuBar();  // create menubar
      private JMenuItem HelpMenu;
      private JPanel displayPanel = new JPanel();
      private JPanel inputPanel = new JPanel(new BorderLayout());
      public  String record = null;
      private int ResultIndex = 0;
      private boolean ThereAreManyDeleteItems = false;
      private String itemString = "";
      private boolean edit_button = false;
      private JMenu 	loadMenu;
      private JMenu History_menu = new JMenu( "History" );
      public  JMenuItem  EBL_Generation_loadMenuItem = new JMenuItem( "EBL Load Generation" );
      public  JMenuItem  L_Generation_loadMenu = new JMenuItem( "Load Generation" );
      public  JMenuItem  EBL_loadMenuItem = new JMenuItem( "EBL Load" ); 
      public  JMenuItem  L_loadMenu = new JMenuItem( "Load" );
      public  JMenuItem  EBL_Generation_Arg_loadMenuItem = new JMenuItem( "EBL Load Generation Arg" );
      public  JMenuItem  Load_Generation_Arg_MenuItem = new JMenuItem( "Load Generation Arg" );
      public  JMenuItem   EBL_load_GenerationMenuItem = new JMenuItem( "EBL Load Generation" );
      public  JMenuItem  Load_Generation_menu = new JMenu( "Load Generation" ); 
      public  JMenuItem  EBL_load_GenerationMenuItem_Arg  = new JMenuItem( "EBL Load Generation Arg" );
      private JMenuItem  quit_system_MenuItem;
      private String CommandString = "";
      private int loadReply = 0;
      private String inPutValue = "";
      private boolean record_exist_in_table = false;
      private boolean   end_ofTable;
      private int createHistoryIndex = 0;
      private JMenuItem[] one_step_back_menuItem = new JMenuItem[20];
      private  Object list = null;
      private boolean parse_error = false;
      private String strList = "";
      private boolean display_message = false;
      private int saveWindowsIndex = 0;
      private boolean   remove_finished = false;
      private Component c = null;
      private JButton Stepperbtn;
      private boolean   button_created = false;
      private JButton Stepbtn =     new JButton("Stepper");
      private JButton debugbtn =     new JButton("Debugging Trace");
      private int[] frameTable = new int[50];
      private JInternalFrame[] frames = null;
      private int frameIndex = 0;
      private int holdFrameIndex = 0;
      private int stepCount = 0;
      private String ac = "";
      private String bc = "";
      private String strName = "";
      private int treeCounter = 0;
      public  JButton buttonThree;
      private String strElement = "";
      private int    nodeNumber = 0;
      private int itemNumber = 0;
      private String summaryString = "";
      private String hold3 = "";
      private String holdNum = "";
      private int sum = 0;
       public int numOfUnavailableCommands = 0;
	  
	  // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return frame2;
	  }
	  // get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
	  public Frame2() {
		  
	  }
	 
	  public Frame2(RegulusGUI window, JButton Stepperbtn, int steppCounter,JButton DebugBtn) {
	  regulusWindow = window;
	  Stepbtn = Stepperbtn;
	  debugbtn = DebugBtn;
	  stepCount = steppCounter;
	  Stepbtn.addMouseListener(this);
	  debugbtn.addMouseListener(this);
	
	  
		  
	  setJMenuBar( bar );  // set the menubar for the JInternalFrame	
	  
	 // deskTopBarPanel =  inputPanel; 
		  
	  frame2 = new JInternalFrame("Stepper "+stepCount,true,true,true,true);
	  
	
	  Container c2 = frame2.getContentPane();
	  
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  // add listener to confirm closing
	  frame2.addVetoableChangeListener(this);
	  //regulusWindow.buttonTwo.addMouseListener(this);
	
	  // create the listmodel
	  listModel = new DefaultListModel();
	  
	  // create a list with a start item
	  //listModel.addElement("This is the Summary window containing sentences and words");
	  listModel.addElement("Summary Window containing sentences and words");
	  summaryList = new JList(listModel);
	  summaryList.setCellRenderer(new ItemCellRenderer());
	  summaryList.setVisibleRowCount(30);
	  
	  // allow multiple selections
	  summaryList.setSelectionMode(
			  ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	  summaryList.setFixedCellHeight( 15);
	  
	  
	  // set up the layout
	  gblayout = new GridBagLayout();
	 
	  displayPanel.setLayout(gblayout);
	  
	  // instansiate the gridbag constraints
	  gbConstraints = new GridBagConstraints();
	  
	  regulusWindow.updateCommandStatus();
	
	  unavailablecommandsforframe2 = new UnavailableCommandsForFrame2(this, getRegulusGUI());
	  availablemenusforframe2 = new AvailableMenusForFrame2(this, getRegulusGUI());
	  unavailablecommandsforframe2.check_unavailable_menus(); 
	  availablemenusforframe2.check_available_menus();
		  
		 //  create LOAD  menu item and sub menu items
		 
		loadMenu = new JMenu( "Load" ); 
		loadMenu.setMnemonic( 'L');
	   
	    L_loadMenu.setToolTipText("Load current Regulus grammar in DCG and left-corner form");
	    L_loadMenu.addActionListener(
	    		 new ActionListener() {
	    				public void actionPerformed( ActionEvent e)
	    				{
	    					if (saveIndex > 0)
	    				    	{
	    				    	generateWarningMessage();
	    				    	if ( loadReply == 0)
	    				    	{
	    				    		regulusWindow.handleCommand("LOAD");
	    	    					if (regulusWindow.regulus_command_succeeded)
	    	    					{ 
	    	    						InputText = "Load command succeeded";
	    	    						regulusWindow.txtBoxDisplayPositive(InputText);
	    	    						 checkMenuStatus();
	    		    					 getValueTable();
	    			 			  		 setfield();
	    	    					}
	    	    					else
	    	    					{
	    	    						createLoadErrorWindow();
	    	    						checkMenuStatus();
	    		    					getValueTable();
	    			 			  		setfield();
	    	    					}
	    				    	}
	    				    	}
	    					else
	    					{
	    					regulusWindow.handleCommand("LOAD");
	    					if (regulusWindow.regulus_command_succeeded)
	    					{ 
	    						InputText = "Load command succeeded";
	    						regulusWindow.txtBoxDisplayPositive(InputText);
	    						 checkMenuStatus();
		    					 getValueTable();
			 			  		 setfield();
	    					}
	    					else
	    					{
	    						createLoadErrorWindow();
	    						checkMenuStatus();
		    					getValueTable();
			 			  		setfield();
	    					}
	    					}
	    	
	    			}
	    		}
	  );
	    regulusWindow.updateCommandStatus();
	    unavailablecommandsforframe2.check_unavailable_menus(); 
		availablemenusforframe2.check_available_menus();
	    loadMenu.add(L_loadMenu);
	    
	   
	   
//	  create EBL_LOAD sub menu item to LOAD
	    
	    
	    EBL_loadMenuItem.setToolTipText("Load current specialised Regulus grammar in DCG and left-corner form ");
	    EBL_loadMenuItem.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
    					if (saveIndex > 0)
    				    	{
    				    	generateWarningMessage();
    				    	if ( loadReply == 0)
    				    	{
    				    		regulusWindow.handleCommand("EBL_LOAD");
    	    					if (regulusWindow.regulus_command_succeeded)
    	    					{ 
    	    						InputText = "Ebl Load command succeeded";
    	    						regulusWindow.txtBoxDisplayPositive(InputText);
    	    						 checkMenuStatus();
    		    					 getValueTable();
    			 			  		 setfield();
    	    					}
    	    					else
    	    					{
    	    						String command = regulusWindow.getCommandErrorString();	
    	    						InputText = command;
    	    						regulusWindow.txtBoxDisplayPositive(InputText);
    	    						 checkMenuStatus();
    		    					 getValueTable();
    			 			  		 setfield();
    	    					}
    				    	}
    				    	}
    					else
    					{
    					regulusWindow.handleCommand("EBL_LOAD");
    					if (regulusWindow.regulus_command_succeeded)
    					{ 
    						InputText = "Ebl Load command succeeded";
    						regulusWindow.txtBoxDisplayPositive(InputText);
    						 checkMenuStatus();
	    					 getValueTable();
		 			  		 setfield();
    					}
    					else
    					{
    						String command = regulusWindow.getCommandErrorString();	
    						InputText = command;
    						regulusWindow.txtBoxDisplayPositive(InputText);
    						checkMenuStatus();
	    					getValueTable();
		 			  		setfield();
    					}
    					
    					}
    					
	    			}
    		}
  );
	    regulusWindow.updateCommandStatus();
	    unavailablecommandsforframe2.check_unavailable_menus(); 
		availablemenusforframe2.check_available_menus();	
	    loadMenu.add( EBL_loadMenuItem);
	    bar.add(loadMenu);
	    

	    
//	  create quit system sub menu item to LOAD_GENERATION
	    
	    quit_system_MenuItem = new JMenuItem( "Exit" );
	    quit_system_MenuItem.setToolTipText("Exit System ");
	    quit_system_MenuItem.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
	    				System.exit(0);
	    				
	        		}
	    		}
	    		);
	    loadMenu.add( quit_system_MenuItem);
	    bar.add(loadMenu);	       
	    
	    // create Load_Generation Menu and sub menus
	    
	    
	    Load_Generation_menu.setMnemonic( 'G');
	    L_Generation_loadMenu.setToolTipText("Load Generation grammar ");
	    L_Generation_loadMenu.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{ 
	    				regulusWindow.handleCommand("LOAD_GENERATION");
	     				if (regulusWindow.regulus_command_succeeded)
	    				{ 
	     					regulusWindow.InputText = "Load Generation command succeeded";
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	 			  			setfield();
	    				}
	    				else
	    				{
	    					String command = regulusWindow.getCommandErrorString();
	    					regulusWindow.InputText = command;
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	    			  		setfield();
	    				}
	     				regulusWindow.updateCommandStatus();
	     			    unavailablecommandsforframe2.check_unavailable_menus(); 
	     				availablemenusforframe2.check_available_menus();
	    				
	    			}
	    		}
	  );
	    Load_Generation_menu.add(L_Generation_loadMenu);
	    bar.add( Load_Generation_menu);
	    
    // create Load_Generation Menu and sub menus
	    
	    Load_Generation_Arg_MenuItem.setToolTipText("Load Generation arg grammar ");
	    Load_Generation_Arg_MenuItem.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{ 
	    				regulusWindow.handleCommand("LOAD_GENERATION arg" );
	     				if (regulusWindow.regulus_command_succeeded)
	    				{ 
	     					regulusWindow.InputText = "Load Generation arg command succeeded";
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	 			  			setfield();
	    				}
	    				else
	    				{
	    					String command = regulusWindow.getCommandErrorString();
	    					regulusWindow.InputText = command;
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	    			  		setfield();
	    				}
	     				regulusWindow.updateCommandStatus();
	     			    unavailablecommandsforframe2.check_unavailable_menus(); 
	     				availablemenusforframe2.check_available_menus();
	    			}
	    		}
	  );
	    Load_Generation_menu.add(Load_Generation_Arg_MenuItem);
	    bar.add( Load_Generation_menu);
	    
//	  create EBL_LOAD_Generation sub menu item to LOAD_GENERATION
	    
	   
	    EBL_load_GenerationMenuItem.setToolTipText("Load current grammar ");
	    EBL_load_GenerationMenuItem.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
	    				regulusWindow.handleCommand("EBL_LOAD_GENERATION");
	     				if (regulusWindow.regulus_command_succeeded)
	    				{ 
	     					regulusWindow.InputText = "Ebl Load Generation command succeeded";
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	 			  			setfield();
	    				}
	    				else
	    				{
	    					String command = regulusWindow.getCommandErrorString();
	    					regulusWindow.InputText = command;
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    				}
	     				regulusWindow.updateCommandStatus();
	     			    unavailablecommandsforframe2.check_unavailable_menus(); 
	     				availablemenusforframe2.check_available_menus();
	        		}
	    		}
	    		);
	    Load_Generation_menu.add( EBL_load_GenerationMenuItem);
	    bar.add(Load_Generation_menu);
	    
//	  create EBL_LOAD_GENERATION_Arg sub menu item to LOAD
	    
	 
	    EBL_load_GenerationMenuItem_Arg.setToolTipText("Compile and load designated version of current specialised Regulus grammar for generation"); 
	    EBL_load_GenerationMenuItem_Arg.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
	    				regulusWindow.handleCommand("EBL_LOAD_GENERATION_Arg");
	    				if (regulusWindow.regulus_command_succeeded)
	    				{ 
	     					regulusWindow.InputText = "Ebl Load Generation arg command succeeded";
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    					getValueTable();
	 			  			setfield();
	    				}
	    				else
	    				{
	    					String command = regulusWindow.getCommandErrorString();
	    					regulusWindow.InputText = command;
	    					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	    				}
	    				regulusWindow.updateCommandStatus();
	     			    unavailablecommandsforframe2.check_unavailable_menus(); 
	     				availablemenusforframe2.check_available_menus();
	     				
	    			}
	    		}
	    		);
	    Load_Generation_menu.add(EBL_load_GenerationMenuItem_Arg);
	    bar.add(Load_Generation_menu);
	    
	    
//	    create History menu
	    
	    History_menu = new JMenu("History");
	    History_menu.setToolTipText("get backlog of sentences");
	    History_menu.setMnemonic( 'H');
	    History_menu.setEnabled(false);
	    
	    bar.add(History_menu);
	  
	  // add radio buttons
	  RbtnEdit = new JRadioButton("Edit",false);
	  //bar.add(RbtnEdit);
	  RbtnDebug = new JRadioButton("Debug",true);
	  //bar.add(RbtnDebug);
	  
	  // check if the remote radio button is clicked on
	  RbtnEdit.addActionListener(this);
	  RbtnDebug.addActionListener(this);
	  
	  // create Help  menu item and sub menu items
	  JMenu helpMenu = new JMenu( "Help" );
	  helpMenu.setMnemonic( 'e');
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
	 
	  // textfield
	  inputField.setToolTipText("Input sentence or word to be parsed");
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = 0;
	  gbConstraints.gridwidth  = 6;
	  gbConstraints.weightx = 1.0;
	  gbConstraints.weighty = 1.0;
	  gbConstraints.insets = new Insets(0, 0, 0, 0);
	  displayPanel.add(inputField,gbConstraints);
	  
//	 Parse button
	 
	  parse.setToolTipText("Start parsing sentence");
	  gbConstraints.weighty = 0.5;
	  gbConstraints.gridx = 6;
	  gbConstraints.gridy = 0;
	  gbConstraints.weightx = 0.5;
	  gbConstraints.weighty = 0.5;
	  gbConstraints.gridwidth  = 1;
	  gbConstraints.anchor = GridBagConstraints.FIRST_LINE_END;
	  displayPanel.add(parse,gbConstraints);
	
 	// Lex button
	  
	  lex.setToolTipText("Start parsing word");
	  gbConstraints.weighty = 0.5;
	  gbConstraints.gridx = 7;
	  gbConstraints.gridy = 0;
	  gbConstraints.weightx = 0.5;
	  gbConstraints.weighty = 0.5;
	  gbConstraints.gridwidth  = 1;
	  displayPanel.add(lex,gbConstraints);	  
	 
	  
// Gap button
	  
	  gap.setToolTipText("Create gap constituant");
	 // gbConstraints.weightx = 0.5;
	  gbConstraints.weighty = 0.0;
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(gap,gbConstraints);
	  
// Generate button
	  
	  generate.setToolTipText("Generate sentences from tree");
	  gbConstraints.gridx = 1;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(generate,gbConstraints);
	  
    // Delete button
	  
	  delete.setToolTipText("Delete sentence or word");
	  gbConstraints.gridx = 2;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(delete,gbConstraints);
	  
 
   // Reset button
	  
	  reset.setToolTipText("Delete all Summary Items");
	  gbConstraints.gridx = 3;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(reset,gbConstraints);
	  
    // Show Tree
	  
	  ShowTree.setToolTipText("Show tree ");
	  gbConstraints.gridx = 4;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(ShowTree,gbConstraints);
  
// Join trees button
	  
	  join.setToolTipText("Join  ");
	  gbConstraints.gridx = 5;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(join,gbConstraints);
	  
// Combine items button
	  
	  combine.setToolTipText("Combine summary items  ");
	  gbConstraints.gridx = 6;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(combine,gbConstraints);
	  
// show items button
	  
	  show.setToolTipText("Show semantic representation  ");
	  gbConstraints.gridx = 7;
	  gbConstraints.gridy = 2;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  displayPanel.add(show,gbConstraints);
	  

	  
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
	  displayPanel.add(new JScrollPane(summaryList),gbConstraints);
	  
	 
	  
	  // Check if Parse button is pressed down
	  parse.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{ 
		  			parseval = inputField.getText();
		  			inPutValue = inputField.getText();
		  			HistoryMenu();
		  			inputField.setText("");
		  			setvalueParse(parseval);
//		  		 	If menu answer is required check what answer you need to enter 
		  		    if (regulusWindow.menu_answer_required){
			  		checkMenuAnswer();
		  		    }
		  			 getValueTable();
		  			 setfield();
	 			
		  			}
		  		}
		  );
	  
	  // Check if Lex button is pressed down  
	  lex.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			lexval = inputField.getText();
			  			inputField.setText("");
			  			setvalueLex(lexval);
			  		    // If menu answer is required check what answer you need to enter 
			  		    if (regulusWindow.menu_answer_required){
				  		checkMenuAnswer();
			  		   	}
			  			getValueTable();
			  			setfield();
	 	  			}
		  		}
			  );
	  
//	 Check if Gap button is pressed down  
	  gap.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			setvalueGap(gapval);
			  			 // If menu answer is required check what answer you need to enter 
			  		    if (regulusWindow.menu_answer_required){
				  		checkMenuAnswer();
			  		   	}
			  		    getValueTable();
			  			setfield();
		  			}
			  		}
			  );
	  
//		 Check if show button is pressed down  
	  show.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			minIndex = summaryList.getMinSelectionIndex();
			  	        maxIndex = summaryList.getMaxSelectionIndex();
			  	        OldmaxIndex = maxIndex;
			  			for (int i = minIndex; i <= maxIndex; i++) {
			  				if (summaryList.isSelectedIndex(i)) {
			              	  generateItem = i;
			  				}
			  			 thisElement = listModel.get(generateItem); 
			  			 strElement = thisElement.toString();
			  			 //System.out.println("strElement "+strElement  );
			  			 getItemNumber();
			  			// Send show command 
			  			RegulusSummaryItem summaryItem; 
			  			ShowcommandPart2 = holdNum;
			  			String node = "1";
			  			ShowcommandPart3 = node;
			  			ShowcommandFull = ShowcommandPart1 + ShowcommandPart2 +" " +ShowcommandPart3;
			  			//System.out.println("ShowcommandFull "+ShowcommandFull);
			   			regulusWindow.handleCommand(ShowcommandFull);
			   			String command = regulusWindow.getCommandErrorString();
			  			if (regulusWindow.regulus_command_succeeded == false)
			  			{
			  				JOptionPane.showMessageDialog(null, command
			  						,"Message for Show",JOptionPane.INFORMATION_MESSAGE);
			  			}
			  			else
			  			{
			  			itemNumber = Integer.parseInt(holdNum);
			  			nodeNumber = Integer.parseInt(node);
			  			
			  			summaryItem = regulusWindow.getStepperItemNode(itemNumber,nodeNumber);
			  			summaryString = summaryItem.toStringNoTree();
			  			TakeAwayTagInSummaryString();
			  			// display the Rule
			  			JOptionPane.showMessageDialog(null, hold3 ,"Semantic Representation",JOptionPane.PLAIN_MESSAGE);
			  			
			  			if (regulusWindow.regulus_command_succeeded == false)
			  				JOptionPane.showMessageDialog(null, command
			  						,"Message for Show",JOptionPane.INFORMATION_MESSAGE);
			  			
			  			getValueTable();
			  			setfield();
			  		
			  			}
			  		}
			  	}
			  	}
			 ); 
	  
//	  		 Check if Generate button is pressed down  
	  generate.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			minIndex = summaryList.getMinSelectionIndex();
			  	        maxIndex = summaryList.getMaxSelectionIndex();
			  	        OldmaxIndex = maxIndex;
			  			for (int i = minIndex; i <= maxIndex; i++) {
			  				if (summaryList.isSelectedIndex(i)) {
			              	  generateItem = i;
			  				}
				  		 System.out.println("generateItem "+ generateItem);
			  			 thisElement = listModel.get(generateItem);  
			  			// Send generate command 
			  			checkItemNumbersBeforeGenerate();
			  			generateElement();
			  			checkItemNumbersAfterGenerate();
			  			calculateItemsGenerated();
			  			getValueTable();
			  			setfield();
			  			checkGeneratedNumbers();
			  			}
			  		}
			  	}
			  ); 
	  
//		 Check if Delete button is pressed down  
	  delete.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			minIndex = summaryList.getMinSelectionIndex();
			  	        maxIndex = summaryList.getMaxSelectionIndex();
			  	        checkNumberOfItems();
			  	        // you want to delete many items
			  	        if (ThereAreManyDeleteItems == true)
			  	        { 
			  	        	System.out.println("deletemanyitems");
			  	        	PutItemsInTable();
			  	        	DeleteManyItems(); 
			  				getValueTable();
			  				setfield();
			  				ThereAreManyDeleteItems = false;
			  	        }
			  			// you just want to delete one item
			  	        else if (ThereAreManyDeleteItems == false)
			  			  {
			  				for (int i = minIndex; i <= maxIndex; i++) {
				  				if (summaryList.isSelectedIndex(i)) {
					              	  deleteItem = i;
				  				}
				  			  thisElement = listModel.get(deleteItem); 
			  				  deleteElement();
			  				  getValueTable();
			  				  setfield();
			  			  }
			  			}
			  		}
			  	}
			  ); 
	  
//		 Check if Reset button is pressed down  
	  reset.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			// Send delete command and answer yes/no on confirmation question 
			  			regulusWindow.handleCommand("DELETE_ALL");
			  		  int reply = JOptionPane.showConfirmDialog(null, 
			  		  "Do you really want to delete all elements" , // Prompt displayed in box 
			  		  "Delete Confirm Pane",                 // Label at top of box
			  		  JOptionPane.OK_CANCEL_OPTION);         // Option type for OK and Cancel buttons
			  		  // check which button is pressed OK = 0 CANCEL = 
			  		  if ( reply == 0)
			  		  {
			  			  YesNoDelete = "y";
			  			  regulusWindow.handleCommand(YesNoDelete); 
			  		  }
			  		  else 
			  		  {
			  			  YesNoDelete = "n";
			  			  regulusWindow.handleCommand(YesNoDelete);   
			  		  }
			  			getValueTable();
			  			setfield();
			  		}
			  	}
			  ); 
	
	  
//		 Check if ShowTree button is pressed down  
	  ShowTree.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			minIndex = summaryList.getMinSelectionIndex();
			  	        maxIndex = summaryList.getMaxSelectionIndex();
			  	      for (int i = minIndex; i <= maxIndex; i++) {
			  	        	if (summaryList.isSelectedIndex(i)) {
		  	        		 showindex = i;
		  	        		 boundsIndex = boundsIndex + 1;
		  	        		 list =summaryList.getSelectedValue();
		  	        		 strList = list.toString();
		  	        		 parse_error = false;
		  	        		 checkForFailedParse();
			  	        	}
			  	      }
			  	        	if (parse_error)
			  	        	{
			  	        		JOptionPane.showMessageDialog(null, " Sentence failed Parsing, no tree to show "	,"Warning for show tree",JOptionPane.INFORMATION_MESSAGE);
				  				
			  	        	}
			  	        	else
			  	        	{
			  	       	thisElement = listModel.get(showindex); 
	//			  	    get an element from the list
				  	     GetElementNo();
			  			// set flag which tree you are using
			  			whichTree = "Tree";
			  			saveItemNo1 = elementno;
	                    //  create a copy of tree windov class 
			  			CreateAndLinktree("Tree",elementno);
			  	        	}
			  	        	
			  			}
			  			
			  		}
			  	
			  ); 

//		 Check if join button is pressed down  
	  join.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			// get the first element from the list
			  			firstItem = summaryList.getMinSelectionIndex();
			  			FirstJoinElement = listModel.get(firstItem);
			  		 	thisElement = listModel.get(firstItem); 
			  			GetElementNo();
			  			String saveNo1  = elementno;
			  			FirstJoinElement.toString();
                        // get the second element from the list
			  			int secondItem = summaryList.getMaxSelectionIndex();
			  			SecondJoinElement = listModel.get(secondItem); 
			  			thisElement = listModel.get(secondItem);
			  			GetElementNo();
			  			String saveNo2  = elementno;
			  			SecondJoinElement.toString();
			  			firstItem = Integer.parseInt(saveNo1);
			  			
			  			secondItem = Integer.parseInt(saveNo2);
			  			// check if first item contains cut
			  			firstItemContainsCut = regulusWindow.getStepperItem(firstItem).containsCut();
                        // check if second item contains cut
			  			secondItemContainsCut = regulusWindow.getStepperItem(secondItem).containsCut();
			  			
			  			// check that at least one of the items have got cut in them
			  			
			  			boolean oneItemcContainsCut = false;
			  			if (firstItemContainsCut || secondItemContainsCut){
			  				oneItemcContainsCut = true;
			  			}
			  			if (oneItemcContainsCut == false )
			  			{
			  				JOptionPane.showMessageDialog(null, "Neither Item no " + " "+ firstItem+ " " + "or Item no " +" "+
			  						secondItem+ " " +"has a cut"	,"Warning for join",JOptionPane.INFORMATION_MESSAGE);
			  				
			  			}
			  				
			  			// Check if both items contain cut, the user will enter which Item should come first
			  			// this Item will be in result
			  			
			  			int result = checkBothItemContainsCut();
			  			
			  			if (result == firstItem){
			  				firstItem = result;
			  			}
			  			else
			  			{
			  				secondItem = result;
			  				firstItem = secondItem;
			  			}	
			  			
			  			// If both items contains cut we have found out from user which item is first and second
			  			
			  			 if (firstItemContainsCut && secondItemContainsCut){
			  				JoincommandPart2 = Integer.toString(firstItem);
			  			    JoincommandPart3 = Integer.toString(secondItem);
			  			    JoincommandFull = JoincommandPart1 + JoincommandPart2 +" " +JoincommandPart3;
				  			regulusWindow.handleCommand(JoincommandFull);
				  			String command = regulusWindow.getCommandErrorString();
				  			if (regulusWindow.regulus_command_succeeded == false)
					  				JOptionPane.showMessageDialog(null, command
				  						,"Warning for Join",JOptionPane.INFORMATION_MESSAGE);
				  			getValueTable();
				  			setfield();
			  			 	}
			  			// Just one item contain a cut The item which contains cut should be the first item   
			  			 else
			  			 {
			  				if (secondItemContainsCut){
			  					int holdValue=0;
			  					holdValue = firstItem;
				  				firstItem = secondItem; 
				  				secondItem = holdValue;
			  			 }
			  				JoincommandPart2 = Integer.toString(firstItem);
			  			    JoincommandPart3 = Integer.toString(secondItem);
			  			    JoincommandFull = JoincommandPart1 + JoincommandPart2 +" " +JoincommandPart3;
			  			    System.out.println("JoincommandFull "+JoincommandFull);
				  			regulusWindow.handleCommand(JoincommandFull);
				  			if (regulusWindow.regulus_command_succeeded == false)
				  			{
				  				String command = regulusWindow.getCommandErrorString();
				  				JOptionPane.showMessageDialog(null, command
				  						,"Warning for Join",JOptionPane.INFORMATION_MESSAGE);
				  			
				  			getValueTable();
				  			setfield();
				  			}
				  			
				  			if (regulusWindow.regulus_command_succeeded)
				  			{
				  				JOptionPane.showMessageDialog(null,"join was successful" 
					  				,"Warning for Join",JOptionPane.INFORMATION_MESSAGE);
				  			
				  			
					  			getValueTable();
					  			setfield();
				  			}
			  			}
			
			  		}
			  	}
			  ); 
	  
	  
//		 Check if Combine button is pressed down  
	  combine.addActionListener(
			  	new ActionListener() {
			  		public void actionPerformed( ActionEvent e)
			  		{ 
			  			input = JOptionPane.showInputDialog("Please enter which items you wish to combine");
			  			System.out.println("input "+input);
			  			getItem();
			  			checkNum(temp);
			  			if (InputIsNumeric == false)
			  				JOptionPane.showMessageDialog(null, "Item number must be numeric "
					  				,"Warning for combine",JOptionPane.INFORMATION_MESSAGE);
			  			checkDumpTable(temp);
			  			setvalueCombine();
			  			if (regulusWindow.regulus_command_succeeded == false)
			  			{
			  				String command = regulusWindow.getCommandErrorString();
			  				JOptionPane.showMessageDialog(null,command  
			  						,"Warning for Combine",JOptionPane.INFORMATION_MESSAGE);
			  			getValueTable();
			  			setfield();
			  			}
			  			if (regulusWindow.regulus_command_succeeded)
			  				JOptionPane.showMessageDialog(null, "Combine was successful "
				  				,"Warning for Combine",JOptionPane.INFORMATION_MESSAGE);
				  			getValueTable();
				  			setfield();
				  		}
			  		}
			  ); 
	  
	
	  frame2.addInternalFrameListener(this);
	  frame2.setDefaultCloseOperation(
		 WindowConstants.DISPOSE_ON_CLOSE);
	 inputPanel.add(bar,BorderLayout.NORTH);
	 c2.add(inputPanel,BorderLayout.NORTH  );
	 c2.add(displayPanel);
	 frame2.pack();
	 frame2.setLocation(100,0);
	
	  }
	
	  public void createLoadErrorWindow() {
		  LoadErrorPane loaderrorpane = new LoadErrorPane(this, getRegulusGUI(),sum);
		  JInternalFrame LoadErrorPaneInternalFrame = loaderrorpane.getInternalFrame();
		  regulusWindow.desktop.add(LoadErrorPaneInternalFrame);
		  LoadErrorPaneInternalFrame.setVisible(true);
	  }
	  public void TakeAwayTagInSummaryString()
		{
			int length = summaryString.length();
			String b = "Tag";
				if (summaryString.indexOf(b) != -1) {
				int startPos = summaryString.indexOf(b);
				int endPos = startPos + 12;
				String Trace = summaryString.substring(startPos,endPos);
				String hold1 = summaryString.substring(0,startPos);
				String hold2 = summaryString.substring(endPos,length);
				hold3 = hold1 + hold2;
				}
		}
	  
	  public void getItemNumber() {
		  String b = ":";
		  int holdPos = 0;
		  	if (strElement.indexOf(b) != -1) {
			holdPos = strElement.indexOf(b);
			holdNum = strElement.substring(0,holdPos);
			//System.out.println("holdNum "+holdNum);
		  	}
	  }
	  
	  public void createButton() {
	    	
	    	Stepperbtn.addMouseListener(this);
	    	
	    }
	  public void createLoad() {
			System.out.println("regulusWindow.regulus_command_succeeded "+regulusWindow.regulus_command_succeeded);
			if (regulusWindow.regulus_command_succeeded)
		{ 
			regulusWindow.InputText = "Load command succeeded";
			regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
			getValueTable();
	  		setfield();
		}
		else
		{
			String command = regulusWindow.getCommandErrorString();
			regulusWindow.InputText = command;
			regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
			getValueTable();
	  		setfield();
		}
			regulusWindow.availablemenus.check_available_menus();
			regulusWindow.unavailablecommands.check_unavailable_menus();  
	       
	}
	  
	  public void checkForFailedParse() {
		  String b = "FAILED";
		  	if (strList.indexOf(b) != -1) {
		  		parse_error = true;
		  	}
	  }
	  public void HistoryMenu() {
		  compare(); 
		  // if the record does not exist in historytable then add it
		 // System.out.println("record_exist_in_table "+record_exist_in_table  );
		  	if (record_exist_in_table == false)
		 		{
		  			end_ofTable = false;
		   			ReadTableSaveItem();
		   			callCreateHistoryMenu();
		   			//saveItem();
		  		}
	  }
	  public void compare()
	  {
		  for (int i = 0; i < 19; i++ )
		  {
			  compareItem(i); 
		  }
	  }
	  public void compareItem(int i)
	  {
		 
		  if (regulusWindow.SentenceTable[i]!= null && regulusWindow.SentenceTable[i].equals(inPutValue))
			  
			  {
				  record_exist_in_table = true;
			  }
	  }
	  public void ReadTableSaveItem()
		 {
			 for (int i = 1;end_ofTable == false ; i++ )
			  {
				 if(regulusWindow.SentenceTable[i] == null)
				 {
				 regulusWindow.SentenceTable[i] = inPutValue;
				 end_ofTable = true;
				 }
				 else
				 {
				 System.out.println("regulusWindow.SentenceTable[i]"+regulusWindow.SentenceTable[i]); 
				 }
			  }
		 }
	  public void saveItem()
	  {
		  regulusWindow.SentenceTable[saveIndex] = inPutValue;
	  }
	  
	
	 public void callCreateHistoryMenu()
	 {
		 for (int i = 1;regulusWindow.SentenceTable[i]!= null ; i++ )
		  {
			 createHistoryIndex = i;
		  }
		 createHistoryMenu(); 
	 }


	public void createHistoryMenu()
	{
		 one_step_back_menuItem[createHistoryIndex] = new JMenuItem(inPutValue);
		
		 one_step_back_menuItem[createHistoryIndex].addActionListener(
			 		new ActionListener() {
			 			public void actionPerformed( ActionEvent e)
			 			{
			 				for (int menuIndex = 1; menuIndex < 19; menuIndex++ ) 
			 				{
			 					if (e.getSource() == one_step_back_menuItem[menuIndex])
			 					{ 		
				 				String menuString = one_step_back_menuItem[menuIndex].getText();
			 					inputField.setText(menuString);
			 					inPutValue = inputField.getText();
			 				  
			 					}
			 				}
			 							 				
			 			}
			 		}
	 		);
		 History_menu.setEnabled(true);
		 History_menu.add(one_step_back_menuItem[createHistoryIndex]);
	}
	
	 public void getMenuItem(int menuIndex)
	 {
		String menuString = one_step_back_menuItem[0].getText();
		inputField.setText(menuString); 
	 }
	  public void generateWarningMessage()
	  {
		   loadReply = JOptionPane.showConfirmDialog(null, 
		  		  "Reloading grammar can render Stepper Items invalid, do you still want to continue?" , // Prompt displayed in box 
		  		  "Delete Confirm Pane",                 // Label at top of box
		  		  JOptionPane.OK_CANCEL_OPTION);         // Option type for OK and Cancel buttons
		  		  // check which button is pressed OK = 0 CANCEL = 
		  		 
	  }
	  public void createBackGroundJob(String strCommand)
	  { 
	  	String CommandIdent = strCommand;
	  	new CommandInBackGround(regulusWindow,CommandIdent).start();
	  	
	  	new ReadOneCommandFile(regulusWindow,CommandIdent).start();
	  } 
	  
	  public void checkMenuStatus()
	  {
		regulusWindow.updateCommandStatus();
		regulusWindow.availablemenus.check_available_menus();
		regulusWindow.unavailablecommands.check_unavailable_menus();
	  }
		
	public void actionPerformed(ActionEvent e)
	  {
		  if (e.getSource() == RbtnEdit)
		  {
			  regulusWindow.handleCommand("EDIT");
			  RbtnEdit.setSelected(true);
			  RbtnDebug.setSelected(false);
			  edit_button = true;
		  }
		  else if (e.getSource() == RbtnDebug)
		  {
			  regulusWindow.handleCommand("DEBUG");
			  RbtnEdit.setSelected(false);
			  RbtnDebug.setSelected(true);
			  edit_button = false;
		  }
		
	  }  
	 public void checkNumberOfItems()
	 {
		 ResultIndex = maxIndex - minIndex;
		 if (ResultIndex > 0)
			 ThereAreManyDeleteItems = true;
		 else
			 ThereAreManyDeleteItems = false;
	 }
	 public void PutItemsInTable()
	 {
	 int index = 0;
	 for (int i = minIndex; i <= maxIndex; i++) {
		 IndexTable[index] = i;
		 index = index +1;
	 	}
 	 }
	  public void CreateAndLinktree(String label, String element)
	  {
//		 update the taskbar on the desktop
		treeCounter = treeCounter + 1;
		
		buttonThree = new JButton("Tree " +elementno);
		regulusWindow.barPanel.add( buttonThree);
		
		 TreePane treePane = new TreePane(this, getRegulusGUI(),label,elementno,edit_button,buttonThree,treeCounter);
		 treePane.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
		  JInternalFrame TreePaneInternalFrame = treePane.getInternalFrame();
		 //add the internal frame to the desktop
		regulusWindow.desktop.add(TreePaneInternalFrame,JLayeredPane.DEFAULT_LAYER);
		TreePaneInternalFrame.setVisible(true); 
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
	  public void checkItemNumbersBeforeGenerate()
	  {
		  getValueTable(); 
		  ItemsBeforeGenerate = saveIndex;
	  }
	  
	  public void checkItemNumbersAfterGenerate()
	  {
		  getValueTable(); 
		  ItemsAfterGenerate = saveIndex;
	  }
	  public void calculateItemsGenerated()
	  {
		  ItemsGenerated = ItemsAfterGenerate - ItemsBeforeGenerate; 
	  }
	  public void checkGeneratedNumbers()
	  {
		  if (regulusWindow.regulus_command_succeeded)
			{ 
				InputText = "Generate command succeeded "+ ItemsGenerated+" Items generated";
				regulusWindow.txtBoxDisplayPositive(InputText);
			
			}
			else
			{
				String command = regulusWindow.getCommandErrorString();
				InputText = command;
				regulusWindow.txtBoxDisplayPositive(InputText);
			}
	  }
	  
	public void getItem() {
	  String s3 = input;
	  temp = s3.split(" ");
	  dump(temp);
	  }

	  public void dump(String []s) {
	    for (int i = 0 ; i < s.length ; i++) {
	      System.out.println("table s "+s[i]);
	    }
	    
	  }
	
	  public void checkNum (String []s) {
		  for (int i = 0 ; i < s.length ; i++) {
			  try
			  {
			  int num =   Integer.parseInt(s[i]);
			  }
			  catch(NumberFormatException e)
			  {
				  InputIsNumeric = false; 
				  break;
			  }
		  }	  
	  }
	  public void checkDumpTable(String []s){
		  for (int i = 0 ; i < s.length ; i++) {
			  holdCombineInput = s[i];
			  InputIsInSummaryTable = checkSummaryTable();
			  if (InputIsInSummaryTable == false)
	  				JOptionPane.showMessageDialog(null, "Item number must exist in Summary List "
			  				,"Warning for combine",JOptionPane.INFORMATION_MESSAGE);
		    }  
	  }
	  public boolean checkSummaryTable() {
		  for ( int i = 1 ; !regulusWindow.stepperSummary[i].equals( "END OF TABLE" ); i++ )
		  {
			  CombineHoldSentence = regulusWindow.stepperSummary[i];
			  GetCombineElementNo();
			  InputIsInSummaryTable = false;
			  if (elementno.equals(holdCombineInput) )
			  {
				  InputIsInSummaryTable = true;
				  return InputIsInSummaryTable;
			  }
			
		  }
		  return InputIsInSummaryTable;
	  }
	 
	  public void checkMenuAnswer(){
//		 private static int YES_NO = 0, CHOOSE_NUMBER_FROM_LIST = 1, CHOOSE_ITEM_FROM_MENU = 2;
			String cat = "";  // cat entered by user
			String yesNo = ""; // User enter yes or no
			String Item = "";
			unpack_Menu_Items();
			textArea.setText(output);
			
			if (regulusWindow.menu_type == 1) 
			{
				cat = JOptionPane.showInputDialog(null,textArea,
						parseval +"\n Enter the number of your choice: ",JOptionPane.INFORMATION_MESSAGE);
				regulusWindow.handleCommand(cat); 
			}
			if (regulusWindow.menu_type == 0)
			{
			  yesNo = JOptionPane.showInputDialog(null,textArea,
				"Enter y for Yes or n for No: ",JOptionPane.INFORMATION_MESSAGE);
			  regulusWindow.handleCommand(yesNo); 
			}
			if (regulusWindow.menu_type == 2)
			{
				  Item = JOptionPane.showInputDialog(null,textArea,
				"Enter item number according to menu: ",JOptionPane.INFORMATION_MESSAGE);
			  regulusWindow.handleCommand(Item); 
			}
	  }
	  
				 
	  public void closeFrame(int count){
		  // Iterate ower the frames, closing them if they are open
		  		count = count - 1;
			  for (int j = 0; j < count; j++ ){
				  JInternalFrame f = allFrames[j];
				  if((f.isClosed()== false) || (f.isIcon()== true))
				  try{
				  f.setIcon(true);
				  regulusWindow.barPanel.add(f);
				  f.dispose();
				  }
			  catch (PropertyVetoException ex) {}
			  }
		  	}
		
			  
		  	  
	 	  public void vetoableChange(PropertyChangeEvent event)
		throws PropertyVetoException {
		JInternalFrame frame2 = (JInternalFrame) event.getSource();
		
		String name = event.getPropertyName();
		//System.out.println("name "+name);
		Object value = event.getNewValue();
		int count = 0;

		// we only want to check attempts to close a frame
		
		if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
			int result = JOptionPane.showConfirmDialog(frame2,
					"       OK to close?",
					"Close Confirm Pane",
					JOptionPane.OK_CANCEL_OPTION);  
			// if the user doesn't agree veto the close
			if (result == JOptionPane.CANCEL_OPTION)
				throw new PropertyVetoException("User cancelled close", event);
			else
			regulusWindow.handleCommand("EXIT_STEPPER","AUTOMATIC"); 
			setVisible(false);
			dispose();
			remove_finished = false;
			lookForIcon();
			return;
		}
		else if (name.equals("icon")&& value.equals(Boolean.TRUE))
		{
		
			
		}
		
}

	public void lookForIcon() {
		for (int i = 0; remove_finished == false ; i ++)  {
			 c = regulusWindow. barPanel.getComponent(i);
			 String strname = c.toString();
			 String b = strName;
			 b = "Stepp";
			 System.out.println("strName "+strName);
			 if (strname.indexOf(b) != -1) {
				Component myComponent = regulusWindow.barPanel.getComponent(i);
				 regulusWindow.barPanel.remove(myComponent);
				 regulusWindow.barPanel.repaint();
				 remove_finished = true;
			 }
			 else
			 {
				
				 
			 }
		 }
	}
	 
    
     public void internalFrameClosing(InternalFrameEvent e) {
     		 regulusWindow.stepperCounter--; 
    	    }

    	    public void internalFrameClosed(InternalFrameEvent e) {
    		
    	    }

    	    public void internalFrameOpened(InternalFrameEvent e) {
    		
    	    }

    	    public void internalFrameIconified(InternalFrameEvent e) {
    	    	getIconFrame();
     	    	remove_finished = false;
    			lookForIcon();
    		
    	    }

    	    public void internalFrameDeiconified(InternalFrameEvent e) {
    	    	getselectedFrame();
    	    	Stepperbtn =     new JButton(strName);
    	    	regulusWindow.barPanel.add(Stepperbtn);
    	    	Stepperbtn.addMouseListener(this);
    	    	getselectedFrame();
    	    	
    	    }

    	    public void internalFrameActivated(InternalFrameEvent e) {
    	    	
    	    }

    	    public void internalFrameDeactivated(InternalFrameEvent e) {
    		
    	    }
    	    
    	    public void mouseClicked(MouseEvent e)
    	    {
    	    }
    	    public void mousePressed(MouseEvent e)
    	    {
    	    	 // System.out.println("mousepressed");
    	  	  JButton button = (JButton)e.getSource();
    	  	  String cc = button.getActionCommand();
     	  	  if (cc.startsWith("Debugging"))
    	  	  {
    	  		  bc = debugbtn.getActionCommand();
    	  		  getAllFramesDebug();
    	  	  }
    	  	  else if (cc.startsWith("Stepp"))
    	  	  {
    	  		  ac = Stepbtn.getActionCommand();
     	  		  getAllFrames();
    	  	  }
    	    	
    	     
    	    }
    	    	
     	    public void getAllFrames(){
    	    	 frames = regulusWindow.desktop.getAllFrames();
    	    	 frameIndex = 0;
    	         int countFrames = frames.length;
    	         for (int i = 0; i < countFrames; i++) {
     	          String strFrames = frames[i].toString();
      	         holdFrameIndex = i;
    	         String b = ac;
     			 if (strFrames.indexOf(b) != -1) {
       				try {
       					frames[i].setSelected(true);
       				
     			  } catch (Exception e) {
     	             // This should not happen
     	             e.printStackTrace();
     	         }
       			 
     			}
    	      }
    		}
    	    
    	    public void getAllFramesDebug(){
   	    	 frames = regulusWindow.desktop.getAllFrames();
   	    	 frameIndex = 0;
   	         int countFrames = frames.length;
   	         for (int i = 0; i < countFrames; i++) {
    	          String strFrames = frames[i].toString();
     	         holdFrameIndex = i;
   	         String b = bc;
    			 if (strFrames.indexOf(b) != -1) {
      				try {
      					frames[i].setSelected(true);
      				
    			  } catch (Exception e) {
    	             // This should not happen
    	             e.printStackTrace();
    	         }
      			 
    			}
   	      }
   		}
    	 
    	    	
    	    public void getselectedFrame(){
   	    	 frames = regulusWindow.desktop.getAllFrames();
   	    	 frameIndex = 0;
   	         int countFrames = frames.length;
   	         for (int i = 0; i < countFrames; i++) {
    	          String strFrames = frames[i].toString();
     	         holdFrameIndex = i;
   	         String b = "Stepper";
    			 if (strFrames.indexOf(b) != -1) {
    				 
      				if (frames[i].isSelected())
      				{
      					strName = frames[i].getTitle();
      					//System.out.println("strName "+strName);
      				}
      			 
    			}
   	      }
   	  }
    	 
    	    public void getIconFrame(){
      	    	 frames = regulusWindow.desktop.getAllFrames();
      	    	 frameIndex = 0;
      	         int countFrames = frames.length;
      	         for (int i = 0; i < countFrames; i++) {
       	          String strFrames = frames[i].toString();
        	         holdFrameIndex = i;
      	         String b = "Stepper";
       			 if (strFrames.indexOf(b) != -1) {
       				 
         				if (frames[i].isIcon())
         				{
         					strName = frames[i].getTitle();
          				}
         			 
       			}
      	      }
      	  }	    	
   	   
    	    
    	    public void mouseReleased(MouseEvent e)
    	    {
    	    }
    	    public void mouseEntered(MouseEvent e)
    	    {
    	    	//System.out.println("mouse entered");
    	    	
    	    }
    	    public void mouseExited(MouseEvent e)
    	    {
    	    }    

	  public void unpack_Menu_Items(){
		  StringBuffer sb = new StringBuffer();
		  sb.append("\n"+"'"+parseval+"'"+"\n");
		  for ( int i = 1 ; !regulusWindow.menu_items[i].equals( "END OF TABLE" ); i++ )
		  {
			  sb.append("\n"+i + " "+regulusWindow.menu_items[i]); 
			  output = sb.toString();
		  }
	  }
	  
	  public int checkBothItemContainsCut(){
	  
	  if (firstItemContainsCut && secondItemContainsCut){
		  String inputValue = JOptionPane.showInputDialog("Please enter which item should come first");
		  int returnItem;
		  returnItem = Integer.parseInt(inputValue);
		  return returnItem;
	  	}
	  	  return firstItem;	
	  }
	  
	  public void setvalueParse(String parsevalue)
	  {
		  ParsecommandFull = ParsecommandPart1 + parseval;
		  regulusWindow.handleCommand(ParsecommandFull);
		
			if (regulusWindow.regulus_command_succeeded)
			{ 
				display_message = false;
				checkErrorString();
				if (display_message)
				{
					String command = regulusWindow.getCommandErrorString();
					InputText = command;
					regulusWindow.txtBoxDisplayPositive(InputText);
				}
			}
			else
			{
				String command = regulusWindow.getCommandErrorString();
				InputText = command;
				regulusWindow.txtBoxDisplayPositive(InputText);
			}

	 }
	 public void checkErrorString() {
		  String b = "Words not in current";
		  	if (regulusWindow.getCommandErrorString().indexOf(b) != -1) {
		  		display_message = true;
		  	}
	 }
	  public void setvalueLex(String lexvalue)
	  {
		  LexcommandFull = LexcommandPart1 + lexval;
		  regulusWindow.handleCommand(LexcommandFull);
			if (regulusWindow.regulus_command_succeeded)
			{ 
				//InputText = "Lex command succeeded";
				//regulusWindow.txtBoxDisplayPositive(InputText);
			
			}
			else
			{
				String command = regulusWindow.getCommandErrorString();
				InputText = command;
				regulusWindow.txtBoxDisplayPositive(InputText);
			}
		  
	 }
	  
	  public void setvalueGap(String gapvalue)
	  {
		  regulusWindow.handleCommand(gapval);
		 
	 }
	  public void setvalueCombine()
	  { 
		  CombinecommandFull = CombinecommandPart1 + input;
		  regulusWindow.handleCommand(CombinecommandFull);
		 
	 }
	  public void getValueTable()
	  {
		  // move summary tables summary elements to another table
		
		  if (regulusWindow.stepperSummary[1].equals("END OF TABLE"))
		  {
			  saveIndex = 0; 
		  }
		  for ( int i = 1 ; !regulusWindow.stepperSummary[i].equals( "END OF TABLE" ); i++ )
		  {
			  holdSummaryTable[i] = regulusWindow.stepperSummary[i];
			 // System.out.println("regulusWindow.stepperSummary[i] "+regulusWindow.stepperSummary[i]);
				saveIndex = i;  
		  }
			  
	  }
	  
 
	  
	  public void setfield()
	  { 
		  listModel.removeAllElements();
		 
		   // Set default listmodel
		
		 for ( int i = saveIndex ; i > 0 ; i-- )
		  {
		  listModel.addElement(holdSummaryTable[i]);
		   }
	  }
	  
	  public void deleteElement()
	  {
		holdSentence = thisElement.toString();
		int len = holdSentence.length();
		char[] tempCharArray = new char[len];
//		 put original string in an array of chars
		 for (int ind = 0; ind < len; ind++) {
		       tempCharArray[ind] = holdSentence.charAt(ind);
  		        } 
		  boolean secondcharIsNumber = true; 
		  char myFirstChar = tempCharArray[0];
		  char mySecondChar = tempCharArray[1];
		  String myFirstString = new Character(myFirstChar).toString();
		  String mySecondString = new Character(mySecondChar).toString();
		  
		  try
		  {
		  int num =   Integer.parseInt(mySecondString);
		  }
		  catch(NumberFormatException e)
		  {
			secondcharIsNumber = false; 
//		   str is not a number
		  }
		  
		  // Check if there are one or two numbers in the itemnumber which has been selected
		  if (secondcharIsNumber){
		  deleteval =  (myFirstString +mySecondString );
		  }
	        else
		  deleteval =  myFirstString;  
		  
		 
		  DeletecommandFull = DeletecommandPart1 + deleteval;
		  regulusWindow.handleCommand(DeletecommandFull);
		  int reply = JOptionPane.showConfirmDialog(null, 
		  "Do you really want to delete element " + deleteval, // Prompt displayed in box 
		  "Delete Confirm Pane",                 // Label at top of box
		  JOptionPane.YES_NO_OPTION);         // Option type for OK and Cancel buttons
		  // check which button is pressed OK = 0 CANCEL = 
		  if ( reply == 0)
		  { 
			  YesNoDelete = "y";
			  regulusWindow.handleCommand(YesNoDelete); 
			  checkCommandStatus();
		  }
		  else 
		  {
			  YesNoDelete = "n";
			  regulusWindow.handleCommand(YesNoDelete);   
		  }
	 
		}
	  public void DeleteManyItems()
	  {
		  int index = 0;
		  int addItem = 1;
		  // add one to index to get the item number
			 for (int i = minIndex; i <= maxIndex; i++) {
				 addItem = addItem + IndexTable[index];
				 IndexTable[index] = addItem;
				 index = index +1; 
				 addItem = 1;
			 }
			// go round and create message with the items you want to delete 
			 index = 0;
			 for (int i = minIndex; i <= maxIndex; i++) {
					 itemString += IndexTable[index] + "   ";
					 index = index +1; 
			 }
			 int reply = JOptionPane.showConfirmDialog(null, 
					  "Do you really want to delete these elements" + itemString, // Prompt displayed in box
					  "Delete Confirm Pane",                 // Label at top of box
					  JOptionPane.YES_NO_OPTION);         // Option type for OK and Cancel buttons
			
			 // check which button is pressed OK = 0 CANCEL = 
			  if ( reply == 0)
				  // if yes go into method which loops roun and deletes items
			  { 
				
				  DoDeletionOfManyItems();
			  }
			
	  }
	  public void DoDeletionOfManyItems()
	  {
	  DeletecommandFull = DeletecommandPart1 + itemString;
	  regulusWindow.handleCommand(DeletecommandFull);
	 // System.out.println("DeletecommandFull "+DeletecommandFull);
	  YesNoDelete = "y";
	  regulusWindow.handleCommand(YesNoDelete);
	  checkCommandStatus();
	  }
	  public void checkCommandStatus()
	  {
		  if (regulusWindow.regulus_command_succeeded)
			{ 
				InputText = "Delete command succeeded";
				regulusWindow.txtBoxDisplayPositive(InputText);
			
			}
			else
			{
				String command = regulusWindow.getCommandErrorString();	
				InputText = command;
				regulusWindow.txtBoxDisplayPositive(InputText);
			}
	  }
	  public void generateElement()
	  {
		holdSentence = thisElement.toString();
		int len = holdSentence.length();
		char[] tempCharArray = new char[len];
//		 put original string in an array of chars
		 for (int ind = 0; ind < len; ind++) {
		       tempCharArray[ind] = holdSentence.charAt(ind);
  		        } 
		  boolean secondcharIsNumber = true; 
		  char myFirstChar = tempCharArray[0];
		  char mySecondChar = tempCharArray[1];
		  String myFirstString = new Character(myFirstChar).toString();
		  String mySecondString = new Character(mySecondChar).toString();
		  
		  try
		  {
		  int num =   Integer.parseInt(mySecondString);
		  }
		  catch(NumberFormatException e)
		  {
			secondcharIsNumber = false; 
//		   str is not a number
		  }
		  // Check if there are one or two numbers in the itemnumber which has been selected
		  if (secondcharIsNumber){
		  generateval =  (myFirstString +mySecondString );
		  }
	        else
		  generateval =  myFirstString;  
		  
		 
		  GeneratecommandFull = GeneratecommandPart1 + generateval;
		  regulusWindow.handleCommand(GeneratecommandFull);
		  
			
		}
	  public void GetElementNo()
	  {
		  holdSentence = thisElement.toString();
		  int len = holdSentence.length();
		  char[] tempCharArray = new char[len];
//		  put original string in an array of chars
		  for (int ind = 0; ind < len; ind++) {
			tempCharArray[ind] = holdSentence.charAt(ind);
	  	  } 
		  boolean secondcharIsNumber = true; 
		  char myFirstChar = tempCharArray[0];
		  char mySecondChar = tempCharArray[1];
		  String myFirstString = new Character(myFirstChar).toString();
		  String mySecondString = new Character(mySecondChar).toString();
		  try
		  {
		  int num =   Integer.parseInt(mySecondString);
		  }
		  catch(NumberFormatException e)
		  {
			secondcharIsNumber = false; 
//		   str is not a number
		  }
		  
		  // Check if there are one or two numbers in the itemnumber which has been selected
		  if (secondcharIsNumber){
		  elementno =  (myFirstString +mySecondString );
		  }
	        else
		  elementno =  myFirstString;  
		  Treeval = Integer.parseInt(elementno);
	  }
	  public void GetCombineElementNo()
	  {
		  holdSentence = CombineHoldSentence;
		  int len = holdSentence.length();
		  char[] tempCharArray = new char[len];
//		  put original string in an array of chars
		  for (int ind = 0; ind < len; ind++) {
			tempCharArray[ind] = holdSentence.charAt(ind);
	  	  } 
		  boolean secondcharIsNumber = true; 
		  char myFirstChar = tempCharArray[0];
		  char mySecondChar = tempCharArray[1];
		  String myFirstString = new Character(myFirstChar).toString();
		  String mySecondString = new Character(mySecondChar).toString();
		  try
		  {
		  int num =   Integer.parseInt(mySecondString);
		  }
		  catch(NumberFormatException e)
		  {
			secondcharIsNumber = false; 
//		   str is not a number
		  }
		  
		  // Check if there are one or two numbers in the itemnumber which has been selected
		  if (secondcharIsNumber){
		  elementno =  (myFirstString +mySecondString );
		  }
	        else
		  elementno =  myFirstString;  
		  Treeval = Integer.parseInt(elementno);
	  }
	
	
	  class ItemCellRenderer extends DefaultListCellRenderer {
		    public Component getListCellRendererComponent(JList list, Object value,
		            int index, boolean isSelected, boolean cellHasFocus) {
		        JLabel label = (JLabel) super.getListCellRendererComponent(list, value,
		                index, isSelected, cellHasFocus);
		        //setText(value.toString());
		        String s = value.toString();
		        String b = "- old";
				 if (s.indexOf(b) != -1) 
		        {
		            label.setBackground(new Color(230,230,255));
		        }
		        return label;
		    }
		}
		    
	 
}


