 package RegulusGUI;

/**
 * Based on EvaluateGUI.java from PrologBeans directory 
 * Latest revision: July 20 2006 - move to Eclipse
 */
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import se.sics.prologbeans.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class RegulusGUI extends JFrame 
  implements ActionListener, VetoableChangeListener {
	
  // If true, prints available and unavailable commands
  private static boolean PRINT_TRACE_INFO_FOR_AVAILABLE_COMMANDS = false;
  // If true, prints batch translation results
  private static boolean PRINT_TRACE_INFO_FOR_BATCH_TRANSLATION = false;
  //If true, tries to run calls to Prolog in background using SwingWorker. So far, doesn't work very well.
  private static boolean USE_SWING_WORKER_FOR_PROLOG_CALLS = false;
   public static int DEFAULT_PORT = 8066;
  private PrologSession session = null;
   JMenuBar bar = new JMenuBar();  // create menubar
  public JMenuBar desktopBar;
  public JTextArea text = new JTextArea(30, 50);
  private JTextField input = new JTextField(36);
  private JButton evaluate = new JButton("Evaluate");
  private boolean in_stepper_mode = false;
  private String command = "";
  private String last_command = "";
  private boolean command_possibly_involving_menus = false;
  public boolean regulus_command_succeeded = false;
  //public boolean stepper_command_succeeded = false;
  public boolean stepper_used = false; 
  // If menu answer is required, will have value 'true'
  public boolean menu_answer_required = false;
  public boolean  checkMenuStatus = false; 
  // If a menu answer is required, check value of 'menu_type'. It should be one of the following.
  private static int YES_NO = 0, CHOOSE_NUMBER_FROM_LIST = 1, CHOOSE_ITEM_FROM_MENU = 2;
  public String[] menu_items = new String[MAX_MENU_LENGTH];
  public int menu_type = 0;
  // If a menu answer is required, 'menu_question' should be the question to put on the menu
  private String menu_question = "";
  private static int MAX_MENU_LENGTH = 100;
  private int[] numbers_to_choose_from = new int[MAX_MENU_LENGTH];
  // If a menu answer is required, the array menu_items will contain the lines, ending with "END OF TABLE"
  private Container c1;
  public String outAnswer = ""; 
  private static final int MAX_STEPPER_SUMMARY_ITEMS = 1000;
  public String[] stepperSummary = new String[MAX_STEPPER_SUMMARY_ITEMS];
  public String[] SentenceTable = new String[20]; 
  private int[] stepperItemIdChanges = new int[MAX_STEPPER_SUMMARY_ITEMS];
  private boolean stepper_summary_ids_have_changed = false;
  private static final int MAX_POSSIBLE_COMMANDS = 1000;
  public String[] availableCommands = new String[MAX_POSSIBLE_COMMANDS];
  public String[] unavailableCommands = new String[MAX_POSSIBLE_COMMANDS];
  private static final int MAX_POSSIBLE_CONFIG_FILES = 1000;
  private static final int MAX_POSSIBLE_COMMANDS_PER_CONFIG_FILE = 50;
  public  String[][] configFiles = new String[MAX_POSSIBLE_CONFIG_FILES][MAX_POSSIBLE_COMMANDS_PER_CONFIG_FILE];
  private String[] Start_Menu_Item =  new String [MAX_POSSIBLE_CONFIG_FILES];
  private static final int MAX_BATCH_TRANSLATION_RESULTS = 5000;
  public  String batchTranslationResultsFile = null;
  public  TranslationResult[] batchTranslationResults = new TranslationResult [MAX_BATCH_TRANSLATION_RESULTS];
  public  JDesktopPane desktop = new JDesktopPane();
  private int i = 0;
  public   JMenu WindowMenu = new JMenu("Start New Window");
  public  JMenu FlagMenu = new JMenu( "Set Flags" );
  private String menu_command = "";
  private String commandErrorString = "";
  public  JMenu Echo_Menu;
  public  JMenu Parser_Menu; 
  public  JMenu Compaction_Menu;
  public  JMenu Answer_Ellipsis_Menu;
  public  JMenuItem L_loadMenu;
  public  JMenu 	loadMenu;
  public  JCheckBox check_surface_parser;
  public  JMenuItem EBL_loadMenuItem;
  public  JMenuItem Start_New_Second_Window_startMenuItem;
  public  JMenuItem Start_New_Third_Window_startMenuItem;
  public  JMenuItem Start_New_Fourth_Window_startMenuItem;
  public  JMenuItem EBL_Generation_loadMenuItem; 
  public  JMenuItem EBL_Generation_Arg_loadMenuItem;
  public  JMenuItem Dialogue_loadMenuItem;
  public  JMenuItem Generation_loadMenuItem;
  public  JMenuItem Generation_Arg_loadMenuItem; 
  public  JMenuItem load_recognition_loadMenuItem;
  public  JMenuItem close_down_recognition_loadMenuItem; 
  public  JMenuItem Surface_Patterns_loadMenuItem;
  public  JMenuItem Translate_loadMenuItem;
  public  JMenuItem Compile_Ellipsis_Patterns_loadMenuItem; 
  public  JMenuItem Gemini_loadMenuItem;
  public  JMenuItem Dialogue_menu;
  public  JCheckBox check_Answer_Ellipsis_on;
  public  JCheckBox check_Answer_Ellipsis_off; 
  public  JMenuItem Nuance_loadMenuItem; 
  public  JMenuItem normal_processing_menu;
  public  JCheckBox check_dcg_parser; 
  public  JMenuItem Translate_menu;
  public  JCheckBox check_compaction_on;
  public  JCheckBox check_compaction_off; 
  public  JMenuItem compaction_off_menu; 
  public  JMenuItem compaction_on_menu;
  public  JCheckBox check_lc_parser;
  public  JCheckBox check_echo_on;
  public  JCheckBox check_echo_off;
  public  JMenu stepper_Menu;
  public  JMenu specialised_Menu;
  public  JMenuItem Ebl_menuItem;
  public  JMenuItem Ebl_Train_menuItem;
  public  JMenuItem Ebl_Analysis_menuItem;
  public  JMenuItem Ebl_Gemini_menuItem;
  public  JMenuItem Ebl_Generation_menuItem;
  public  JMenuItem Ebl_Treebank_menuItem;
  public  JMenuItem Ebl_Grammar_Probs_menuItem;
  public  JMenuItem Ebl_Postprocess_menuItem;
  public  JMenuItem Ebl_Nuance_menuItem;
  public  JMenuItem Compile_Nuance_to_recognicer_meuItem;
  public  JMenuItem Compile_Nuance_to_recognicer_pcfg_meuItem;
  public  JMenuItem Generation_menu;
  public  JMenuItem translate_on_Menu; 
  public  JMenuItem Dialogue_on_Menu; 
  public  JMenuItem training__Menu; 
  public  int num_UnAvailable = 0;
  public  int num_Available = 0;
  private JMenu startMenu;
  public JMenuItem configItems[];
  public JCheckBox checkConfigItems[];
  public int firstIndex = 0;
  public int saveIndex = 0; 
  String InputText = "";
  public  JInternalFrame frame1;
  public String label;
  public int fileIndex = 0;
  public String commandString = "";
  public  int len = 0;
  public  char[] tempCharArray;
  private boolean this_is_answer_command = false;
  private boolean this_is_question_command = false;
  private boolean this_is_normal_command = false;
  private String newCommand = "";
  private String myFirstString = "";
  private int port = 0;
  private RegulusGUI regulusgui = null;
  public  UnavailableCommands unavailablecommands = null;
  public  AvailableMenus availablemenus = null;
  public String getResult = "";  
  public boolean user_does_not_want_to_execute_list_of_commands = false;
  private String[] Commands = new String[15];
  public int reply = 0;
  private JTextArea outputTextArea;
  private boolean command_succeeded = false;
  public  boolean command_run_in_Background = false;
  private boolean optionPane_already_shown = false;
  private int holdIndex = 0;
  private int jDisplay = 0;
  public String[] Commands_In_Back_Ground = new String[15];
  public  boolean[] True_Or_False = new boolean[15];
  private int backGroundIndex = 0;
  public boolean execution_is_ready = false; 
  public boolean is_this_list_of_command = false;
  private int holdCheckIndex = 0;
  public  JPanel barPanel = new JPanel(new FlowLayout());
  public String[] availableWindows = new String[20];
  public  int windowIndex = 0;
  public  JButton buttonOne, buttonTwo,buttonThree,buttonFour;
  public  int mouseClickCounter = 0;
  public  int stepperCounter = 0;
  public  int translationCounter = 0;
  public  int dialoguecounter = 0; 
  public  int judgeCounter = 0;
  public String[] speechSentenceTable = new String[100];
  public int textLength = 0;
  public boolean  config_file_loaded = false;
  private JButton[] btnFix = new JButton[10];
  private int fixIndex = 0;
  static private final Rectangle bottom = new Rectangle( 0, Integer.MAX_VALUE, 0, 0 );
 
 
  
  public class ProgressFileInfo {
	  private String file;
	  
	  private int number_of_lines;
	  
	  private String search_pattern;
	  
	  public String getFile() {
		  return file;
	  }
	  
	  public int getNumberOfLines() {
		  return number_of_lines;
	  }
	  
	  public String getSearchPattern() {
		  return search_pattern;
	  }
	  
	  public ProgressFileInfo() {
	  }
	  
	  public ProgressFileInfo(String f, int n, String p) {
		  file = f;
		  number_of_lines = n;
		  search_pattern = p;
	  }
	  
	  public String toString() {
		  return "ProgressFileInfo: " + file + " (" + 
		         number_of_lines + " records; search pattern = " + search_pattern + ")";
	  }
  }
  
  public class WavfileInfo {
	  private String fileName;
	  
	  private String timeStamp;
	  
	  private String words;
	  
	  public String getFileName() {
		  return fileName;
	  }
	  
	  public String getTimeStamp() {
		  return timeStamp;
	  }
	  
	  public String getWords() {
		  return words;
	  }
	  
	  public WavfileInfo() {
	  }
	  
	  public WavfileInfo(String t, String f, String w) {
		  fileName = f;
		  timeStamp = t;
		  words = w;
	  }
	  
	  public String toString() {
		  return "WavfileInfo: " + fileName + " (" + 
		          timeStamp + " ; " + words + ")";
	  }
  }
  
  public RegulusGUI() {
	  JDesktopPane desktop = new JDesktopPane();
	  new RegulusGUI(label, desktop , DEFAULT_PORT, 0, 0);
	  desktop.setOpaque(false);
  }

  public RegulusGUI(String label,JDesktopPane  desktopVar ,int port, int X_POS, int Y_POS) {
	  desktop.setOpaque(false);
	  desktop = desktopVar;
	  unavailablecommands = new UnavailableCommands(this);
	  availablemenus = new AvailableMenus(this);
	  
    session = new PrologSession();
    session.setPort(port);
	session.setTimeout(2000000);
	  
	// Find out what config files there are, and put them in the configFiles array
	updateConfigFileTable();
	
   // create menubar for desktop
	desktopBar = new JMenuBar();
	setJMenuBar( desktopBar );  // set the menubar for the JDesktopPane
	
	// create menubar for internal frame
    JMenuBar bar = new JMenuBar();  // create menubar
    setJMenuBar( bar );  // set the menubar for the JInternalFrame
 
    // get the content pane
    
    getContentPane().add(desktop);
    
    // create the initial internal frame to show
    frame1 = new JInternalFrame(label,true,true,true,true);
    
    makeandLinkNewDesktopMenus();
    
    //desktopBar.add(WindowMenu);
    
    //  create START  menu item
    	
    startMenu = new JMenu( "Choose Config File" );
    startMenu.setMnemonic( 'C');
   
   // buildStartMenu();
    createCheckBoxMenuStartItems();
    bar.add(startMenu);
    
   
   //  create LOAD  menu item and sub menu items
    
    
    makeandLinkNewRegulusMenus();
   // bar.add(loadMenu);
    bar.add(FlagMenu); 
    
//  create Stepper  menu item and sub menu item
    stepper_Menu = new JMenu( "Mode" );
    stepper_Menu.setMnemonic( 'M');
    JMenuItem stepper_on_Menu = new JMenuItem( "Start Stepper" );
    stepper_on_Menu.setToolTipText("Start debugging Regulus grammar");
    stepper_on_Menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				handleCommand("START_STEPPER");
    				
    				// create a copy of stepper windov class 
    				
    				makeAndLinkNewFrame2();
    				
  			  	
  				}  	
        	}
        );	
       
    stepper_Menu.add(stepper_on_Menu);
    bar.add(stepper_Menu);
    
//  create Translat menu item
    
    translate_on_Menu = new JMenuItem( "Start Translate" );
    translate_on_Menu.setToolTipText("Start translation");
    translate_on_Menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				handleCommand("LOAD_DIALOGUE");
    				
    				// create a copy of translate windov class 
    				
    				makeAndLinkNewFrame3();
    				
  			  	
  				}  	
        	}
        );	
       
    stepper_Menu.add(translate_on_Menu);
    bar.add(stepper_Menu);
    if (config_file_loaded )
    	stepper_Menu.setEnabled(true);
    else
    	stepper_Menu.setEnabled(false);
 
//  create dialogue processing menu item
    
    Dialogue_on_Menu = new JMenuItem( "Start Dialogue Processing" );
    Dialogue_on_Menu.setToolTipText("Start Dialogue Processing");
    Dialogue_on_Menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				handleCommand("TRANSLATE");
    				
    				// create a copy of translate windov class 
    				
    				makeAndLinkNewFrame4();
    				
  			  	
  				}  	
        	}
        );	
    
    stepper_Menu.add(Dialogue_on_Menu);
    bar.add(stepper_Menu);
    
    makeandLinkNewSpecialisationMenu();
    bar.add(specialised_Menu);
    if (config_file_loaded )
    	specialised_Menu.setEnabled(true);
    else
    	specialised_Menu.setEnabled(false);
   

    availableWindows[windowIndex] = "Main";
    windowIndex++;
   
    c1 = frame1.getContentPane();
    
    //pane.append( text );
    //pane.scrollRectToVisible( bottom );
     
    int line = text.getLineCount();
    textLength = textLength + line;
   
    text.setCaretPosition(textLength);
    System.out.println("textLength "+textLength);
    
    text.setCaretPosition(text.getDocument().getLength());
    text.requestFocus();
    
    
    c1.add(new  JScrollPane(text), BorderLayout.CENTER);
    
    JPanel inputPanel = new JPanel(new BorderLayout());
   
    inputPanel = new JPanel(new BorderLayout());
    inputPanel.add(input, BorderLayout.CENTER);
    inputPanel.add(evaluate,BorderLayout.EAST);
    inputPanel.add(bar,BorderLayout.NORTH);
    
    //barPanel.add(sysTray.getContent());
    buttonOne = new JButton("Debugging Trace");
    barPanel.add(buttonOne);
     
    c1.add(inputPanel,BorderLayout.NORTH  );
   
    text.setEditable(false);
    evaluate.addActionListener(this);
    input.addActionListener(this);
    frame1.addVetoableChangeListener(this);
  
    setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
   
     frame1.pack();
     
    // attach internal frame to desktop and show it
     
     
     this.desktop = new JDesktopPane();
     setLayout(new BorderLayout());
     add(desktopBar, BorderLayout.NORTH);
     add(this.desktop, BorderLayout.CENTER);
     add(barPanel, BorderLayout.SOUTH);
    
    desktop.add(frame1);
  
    desktop.setVisible(true);
    frame1.setVisible(true);
 
    // show desktop
    
    setSize(1000,700);
    setVisible(true);
   
  	}
 
 public void createButtonTwo() {
	  buttonTwo = new JButton("Stepper");
     buttonTwo.setActionCommand("Trace");
     // Set the actions
     buttonTwo.addActionListener(this);
    
     barPanel.add(buttonTwo);
   
}
  public void makeandLinkNewRegulusMenus(){
	  RegulusMenus regulusmenus = new RegulusMenus(this);
  }
  
  public void makeandLinkNewDesktopMenus(){
	  DesktopMenus desktopmenus = new DesktopMenus(this);
  }
  
  public void makeandLinkNewSpecialisationMenu(){
	  SpecialisationMenu specialisationmenu = new SpecialisationMenu(this);
  }
  
  public void makeandLinkNewUnAvailableCommands(){
	  UnavailableCommands unavailablecommands = new UnavailableCommands(this);
  }
  public void makeandLinkNewAvailableMenus(){
	  AvailableMenus availablemenus = new AvailableMenus(this);
  }
 public void makeAndLinkNewFrame2() {
	 
//	 update the taskbar on the desktop 
	stepperCounter = stepperCounter + 1;
	buttonTwo = new JButton("Stepper " +stepperCounter);
	barPanel.add( buttonTwo);
	
	
	Frame2 frame2 = new Frame2(this,buttonTwo,stepperCounter,buttonOne);
	
	frame2.setRegulusGUI(this);
	
	
//	display new internal window
  	JInternalFrame frame2InternalFrame = frame2.getInternalFrame();
   	
 //   add the taskbar to the desktop
  	
  	add(desktopBar, BorderLayout.NORTH); 
	add(this.desktop, BorderLayout.CENTER);
	add(barPanel, BorderLayout.SOUTH);
	//desktop.add(barPanel, BorderLayout.SOUTH);
  	desktop.setVisible(true);
  	
  	
  	 //   add the internal frame to the desktop
  	
  	desktop.add(frame2InternalFrame);
  	frame2InternalFrame.setVisible(true);
  	frame2.getValueTable();
  	frame2.setfield();
  	
 }
 

 public void makeAndLinkNewTrainingPane() {
		TrainingPane trainingpane = new TrainingPane();
		
		trainingpane.setRegulusGUI(this);
			  	
//		display new internal window
	  	JInternalFrame trainingpaneInternalFrame = trainingpane.getInternalFrame();
	  	
//	 add the internal frame to the desktop	
	  	desktop.add(trainingpaneInternalFrame);
	  	trainingpaneInternalFrame.setVisible(true);
	  
	 }
 public void makeAndLinkNewFrame3() {
	 handleCommand("TRANSLATE_TRACE_ON","AUTOMAIC GUI CALL");
	 
	 
//	 update the taskbar on the desktop 
	translationCounter  = translationCounter + 1;
	buttonThree = new JButton("Translator " +translationCounter);
	barPanel.add( buttonThree);
	 
	Frame3 frame3 = new Frame3(this,buttonThree,translationCounter,buttonOne);
		
	frame3.setRegulusGUI(this);
			  	
//		display new internal window
	  	JInternalFrame frame3InternalFrame = frame3.getInternalFrame();	
	  	
		add(desktopBar, BorderLayout.NORTH); 
		add(this.desktop, BorderLayout.CENTER);
		add(barPanel, BorderLayout.SOUTH);
		//desktop.add(barPanel, BorderLayout.SOUTH);
	  	desktop.setVisible(true);
	  	
	  	
	  	 //   add the internal frame to the desktop
	  	
	  	desktop.add(frame3InternalFrame);
	  	frame3InternalFrame.setVisible(true);
	 }

  	
 public void makeAndLinkNewFrame4() {
	 handleCommand("LOAD_DIALOGUE");
	 
//	 update the taskbar on the desktop 
		dialoguecounter  = dialoguecounter + 1;
		buttonFour = new JButton("Dialogue " +dialoguecounter);
		//barPanel.add( buttonFour);
		fixIndex++; 
		System.out.println("fixIndex in adding "+fixIndex);
		btnFix[fixIndex]= buttonFour;
		barPanel.add(btnFix[fixIndex]);
	 
		Frame4 frame4 = new Frame4(this,buttonFour,dialoguecounter,buttonOne);
		
		frame4.setRegulusGUI(this);
			  	
//		display new internal window
	  	JInternalFrame frame4InternalFrame = frame4.getInternalFrame();	
	  	
	  	add(desktopBar, BorderLayout.NORTH); 
		add(this.desktop, BorderLayout.CENTER);
		add(barPanel, BorderLayout.SOUTH);
		//desktop.add(barPanel, BorderLayout.SOUTH);
	  	desktop.setVisible(true);
	  	
//	 add the internal frame to the desktop	
	  	desktop.add(frame4InternalFrame);
	  	frame4InternalFrame.setVisible(true);
	 }
   public JTextArea textArea() {
	   return text;
   }
   public void deleteButton() {
	   System.out.println("fixIndex in deleting "+fixIndex);
	   barPanel.remove(btnFix[fixIndex]);
	   //barPanel.remove( buttonOne);
	   barPanel.repaint();
	 
   }
  
  public void actionPerformed(ActionEvent event) {
	  JButton button = (JButton)event.getSource();
	  String ac = button.getActionCommand();
	  if(ac.equals("Stepper")){
	         System.out.println("ActionEvent set by buttonDebugging Trace");
	  }
	  handleCommand(input.getText(), false);
  }
  
  public String getCommandErrorString() {
	  return commandErrorString;
  }
  
  private void setCommandErrorString(String s) {
	  commandErrorString = s;
  }
  
  public void buildStartMenu(){
//	  create sub menu items to START
	  configItems = new JMenuItem[100]; 
	  for ( firstIndex = 0 ; !configFiles[firstIndex][1].equals( "END OF TABLE" ); firstIndex++ ){
		 configItems[firstIndex]=
			 new JMenuItem(configFiles[firstIndex][0]);
		 MenuAction(firstIndex);
	    startMenu.add(configItems[firstIndex]);
	   // bar.add(startMenu);
	  }     
  }
  public void createCheckBoxMenuStartItems() {
	  checkConfigItems = new JCheckBox[100];
	  for ( firstIndex = 0 ; !configFiles[firstIndex][1].equals( "END OF TABLE" ); firstIndex++ ){
		  checkConfigItems[firstIndex]=
				 new JCheckBox(configFiles[firstIndex][0]);
		  MenuAction(firstIndex);
		  startMenu.add(checkConfigItems[firstIndex]);
		
	  }
  }
   public void txtBoxDisplayPositive(String txtBoxText){
	  JOptionPane.showMessageDialog(null,txtBoxText,"Information ",JOptionPane.INFORMATION_MESSAGE);
		  
  }
  public void txtBoxDisplayNegative(String txtBoxText){
	  JOptionPane.showMessageDialog(null,txtBoxText,"Information ",JOptionPane.INFORMATION_MESSAGE);
		  
  }
  public void MenuAction(int firstIndex){
	
	  checkConfigItems[firstIndex].addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{ 
	    				System.out.println(" I am here");
	    			   for (int i =0; i< checkConfigItems.length; i++ ) {
	    				   //checkConfigItems[i].setSelected(false);  
	    				   if ( e.getSource() == checkConfigItems[i]) {
	    					   loadConfigFile(configFiles[i][1]);
	    					   checkConfigItems[holdCheckIndex ].setSelected(false);
	    					   fileIndex = i;
	    					   holdCheckIndex = i;
	    					   runAsCommandToConfigFile();
	    					   //System.out.println("configFiles[i][2]"+configFiles[i][2]);
	    					   updateCommandStatus();
	    					   makeandLinkNewAvailableMenus();
	    					   availablemenus.check_available_menus();
	    					   makeandLinkNewUnAvailableCommands();
	    					   unavailablecommands.check_unavailable_menus();
	    					   config_file_loaded = true;
	    					   loadMenu.setEnabled(true);
	    					   FlagMenu.setEnabled(true);
	    					   stepper_Menu.setEnabled(true);
	    					   stepper_Menu.setEnabled(true);
	    					   specialised_Menu.setEnabled(true);
	    					   return;
	    				   }
	    			   }
	    			   System.out.println("*** Error: couldn't find a config file to load");
	    			}
	    	}
	   	);
  }
  
 
 public void runAsCommandToConfigFile()
 {
	 optionPane_already_shown = false;
	 getHoldOfListOfCommands();
	 outputTextArea = new JTextArea(holdIndex,20);
	 outputTextArea.setText("");
	 backGroundIndex = 0;
	 for (int j = 2; (configFiles[fileIndex][j] != null);j++)
	 {
		 commandString = configFiles[fileIndex][j];
		 checkCommand();
		 checkQuestionorAnswer();
		 getHoldOfListOfCommands();
		 if(user_does_not_want_to_execute_list_of_commands == false)
		 {
				createOptionPane();
				  // check which button is pressed OK = 0 CANCEL = 
					  if ( reply == 0)
					  { 
						executeListOfCommands();
						
					  }
					  else 
					  {
					  // user does not want to execute list of commands
					  user_does_not_want_to_execute_list_of_commands = true;
					  }
		 		}
			else
			{
				 outputTextArea.setText(""); 
			}
		 }
	 // this modul checks if everything went well do this after the whole list is executed
	 //checkCommandList();
	 createBackGroundRun();
	 //readBackGround();
	 user_does_not_want_to_execute_list_of_commands = false;
 }

	
 public void checkCommandList()
 {
	 if ( reply == 0)
	 {
	 if (command_succeeded)
		{ 
		 createOptionPaneSuccessfullyLoaded();
		 JOptionPane.showMessageDialog(null,outputTextArea,"Information ",JOptionPane.INFORMATION_MESSAGE);
		}
		else
		{
		 createOptionPaneUnSuccessfullyLoaded();
		 JOptionPane.showMessageDialog(null,"Starting Configuration File ","Information ",JOptionPane.INFORMATION_MESSAGE);
		}
	 }
 }
 public void executeListOfCommands()
 {
	 executeCommand();
	 if (regulus_command_succeeded)
		{ 
		 	command_succeeded = true;
		}
		else
		{
			 command_succeeded = false;
		}
	
 }
 public void getHoldOfListOfCommands()
 {
	 for (int j = 2; (configFiles[fileIndex][j] != null);j++)
	 {
		 Commands[j] =  configFiles[fileIndex][j];
		 holdIndex = j; 
	 }
 }
 public void createOptionPane()
 {
	 if (optionPane_already_shown == false)
	 {
	
	 outputTextArea.append("Do you want to load this list of Command/s\n" );
	 	for (int j = 2; Commands[j] != null;j++)
	 	{
	 		jDisplay = j;
			outputTextArea.append(Commands[j] +"\n");
	 	}
	 	reply = JOptionPane.showConfirmDialog(null, 
	 		outputTextArea,									//Pompt displayed in box 
	 		"Load Confirm Pane",                 			// Label at top of box
	 		JOptionPane.OK_CANCEL_OPTION);        			 // Option type for OK and Cancel buttons
	 	optionPane_already_shown = true;
	 }
 }
 public void createOptionPaneSuccessfullyLoaded()
 {
	 outputTextArea.setText(""); 
	 outputTextArea.append("You have successfully loaded this list of Command/s\n" );
	 	for (int j = 2; Commands[j] != null;j++)
	 	{
			 outputTextArea.append(Commands[j] +"\n");
	 	}
 }
 public void createOptionPaneUnSuccessfullyLoaded()
 {
	 outputTextArea.setText(""); 
	 outputTextArea.append("You have successfully loaded this list of Command/s\n" );
	 	for (int j = 2; Commands[j] != null;j++)
	 	{
			 outputTextArea.append(Commands[j] +"\n");
	 	}
 }
 public void checkCommand()
 { 
 	len = commandString.length();
 	tempCharArray = new char[len];
// 	put original string in an array of chars
 	for (int ind = 0; ind < len; ind++) {
 	tempCharArray[ind] = commandString.charAt(ind);
 	}
 }
 
 public void checkQuestionorAnswer()
 {
	 char myFirstChar = tempCharArray[0];
	 myFirstString = new Character(myFirstChar).toString();
	 if (myFirstString.equals("A"))
	 {
		 this_is_answer_command = true;
		 newCommand = commandString.substring(2,len);
	 }
	 else if (myFirstString.equals("Q"))
	 {
		 this_is_question_command = true;
		 newCommand = commandString.substring(2,len);
	 }
	 else
	 {
		 this_is_normal_command = true;
		 newCommand = commandString;
	 }
 }
 public void executeCommand()
 {
	 if (myFirstString.equals("A"))
	 {
		 is_this_list_of_command = true;
		 handleRemoteCommand(newCommand);
	 }
	 else if (myFirstString.equals("Q"))
	 {
		 is_this_list_of_command = true;
		 handleCommand(newCommand);
	 }
	 else
	 {
		 is_this_list_of_command = true;
		 handleCommand(newCommand);
	 }
 }
 
 public void createCommand(){
	 
 }
  public void loadConfigFile(String file) {
	    Bindings bindings = null;
	    QueryAnswer answer = null;
	    Term result = null;    

	    try {
	    	bindings = new Bindings().bind("ConfigFile", file);
		    answer = session.executeQuery("start_regulus(ConfigFile, Response)", bindings);

		    if ( answer != null ) {
		    	result = answer.getValue("Response");
		    	if (result != null) {
		    		if ( result.isString() ) {
		    			text.append( "\n\n" + ( (PBString) result ).getString() + "\n" );
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    			String Answer2 = ("\n\n" + ( (PBString) result ).getString() + "\n");
		    			outAnswer = Answer2;
		    		} 
		    		else {
		    			text.append("Error: result was not a string\n");
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    		}
		    	} 
		    	else {
		    		text.append("Error: " + answer.getError() + '\n');
		    		text.setCaretPosition(text.getDocument().getLength());
		     	    text.requestFocus();
		    	}
		    }

		    input.setText("");
		
	    } catch (Exception e) {
		text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		text.setCaretPosition(text.getDocument().getLength());
 	    text.requestFocus();
		e.printStackTrace();
	    }
	    
	  }
  
  
  public boolean checkIfInBidirectionalMode() {
	  return checkRegulusStatus("bidirectional_on");
  }
  
  public boolean checkRegulusStatus(String key) {
	    Bindings bindings = null;
	    QueryAnswer answer = null;
	    Term result = null;    

	    try {
	    	bindings = new Bindings().bind("Key", key);
		    answer = session.executeQuery("check_regulus_status(Type, YesNo)", bindings);

		    if ( answer != null ) {
		    	result = answer.getValue("YesNo");
		    	if (result != null) {
		    		if ( result.isString() ) {
		    			String resultString = ( (PBString) result ).getString();
		    			text.append("checkRegulusStatus(" + key + ") = " + resultString + "\n");
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    			if (resultString.equals("yes")) {
		    				text.append("checkRegulusStatus(" + key + "): return true");
		    				text.setCaretPosition(text.getDocument().getLength());
		    		  	    text.requestFocus();
		    				return true;
		    			}
		    		} 
		    		else {
		    			text.append("Error: result of checkRegulusStatus(" + key + ") was not a string\n");
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    		}
		    	}
		    	else {
		    		text.append("checkRegulusStatus(" + key + ") failed\n");
		    		text.setCaretPosition(text.getDocument().getLength());
		     	    text.requestFocus();
		    	}
		    }
		    text.append("checkRegulusStatus(" + key + "): return false");
		    text.setCaretPosition(text.getDocument().getLength());
	  	    text.requestFocus();
		    return false;
		
	    } catch (Exception e) {
		text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		text.setCaretPosition(text.getDocument().getLength());
 	    text.requestFocus();
		e.printStackTrace();
		return false;
	    }
	    
	  }
  
  public String getRegulusFile(String key, String inputOrOutput) {
	    Bindings bindings = null;
	    QueryAnswer answer = null;
	    Term result = null;    

	    try {
	    	bindings = new Bindings().bind("KeyString", key);
		    answer = session.executeQuery("get_regulus_file(KeyString, File, inputOrOutput)", bindings);

		    if ( answer != null ) {
		    	result = answer.getValue("File");
		    	if (result != null) {
		    		if ( result.isAtom() ) {
		    			String resultString = ( (PBAtomic) result ).toString();
		    			text.append("getRegulusStatus(" + key + ") = " + resultString + "\n");
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    			return resultString;
		    		} 
		    		else {
		    			text.append("Error: result of getRegulusFile(" + key + ") was not a Prolog atom\n");
		    			text.setCaretPosition(text.getDocument().getLength());
		    	  	    text.requestFocus();
		    		}
		    	}
		    	else {
		    		text.append("getRegulusFile(" + key + ") failed\n");
		    		text.setCaretPosition(text.getDocument().getLength());
		     	    text.requestFocus();
		    	}
		    }
		    return null;
		
	    } catch (Exception e) {
		text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		text.setCaretPosition(text.getDocument().getLength());
 	    text.requestFocus();
		e.printStackTrace();
		return null;
	    }
	    
	  }
  
  public void handleCommand(String command) {
	  if (is_this_list_of_command)
	  {
		 String strCommand = command;
		 Commands_In_Back_Ground[backGroundIndex] = strCommand;
		 True_Or_False[backGroundIndex] = false;
		 backGroundIndex = backGroundIndex + 1;
		 is_this_list_of_command = false; 
	  }
	  else
	  {
	  handleCommand(command,false);
	  }
  }
  
  public void handleRemoteCommand(String command) {
	  if (is_this_list_of_command)
	  {
		 String strCommand = command;
		 Commands_In_Back_Ground[backGroundIndex] = strCommand;
		 True_Or_False[backGroundIndex] = true;
		 backGroundIndex = backGroundIndex + 1;
		 is_this_list_of_command = false; 
	  }
	  else
	  {
	  handleCommand(command,true);
	  }
	  
  }
  public void readBackGround()
  {
	  for (int j = 0; Commands_In_Back_Ground[j] != null;j++)
	  {
		  System.out.println("Commands_In_Back_Ground[j] "+Commands_In_Back_Ground[j]);
	  }
  }
  public void createBackGroundRun()
  {
		  createBackGroundJobLight();
  }
 
  public void createBackGroundJobLight()
  {
		//CommandInBackGroundList commandinbackgroundlist = new CommandInBackGroundList(this);
		
	  	//commandinbackgroundlist.start();
	  	
	  	ReadManyCommands readmanycommands = new ReadManyCommands(this);
	  	
	  	readmanycommands.start();
	  	
  }

  public void handleCommand(String new_command, String comment) {
      handleCommand(new_command, comment, false);
	    }

  public void handleRemoteCommand(String new_command, String comment) {
      handleCommand(new_command, comment, true);
	    }

  public void handleCommand(String new_command, boolean remote) {
      handleCommand(new_command, "*null_comment*", remote);
	    }

 
  public void handleCommand(String new_command, String comment, boolean remote) {
	  
    text.append("\n\nStart handleCommand " + new_command + " " + comment + "\n");
    text.setCaretPosition(text.getDocument().getLength());
    text.requestFocus();
    Bindings bindings = null;
    QueryAnswer answer = null;
    Term result = null;
    Term menuTerm = null;
    Term errorTerm = null;
    boolean already_shown_response = false;

    last_command = command;
    command = new_command;
    stepper_summary_ids_have_changed = false;
    try {

	 if ( command.equals("START_STEPPER") && !remote ) {
	    answer = session.executeQuery("start_regulus_stepper(Response)");
	    in_stepper_mode = true;
	    command_possibly_involving_menus = false;
	    updateStepperSummary();
	    if ( stepperSummary[1] == null ) {
	    	text.append("\nstepperSummary[1] is null\n");
	    } else {
	    	text.append("\nstepperSummary[1] = " + stepperSummary[1] + "\n");
	    	text.append("Stepper command succeeded\n");
	    	text.setCaretPosition(text.getDocument().getLength());
	    	text.requestFocus();
	    }
	    stepper_summary_ids_have_changed = false;
	    
	}
	else if ( command.equals("EXIT_STEPPER") ) {
	    text.append( "\n\n" + "Leaving stepper, now in normal mode" + "\n");
	    text.setCaretPosition(text.getDocument().getLength());
    	text.requestFocus();
	    in_stepper_mode = false;
	    already_shown_response = true;
	    command_possibly_involving_menus = false;
	}
	else if ( in_stepper_mode && !menu_answer_required ) {
	    bindings = new Bindings();
	    bindings.bind("Command", command);
	    bindings.bind("Comment", comment);
	    answer = session.executeQuery("execute_stepper_command(Command, Comment, null, Response, " +
	    		                      "Status, ChangeTerm, MenuTerm, Errors)", 
	    		                      bindings);
	    command_possibly_involving_menus = true;
	    // put in move text here
	   
	    
	}
	else if ( in_stepper_mode && menu_answer_required ) {
	    bindings = new Bindings();
	    bindings.bind("Command", last_command);
	    bindings.bind("Comment", comment);
	    bindings.bind("MenuAnswer", command);
	    answer = session.executeQuery("execute_stepper_command(Command, Comment, MenuAnswer, Response, " +
	    	                      "Status, ChangeTerm, MenuTerm, Errors)", 
	    		                      bindings);
	    //answer = ExecutePrologCallInBackground("execute_stepper_command(Command, MenuAnswer, Response, " +
        //        "Status, ChangeTerm, MenuTerm, Errors)", 
         //       bindings);
	    command_possibly_involving_menus = true;
	    
	}
	else if ( !in_stepper_mode && !menu_answer_required ) {
	    bindings = new Bindings();
	    bindings.bind("Command", command);
	    bindings.bind("Comment", comment);
	    if ( remote == false ) {
	    	answer = session.executeQuery("execute_regulus_command(Command, Comment, null, Response, " +
	    			                      "MenuTerm, Status, Errors)", 
	    			                      bindings);
	    }
	    else {
	    	answer = session.executeQuery("remote_execute_regulus_command(Command, Comment, null, Response, " +
	    			                      "MenuTerm, Status, Errors)", 
	    			                      bindings);
	    	
	    }
	    command_possibly_involving_menus = true;
	}
	else if ( !in_stepper_mode && menu_answer_required ) {
	    bindings = new Bindings();
	    bindings.bind("Command", last_command);
	    bindings.bind("Comment", comment);
	    bindings.bind("MenuAnswer", command);
	    if ( remote == false ) {
	    	answer = session.executeQuery("execute_regulus_command(Command, Comment, MenuAnswer, Response, " +
	    					              "MenuTerm, Status, Errors)", 
	    					              bindings);
	    }
	    else {
	    	answer = session.executeQuery("remote_execute_regulus_command(Command, Comment, MenuAnswer, Response, " +
	    			                      "MenuTerm, Status, Errors)", 
	    			                      bindings);
	    }
	    command_possibly_involving_menus = true;
	}

	if ( !already_shown_response && answer != null ) {
	    result = answer.getValue("Response");
	    if (result != null) {
	    	if ( result.isString() ) {
	    		getResult = ( (PBString) result ).getString();
	    		//System.out.println("getResult "+getResult);
	    		text.append( "\n\n" + ( (PBString) result ).getString() + "\n" );
	    		text.setCaretPosition(text.getDocument().getLength());
	    	    text.requestFocus();
	    		String Answer2 = ("\n\n" + ( (PBString) result ).getString() + "\n");
	    		outAnswer = Answer2;
		   
	    	} else {
	    		
	    		text.append("Error: result was not a string\n");
	    		text.setCaretPosition(text.getDocument().getLength());
	    	    text.requestFocus();
	    	}
	    } 
	    else {
	    	text.append("Error: " + answer.getError() + '\n');
	    	text.setCaretPosition(text.getDocument().getLength());
    	    text.requestFocus();
	    }
	    
	    // Get the current error messages and save them
	    errorTerm = answer.getValue("Errors");
	    if ( errorTerm != null ) {
	    	setCommandErrorString( ( (PBString) errorTerm ).getString() );
	    }
	    else {
	    	setCommandErrorString("NO ERROR");
	    }
	    text.append("\n\nERRORS: \n\n" + getCommandErrorString() + "\n\nEND ERRORS\n");
	    text.setCaretPosition(text.getDocument().getLength());
	    text.requestFocus();
	    
	    if ( command_possibly_involving_menus ) {
	    	menuTerm = answer.getValue("MenuTerm");
	    	if ( menuTerm != null && menuTerm.isCompound() ) {
	    		menu_answer_required = true ;
	    		unpackMenuTerm(menuTerm);
	    	}
	    	else {
	    		menu_answer_required = false ;
	    		updateCommandStatus();
	    		updateStepperSummary();
	    	}
	    }
	    
	    // If command was START_STEPPER, then we have just entered stepper mode,
	    // so we don't have Status or ChangeTerms to look at.
	    if ( in_stepper_mode && !command.equals("START_STEPPER") ) {
	    	Term statusTerm = answer.getValue("Status");
	    	Term changeTerm = answer.getValue("ChangeTerm");
	    	if ( statusTerm != null && statusTerm.isAtom() ) {
	    		String status = statusTerm.getName();
	    		if ( status.equals("ok") ) {
	    			regulus_command_succeeded = true;
	    			text.append("Stepper command succeeded\n");
	    			text.setCaretPosition(text.getDocument().getLength());
		    	    text.requestFocus();
	    			
	    		}
	    		else {
	    			regulus_command_succeeded = false;
	    			text.append("Stepper command failed\n");
	    			text.setCaretPosition(text.getDocument().getLength());
		    	    text.requestFocus();
	    		}
	    	}
	    	else if ( statusTerm == null ){
	    		text.append("Error: status field is null - unclear whether stepper command succeeded or failed");
	    		text.setCaretPosition(text.getDocument().getLength());
	    	    text.requestFocus();
	    	}
	    	else if ( !statusTerm.isAtom() ){
	    		text.append("Error: status field is not an atom");
	    		text.setCaretPosition(text.getDocument().getLength());
	    	    text.requestFocus();
	    	}
	    	unpackStepperChangeTerm(changeTerm);
	    }
	    else {
	    	Term statusTerm = answer.getValue("Status");
	    	if ( statusTerm != null && statusTerm.isAtom() ) {
	    		String status = statusTerm.getName();
	    		if ( status.equals("ok") ) {
	    			regulus_command_succeeded = true;
	    			text.append("Regulus command succeeded\n");
	    			text.setCaretPosition(text.getDocument().getLength());
		    	    text.requestFocus();
	    		}
	    		else {
	    			regulus_command_succeeded = false;
	    			text.append("Regulus command failed\n");
	    			text.append("Stepper command succeeded\n");
	    			text.setCaretPosition(text.getDocument().getLength());
		    	    text.requestFocus();
	    			
	    		}
	    	}
	    }
	    
	    // For debugging...
	    
	    if ( command.equals("getProgress LOAD") ) {
	    	getProgressFileInfo("LOAD");
	    }
	    if ( command.equals("getWavfiles") ) {
	    	WavfileInfo[] wavfiles = getWavfileInfo(3);
		for ( int i = 0; i < wavfiles.length; i++ ) {
		    text.append("\n Wavfile " + i + ":\n" + wavfiles[i].toString() + "\n");
		    text.setCaretPosition(text.getDocument().getLength());
    	    text.requestFocus();
		}
	    }
	    if ( command.equals("recFromLastWavfile") ) {
	    	WavfileInfo[] wavfiles = getWavfileInfo(1);
		String last_wavfile = wavfiles[0].getFileName();
		text.append("\n Doing recognition from " + last_wavfile + "\n");
		text.append("\n Result: " + doSpeechRecognitionFromWavfile(last_wavfile) + "\n");
		text.setCaretPosition(text.getDocument().getLength());
	    text.requestFocus();
	    }
	    else if ( command.equals("getProgress TRANSLATE_CORPUS") ) {
	    	getProgressFileInfo("TRANSLATE_CORPUS");
	    }
	    else if ( command.equals("end_session")) {
	    	endRegulusSession();
	    }
	    else if ( command.equals("recognise")) {
	    	text.append( "String returned from doSpeechRecogition(): \"" + doSpeechRecognition() + "\"\n" );
	    	text.setCaretPosition(text.getDocument().getLength());
    	    text.requestFocus();
	    }
	    else if ( command.equals("getDialogue 1")) {
	    	performDialogueProcessing("switch on the light");
	    }
	    
	}

	input.setText("");
	text.append("\nEnd handleCommand " + new_command + "\n");
	text.setCaretPosition(text.getDocument().getLength());
    text.requestFocus();
    
  } catch (Exception e) {
	  text.append("Error when querying Prolog server");
	  text.setCaretPosition(text.getDocument().getLength());
	  text.requestFocus();
  	}
  }

  private void unpackStepperChangeTerm(Term changeTerm) {
	  if ( changeTerm == null ) {
		  text.append("\nError: stepper change info is null\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  stepper_summary_ids_have_changed = false;
	  }
	  else if ( changeTerm.isAtomic() ) {
		  text.append("\nStepper summary IDs are unchanged\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  stepper_summary_ids_have_changed = false;
	  }
	  else {
		  text.append("\nStepper summary IDs have changed\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  stepper_summary_ids_have_changed = true;
		  for ( int i = 1 ; i < MAX_STEPPER_SUMMARY_ITEMS ; i++ ) {
			  stepperItemIdChanges[i] = -1;
		  }
		  for ( int i = 1 ; i <= changeTerm.getArity() ; i++ ) {
			  Term changeLine = changeTerm.getArgument(i);
			  if ( changeLine != null && changeLine.getArity() == 2 &&
					  changeLine.getArgument(1).isInteger() && 
					  changeLine.getArgument(2).isInteger()) {
				  int fromVal = changeLine.getArgument(1).intValue();
				  int toVal = changeLine.getArgument(2).intValue();
				  stepperItemIdChanges[fromVal] = toVal;
				  text.append(fromVal + " -> " + toVal + "\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }  
		  }
	  }
  }
  
  // This has to be called immediately after the change, otherwise it's
  // not guaranteed to be correct
  public int newStepperIdCorrespondingToOldOne( int oldID ) {
	  if ( stepper_summary_ids_have_changed && 
			  oldID > 0 && oldID < MAX_STEPPER_SUMMARY_ITEMS && 
			  stepperItemIdChanges[oldID] != -1 ) {
		  return stepperItemIdChanges[oldID];
	  }
	  else {
		  return oldID;
	  }
  }
  
  private void updateConfigFileTable() {
	  text.append("\nTrying to update config file table\n");
	  try {
		  QueryAnswer answer = session.executeQuery("get_config_file_alist(AlistTerm)");
		  Term alistTerm = answer.getValue("AlistTerm");
		  if ( alistTerm != null && alistTerm.isCompound() ) {
			  int arity = alistTerm.getArity();
			  int i, j;
				  for ( i = 1 ; i <= arity ; i++ ) {
					  Term alistItem = alistTerm.getArgument(i);
					  int alistItemArity = alistItem.getArity();
					  if ( alistItem != null && alistItem.isCompound() && alistItemArity >= 2 ) {
						  for ( j = 1 ; j <= alistItemArity ; j++ ) {
							  configFiles[i - 1][j - 1] = ( (PBString) alistItem.getArgument(j) ).getString();
						  }
						  text.append("\n" + configFiles[i - 1][0] + ": " + configFiles[i - 1][1]);
						  text.setCaretPosition(text.getDocument().getLength());
				  	      text.requestFocus();
						  if ( alistItemArity == 2 ) {
							  text.append(" (No associated commands)");
							  text.setCaretPosition(text.getDocument().getLength());
					  	      text.requestFocus();
						  }
						  else {
							  text.append("\n   Associated commands:");
							  text.setCaretPosition(text.getDocument().getLength());
					  	      text.requestFocus();
							  for ( j = 3; j <= alistItemArity; j++ ) {
								  text.append(" \"" + configFiles[i - 1][j - 1] + "\"");
								  text.setCaretPosition(text.getDocument().getLength());
						  	      text.requestFocus();
							  }
						  }
					  } 			
				  }
				  configFiles[i - 1][0] = "END OF TABLE";
				  configFiles[i - 1][1] = "END OF TABLE";
				  text.append("\n\nUpdated config file table: " + alistTerm.getArity() + " items\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  } 
			  else if ( alistTerm != null && alistTerm.isAtom() ) {
				  configFiles[0][0] = "END OF TABLE";
				  configFiles[0][1] = "END OF TABLE";
				  text.append("\nWarning: couldn't find any config files\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
			  else {
				  text.append("\nError: unable to update config file table\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
		  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  e.printStackTrace();
		  }
	  }
  
  private void updateStepperSummary() {
      updateStepperSummary("*null_comment*");
  }
  
  private void updateStepperSummary(String comment) {
	  if ( in_stepper_mode ) {
		  try {
                          Bindings bindings = new Bindings();
	                  bindings.bind("Comment", comment);
			  QueryAnswer answer = session.executeQuery("get_stepper_summary(Comment, SummaryTerm)");
			  Term summaryTerm = answer.getValue("SummaryTerm");
			  if ( summaryTerm != null && summaryTerm.isCompound() ) {
				  int arity = summaryTerm.getArity();
				  int i;
				  for ( i = 1 ; i <= arity ; i++ ) {
					  stepperSummary[i] = ( (PBString) summaryTerm.getArgument(i) ).getString();
				  }
				  stepperSummary[i] = "END OF TABLE";
				  text.append("\n Updated stepper summary: " + summaryTerm.getArity() + " items\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  } 
			  else if ( summaryTerm != null && summaryTerm.isAtom() && ( summaryTerm.getName() ).equals("summary") ) {
				  stepperSummary[1] = "END OF TABLE";
				  text.append("\n Updated stepper summary: no items\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
			  else {
				  text.append("\nError: unable to update stepper summary array\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
		  } 
		  catch (Exception e) {
			  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
			  e.printStackTrace();
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  }
	  }
  }
  
  /*
   *  Possible values for menu-term (Prolog notation):
   *  
   *  - yes_or_no(PromptString)
   * 
   *  - choose_number(PromptString, ListTerm)
   *  
   *  - choose_from_menu(PromptString, ListTerm)
   */
    
  private void unpackMenuTerm(Term menuTerm) {
	  String menuType = menuTerm.getName();
	  menu_question = ((PBString) menuTerm.getArgument(1)).getString();
	  System.out.println("Question = '" + menu_question + "'");
	  if ( menuType.equals("yes_or_no") ) {
		  menu_type = YES_NO;
		  System.out.println("Yes/No question");
	  }
	  else if ( menuType.equals("choose_number") ) {
		  menu_type = CHOOSE_NUMBER_FROM_LIST;
		  unpackNumbersToChooseFrom(menuTerm.getArgument(2));
	  }
	  else if ( menuType.equals("choose_from_menu")) {
		  menu_type = CHOOSE_ITEM_FROM_MENU;
		  unpackMenuItem(menuTerm.getArgument(2));
	  }
	  else {
		  System.out.println("Error: unknown menu type: " + menuType);
	  }
  }
  
  private void unpackNumbersToChooseFrom(Term ListTerm) {
	  int i;
	  for ( i = 1 ; i <= ListTerm.getArity() ; i++ ) {
		  numbers_to_choose_from[i] = (ListTerm.getArgument(i)).intValue();
		  System.out.println("numbers_to_choose_from[" + i + "] = " + numbers_to_choose_from[i]);
	  }
	  numbers_to_choose_from[i] = -1;
  }
  
  private void unpackMenuItem(Term ListTerm) {
	  int i;
	  for ( i = 1 ; i <= ListTerm.getArity() ; i++ ) {
		  menu_items[i] = ( (PBString) ListTerm.getArgument(i) ).getString();
		  System.out.println("menu_items[" + i + "] = " + menu_items[i]);
	  }
	  menu_items[i] = "END OF TABLE";
  }
  
  public void updateCommandStatus() {
	  try {
		  QueryAnswer answer = session.executeQuery("get_regulus_command_status(PosTerm, NegTerm)");
		  Term posTerm = answer.getValue("PosTerm");
		  Term negTerm = answer.getValue("NegTerm");
			  if ( posTerm != null && posTerm.isCompound() ) {
				  int arity = posTerm.getArity();
				  int i;
				  for ( i = 1 ; i <= arity ; i++ ) {
					  availableCommands[i] = ( (PBString) posTerm.getArgument(i) ).getString();
					  if ( PRINT_TRACE_INFO_FOR_AVAILABLE_COMMANDS ) {
					  text.append("\nAvailable command: " + availableCommands[i]);
					  text.setCaretPosition(text.getDocument().getLength());
			  	      text.requestFocus();
					  }
				  }
				  availableCommands[i] = "END OF TABLE";
				  if ( PRINT_TRACE_INFO_FOR_AVAILABLE_COMMANDS ) {
				  text.append("\n\nTotal of " + posTerm.getArity() + 
				  		  	  " available commands\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
				  }
				  num_Available = posTerm.getArity();
			  } 
			  else if ( posTerm != null && posTerm.isAtom() && ( posTerm.getName() ).equals("positive") ) {
				  availableCommands[1] = "END OF TABLE";
			  }
			  else {
				  text.append("\nError when updating command status\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
			  
			  if ( negTerm != null && negTerm.isCompound() ) {
				  int arity = negTerm.getArity();
				  int i;
				  for ( i = 1 ; i <= arity ; i++ ) {
					  unavailableCommands[i] = ( (PBString) negTerm.getArgument(i) ).getString();
					  if ( PRINT_TRACE_INFO_FOR_AVAILABLE_COMMANDS ) {
					  text.append("\nUnavailable command: " + unavailableCommands[i]);
					  text.setCaretPosition(text.getDocument().getLength());
			  	      text.requestFocus();
					  }
				  }
				  unavailableCommands[i] = "END OF TABLE";
				  if ( PRINT_TRACE_INFO_FOR_AVAILABLE_COMMANDS ) {
				  text.append("\n\n Total of " + negTerm.getArity() + 
				  	  	  " unavailable commands\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
				  }
				  num_UnAvailable = negTerm.getArity();
			  } 
			  else if ( negTerm != null && negTerm.isAtom() && ( negTerm.getName() ).equals("negative") ) {
				  unavailableCommands[1] = "END OF TABLE";
			  }
			  else {
				  text.append("\nError when updating command status\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
			 
		  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  }
  }
  
  public TranslationResult performTranslation(String sourceLanguageString) {
	  return performTranslation(sourceLanguageString, false);
  }
  
  public TranslationResult performRemoteTranslation(String sourceLanguageString) {
	  return performTranslation(sourceLanguageString, true);
  }
  
  public TranslationResult performTranslation(String sourceLanguageString, boolean remote) {
	  QueryAnswer answer;
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("SourceString", sourceLanguageString);
		  if ( remote == false ) {
			  answer = session.executeQuery("perform_translation(SourceString, AnswerTerm)", bindings);
		  }
		  else {
			  answer = session.executeQuery("remote_perform_translation(SourceString, AnswerTerm)", bindings);
		  }
		  Term answerTerm = answer.getValue("AnswerTerm");
		  if ( answerTerm != null && answerTerm.isCompound() ) {
			  TranslationResult translationResult = new TranslationResult();
			  translationResult.initFromProlog( answerTerm );
			  text.append("\nReturning TranslationResult for: " + sourceLanguageString + "\n");
			  text.append(translationResult.toString());
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return translationResult;
		  }
		  else {
			  text.append("\nError: unable to perform translation for " + sourceLanguageString + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
  public DialogueProcessingResult performDialogueProcessing(String s) {
	  QueryAnswer answer;
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("SourceString", s);
		  answer = session.executeQuery("perform_dialogue_processing(SourceString, AnswerTerm)", bindings);
		  Term answerTerm = answer.getValue("AnswerTerm");
		  if ( answerTerm != null && answerTerm.isCompound() ) {
			  DialogueProcessingResult dialogueResult = new DialogueProcessingResult();
			  dialogueResult.initFromProlog( answerTerm );
			  text.append("\nReturning DialogueProcessingResult for: " + s + "\n");
			  text.append(dialogueResult.toString());
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return dialogueResult;
		  }
		  else {
			  text.append("\nError: unable to perform translation for " + s + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
  public int getBatchTranslationResults(String id) {
	  QueryAnswer answer;
	  Term answerTerm;
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("Id", id);
		  answer = session.executeQuery("read_translation_output_file(Id, AnswerTerm)", bindings);
		  answerTerm = answer.getValue("AnswerTerm");
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return 0;
		  }
	  if ( answerTerm != null && answerTerm.isCompound() && answerTerm.getArity() == 2 ) {
		  text.append("\nGot batch translation results for " + id + "\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return unpackBatchTranslationResults( (Term) answerTerm );
	  }
	  else {
		  text.append("\nError: unable to get batch translation results for '" + id + "'\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return 0;
	  }
  }
  
  public int getBatchSpeechTranslationResults(String id) {
	  QueryAnswer answer;
	  Term answerTerm;
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("Id", id);
		  answer = session.executeQuery("read_speech_translation_output_file(Id, AnswerTerm)", bindings);
		  answerTerm = answer.getValue("AnswerTerm");
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return 0;
		  }
	  if ( answerTerm != null && answerTerm.isCompound() && answerTerm.getArity() == 2 ) {
		  text.append("\nGot batch speech translation results for " + id + "\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return unpackBatchTranslationResults(answerTerm);
	  }
	  else {
		  text.append("\nError: unable to get batch speech translation results for '" + id + "'\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return 0;
	  }
  }
  
public int unpackBatchTranslationResults(Term answerTerm) {
	  PBList answerList;
	  String answerFile;
	  if ( answerTerm.isCompound() && answerTerm.getArity() == 2 
			  && answerTerm.getArgument(1).isString() && answerTerm.getArgument(2).isList() ) {
		  answerFile = ((PBString) answerTerm.getArgument(1)).getString();
		  answerList = ((PBList) answerTerm.getArgument(2));
	  }
	  else {
		  text.append("\nUnable to unpack batch translation results - arguments are of wrong form\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return 0;
	  } 
	  batchTranslationResultsFile = answerFile;
	  int length = answerList.getLength();
	  int i;
	  boolean unpackedOK;
	  for ( i = 1; i <= length; i++ ) {
		  TranslationResult translationResult = new TranslationResult();
		  try {
			  unpackedOK = translationResult.initFromProlog( answerList.getTermAt(i) );
		  }
		  catch (Exception e) {
			  text.append("Error when unpacking result: " + e.getMessage() + '\n');
			  e.printStackTrace();
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  unpackedOK = false;
			  }
		  if ( unpackedOK ) {
			  batchTranslationResults[i - 1] = translationResult;
			  if ( PRINT_TRACE_INFO_FOR_BATCH_TRANSLATION ) {
				  text.append("\n\nResult " + (i - 1) + "\n");
				  text.append(translationResult.toString());
				  text.append("\n");
				  text.setCaretPosition(text.getDocument().getLength());
		  	      text.requestFocus();
			  }
		  }
		  else {
			  text.append("\nError: unable to unpack term " + answerList.getTermAt(i) + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  batchTranslationResults[i - 1] = null;
		  }
	  }
	  batchTranslationResults[i] = null;
	  text.append("Unpacked batch translation results (" + i + "records)\n");
	  text.setCaretPosition(text.getDocument().getLength());
	  text.requestFocus();
	  return length;
  }

public boolean putBatchTranslationResults(String id) {
	int i;
	if ( batchTranslationResultsFile != null ) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(batchTranslationResultsFile));
			for ( i = 0; batchTranslationResults[i] != null; i++ ) {
				out.write(batchTranslationResults[i].toStringForJudgements());
				out.write(".\n\n");
			}
			out.close();
			text.append("\nWrote batch translation results to " + batchTranslationResultsFile + " (" + i + " records)\n");
			text.setCaretPosition(text.getDocument().getLength());
	  	    text.requestFocus();
			return true;
		} catch (IOException e) {
			text.append("\nError when trying to write batch translation results to " + batchTranslationResultsFile + "\n");
			text.setCaretPosition(text.getDocument().getLength());
	  	    text.requestFocus();
			return false;
		}
	}
	else {
		text.append("\nError when trying to write batch translation results: file is not defined\n");
		text.setCaretPosition(text.getDocument().getLength());
 	    text.requestFocus();
		return false;
	}
}

public boolean putBatchSpeechTranslationResults(String id) {
	return putBatchTranslationResults(id);
}

  public RegulusSummaryItem getStepperItem(int itemNumber) {
      return getStepperItem(itemNumber, "*null_comment*");
  }
  
  public RegulusSummaryItem getStepperItem(int itemNumber, String comment) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("ItemNumber", itemNumber);
		  bindings.bind("Comment", comment);
		  QueryAnswer answer = session.executeQuery("show_item(Comment, ItemNumber, Summary)", bindings);
		  Term prologSummary = answer.getValue("Summary");
		  if ( prologSummary != null && prologSummary.isCompound() ) {
			  RegulusSummaryItem summaryItem = new RegulusSummaryItem();
			  summaryItem.initFromProlog( prologSummary );
			  text.append("\nReturning RegulusSummaryItem " + itemNumber + "\n");
			  text.append(summaryItem.toString());
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return summaryItem;
		  }
		  else {
			  text.append("\nError: unable to get stepper item " + itemNumber + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }

  public RegulusSummaryItem getStepperItem(int itemNumber, int nodeNumber) {
      return getStepperItem(itemNumber, nodeNumber, "*null_comment*");
  }
  
  public RegulusSummaryItem getStepperItem(int itemNumber, int nodeNumber, String comment) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("ItemNumber", itemNumber);
		  bindings.bind("NodeNumber", nodeNumber);
		  bindings.bind("Comment", comment);
		  QueryAnswer answer = session.executeQuery("show_item(Comment, ItemNumber, NodeNumber, Summary)", bindings);
		  Term prologSummary = answer.getValue("Summary");
		  if ( prologSummary != null && prologSummary.isCompound() ) {
			  RegulusSummaryItem summaryItem = new RegulusSummaryItem();
			  summaryItem.initFromProlog( prologSummary );
			  text.append("\nReturning RegulusSummaryItem " + itemNumber + ", node " + nodeNumber + "\n");
			  text.append(summaryItem.toString());
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return summaryItem;
		  }
		  else {
			  text.append("\nError: unable to get stepper item " + itemNumber + ", node " + nodeNumber + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }

  public String getStepperItemRule(int itemNumber, int nodeNumber) {
      return getStepperItemRule(itemNumber, nodeNumber, "*null_comment*");
  }
  
  public String getStepperItemRule(int itemNumber, int nodeNumber, String comment) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("ItemNumber", itemNumber);
		  bindings.bind("NodeNumber", nodeNumber);
		  bindings.bind("Comment", comment);
		  QueryAnswer answer = session.executeQuery("show_rule_for_item(Comment, ItemNumber, NodeNumber, RuleString)", bindings);
		  Term prologRuleString = answer.getValue("RuleString");
		  if ( prologRuleString != null ) {
			  String ruleString = ((PBString) prologRuleString).getString();
			  text.append("\nReturning rule:\n\n" + ruleString + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return ruleString;
		  }
		  else {
			  text.append("\nError: unable to get rule for stepper item " + itemNumber + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
 
  public RegulusSummaryItem getStepperItemNode(int itemNumber, int nodeNumber) {
      return getStepperItemNode(itemNumber, nodeNumber, "*null_comment*");
  }

  public RegulusSummaryItem getStepperItemNode(int itemNumber, int nodeNumber, String comment) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("ItemNumber", itemNumber);
		  bindings.bind("NodeNumber", nodeNumber);
		  bindings.bind("Comment", comment);
		  QueryAnswer answer = session.executeQuery("show_item(Comment, ItemNumber, NodeNumber, Summary)", bindings);
		  Term prologSummary = answer.getValue("Summary");
		  if ( prologSummary != null && prologSummary.isCompound() ) {
			  RegulusSummaryItem summaryItem = new RegulusSummaryItem();
			  summaryItem.initFromProlog( prologSummary );
			  text.append("\nReturning node " + nodeNumber + " from RegulusSummaryItem " + itemNumber + "\n");
			  text.append(summaryItem.toString());
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return summaryItem;
		  }
		  else {
			  text.append("\nError: unable to get node " + nodeNumber + " from stepper item " + itemNumber + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 

	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
  public ProgressFileInfo[] getProgressFileInfo(String command) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("CommandString", command);
		  QueryAnswer answer = session.executeQuery("get_progress_file_info(CommandString, InfoTerm)", bindings);
		  Term info = answer.getValue("InfoTerm");
		  if ( info != null && info.isCompound() ) {
			  int nFiles = info.getArity();
			  ProgressFileInfo[] info_array = new ProgressFileInfo[nFiles];
			  for ( int i = 0; i < nFiles; i++ ) {
				  Term item = info.getArgument(i + 1);
				  if ( item != null && item.isCompound() && item.getArity() == 3 ) {
					  	String fileName = ((PBString) item.getArgument(1)).getString();
				  		int nRecords = (item.getArgument(2)).intValue();
				  		String searchPattern = ((PBString) item.getArgument(3)).getString();
				  		if ( searchPattern.equals("*no_pattern*") ) {
				  			searchPattern = null;
				  		}
				  		info_array[i] = new ProgressFileInfo(fileName, nRecords, searchPattern);
				  		text.append("\nFound " + info_array[i] + "\n");
				  		text.setCaretPosition(text.getDocument().getLength());
				  	    text.requestFocus();
				  }
			  }
			  return info_array;
		  }
		  else {
			  text.append("\nError: unable to get progress info for \"" + command + "\"\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
  public WavfileInfo[] getWavfileInfo(int nWavfiles) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("NWavfiles", nWavfiles);
		  QueryAnswer answer = session.executeQuery("get_most_recent_recorded_wavfiles(NWavfiles, InfoTerm)", bindings);
		  Term info = answer.getValue("InfoTerm");
		  if ( info != null && info.isCompound() ) {
			  int nFiles = info.getArity();
			  WavfileInfo[] info_array = new WavfileInfo[nFiles];
			  for ( int i = 0; i < nFiles; i++ ) {
				  Term item = info.getArgument(i + 1);
				  if ( item != null && item.isCompound() && item.getArity() == 3 ) {
				  		String timeStamp = ((PBString) item.getArgument(1)).getString();
					  	String fileName = ((PBString) item.getArgument(2)).getString();
				  		String words = ((PBString) item.getArgument(3)).getString();
				  		info_array[i] = new WavfileInfo(timeStamp, fileName, words);
				  		text.append("\nFound " + info_array[i] + "\n");
				  		text.setCaretPosition(text.getDocument().getLength());
				  	    text.requestFocus();
				  }
			  }
			  return info_array;
		  }
		  else {
			  text.append("\nError: unable to get wavfile info\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return null;
		  }
  }
  
  public boolean deleteRegulusFile(String fileString) {
	  try {
		  Bindings bindings = new Bindings();
		  bindings.bind("FileString", fileString);
		  QueryAnswer answer = session.executeQuery("delete_file(FileString, Status)", bindings);
		  Term prologStatus = answer.getValue("Status");
		  if ( prologStatus != null && prologStatus.isString() ) {
			  String status = prologStatus.toString();
			  text.append("\n Delete file " + fileString + ": " + status + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return true;
		  }
		  else {
			  text.append("\nError when trying to delete file " + fileString + "\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return false;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return false;
		  }
  }

    public String doSpeechRecognition() {
	try {
		  QueryAnswer answer = session.executeQuery("do_speech_recognition(RecognisedString)");
		  Term prologString = answer.getValue("RecognisedString");
		  if ( prologString != null ) {
		          //String recognisedString = prologString.toString();
		          String recognisedString = unpackPBString(prologString);
			  text.append("\n Recognition result: \"" + recognisedString + "\"\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return recognisedString;
		  }
		  else {
			  text.append("\nError when trying to perform speech recognition\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return "ERROR: non-string value returned from Prolog";
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return "ERROR: no value returned from Prolog";
		  }
  }

    public String doSpeechRecognitionFromWavfile(String fileString) {
	try {
		  Bindings bindings = new Bindings();
		  bindings.bind("FileString", fileString);
		  QueryAnswer answer = session.executeQuery("do_speech_recognition_from_wavfile(FileString, RecognisedString)", bindings);
		  Term prologString = answer.getValue("RecognisedString");
		  if ( prologString != null ) {
		          //String recognisedString = prologString.toString();
			  String recognisedString = unpackPBString(prologString);
			  text.append("\n Recognition result: \"" + recognisedString + "\"\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return recognisedString;
		  }
		  else {
			  text.append("\nError when trying to perform speech recognition\n");
			  text.setCaretPosition(text.getDocument().getLength());
	  	      text.requestFocus();
			  return null;
		  }
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return "ERROR: no value returned from Prolog";
		  }
  }

  public boolean endRegulusSession() {
	  try {
		  QueryAnswer answer = session.executeQuery("end_regulus_session");
		  text.append("\nSent End Regulus Session message to Prolog server\n");
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return true;
	  } 
	  catch (Exception e) {
		  text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
		  e.printStackTrace();
		  text.setCaretPosition(text.getDocument().getLength());
  	      text.requestFocus();
		  return false;
		  }
  }
    
  public void vetoableChange(PropertyChangeEvent event)
	throws PropertyVetoException {
	 
	JInternalFrame frame1 = (JInternalFrame) event.getSource();
	String name = event.getPropertyName();
	Object value = event.getNewValue();
	
	// we only want to check attempts to close a frame
	
	if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
		int result = JOptionPane.showConfirmDialog(frame1,
				"      OK to close?",
				"Close Confirm Pane",
				JOptionPane.OK_CANCEL_OPTION); 
		// if the user doesn't agree veto the close
		if (result == JOptionPane.CANCEL_OPTION)
			throw new PropertyVetoException("User cancelled close", event);
		else
			endRegulusSession();
			
	}
	
  }
  
	
  public void runRegulusSecond()
	{
	  	String label = JOptionPane.showInputDialog(null,null,"Enter Name for second Window: ",
		 JOptionPane.INFORMATION_MESSAGE);
	  if (label.equals(""))
	  {
		  		int reply = JOptionPane.showConfirmDialog(null, 
				  "Please fill in Name", 					// Prompt displayed in box 
				  "Name Confirm Pane",                 // Label at top of box
				  JOptionPane.OK_CANCEL_OPTION);         // Option type for OK and Cancel buttons
	  }
	  else
	  {
		  new RegulusGUI(label, desktop, DEFAULT_PORT+1,550, 20);  
	  }
	 
	
	}
  public void runRegulusThird()
	{
	  String label = JOptionPane.showInputDialog(null,null,"Enter Name for second Window: ",
				 JOptionPane.INFORMATION_MESSAGE);
			  if (label.equals(""))
			  {
				  		int reply = JOptionPane.showConfirmDialog(null, 
						  "Please fill in Name", 					// Prompt displayed in box 
						  "Name Confirm Pane",                 // Label at top of box
						  JOptionPane.OK_CANCEL_OPTION);         // Option type for OK and Cancel buttons
			  }
			  else
			  {
				  new RegulusGUI(label, desktop, DEFAULT_PORT+2,0, 80);  
			  }
			 
	}
  public void runRegulusFourth()
	{
	  String label = JOptionPane.showInputDialog(null,null,"Enter Name for second Window: ",
				 JOptionPane.INFORMATION_MESSAGE);
			  if (label.equals(""))
			  {
				  		int reply = JOptionPane.showConfirmDialog(null, 
						  "Please fill in Name", 					// Prompt displayed in box 
						  "Name Confirm Pane",                 // Label at top of box
						  JOptionPane.OK_CANCEL_OPTION);         // Option type for OK and Cancel buttons
			  }
			  else
			  {
				  new RegulusGUI(label, desktop, DEFAULT_PORT+3,550, 80);  
			  }
			 
	
	}

  	private static String[][] conversionTable = 
		{
			{"NL", "\n"},
			{"a1", "á"},
			{"a2", "â"},
			{"a3", "à"},
			{"a4", "ä"},
			{"a5", "å"},

			{"c1", "ç"},

			{"e1", "é"},
			{"e2", "ê"},
			{"e3", "è"},
			{"e4", "ë"},
			{"e6", "æ"},

			{"i1", "í"},
			{"i2", "î"},
			{"i3", "ì"},
			{"i4", "ï"},

			{"n1", "ñ"},

			{"o1", "ó"},
			{"o2", "ô"},
			{"o3", "ò"},
			{"o4", "ö"},

			{"u1", "ú"},
			{"u2", "û"},
			{"u3", "ù"},
			{"u4", "ü"},
		};
	
	private static String unpackPBString(Term PBStringTerm) {
		if ( PBStringTerm == null ) {
			return null;
		}
		else if ( !PBStringTerm.isString() ) {
			return PBStringTerm.toString();
		}
		else {
			String s = ((PBString) PBStringTerm).getString();
			if ( s.equals("*empty_string*") ) {
				return "";
			}
			else {
				String s1;
				for ( int i = 0; i < conversionTable.length; i++ ) {
					String fromString = "!" + conversionTable[i][0] + "!";
					String toString = conversionTable[i][1];
					s1 = s.replaceAll(fromString, toString);
					s = s1;
				}
				return s;
			}
		}
	}
  
  
  public static void main(String[] args) {
	  
	  if (args.length == 0)
	  {
		// System.out.println("DEFAULT_PORT "+DEFAULT_PORT); 
	  }
	  else
	  {
		  String strPort = args[0];
		  DEFAULT_PORT = Integer.parseInt(strPort );
		 
	  }
    	  JDesktopPane desktop = new JDesktopPane();
	    	  new RegulusGUI("Debugging Trace", desktop, DEFAULT_PORT, 0, 20);
	    	  // new RegulusGUI("Regulus 2", desktop , 8067, 550, 20);
	      }
	 
}
