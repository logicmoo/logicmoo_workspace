package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.PropertyChangeEvent;


public class Frame4 extends JFrame implements  ActionListener, KeyListener,MouseListener,VetoableChangeListener,InternalFrameListener{

 private RegulusGUI regulusWindow = null;
 private JInternalFrame frame4 = null;
 private JInternalFrame[] allFrames = null;
 private UnAvailableCommandsForFrame4 unavailablecommandsforframe4 = null;
 private AvailableMenusForFrame4  availablemenusforframe4 = null;
 private DialogueProcessingResult dialogueprocessingresult;
 private JMenuBar bar = new JMenuBar();  // create menubar
 private GridBagLayout gblayout;
 private GridBagConstraints gbConstraints;
 private JTextField inputField = new JTextField(75);
 private JButton dialogue = new JButton("Process Dialogue");
 private JButton recognice = new JButton("       Recognise        ");
 private String dialogueValue = "";
 private JPanel inputPanel = new JPanel(new BorderLayout());
 private JPanel barPanel = new JPanel(new BorderLayout());
 private JPanel buttonPanel = new JPanel(new BorderLayout());
 private JPanel displayPanel = new JPanel();
 private String old_state;
 private String source;
 private String lf;
 private String resolved_lf;
 private String resolution;
 private String dialogue_move;
 private String resolved_dialogue_move;
 private String abstract_action;
 private String concrete_action;
 private String new_state;
 private String paraphrase;
 private String DialogueProcessingresult = "";
 private int  index = 0;
 private JCheckBoxMenuItem[] DisplayCheckBoxes = new JCheckBoxMenuItem[11];
 private JMenu viewMenu;
 private final String[] DisplayLabelText = {"Source","Old State","Lf ","Resolved Lf","Resolution", "Dialogue Move",
		"Resolved Dialogue Move", "Abstract Action", "Concrete Action","New State","Paraphrase"};
 private JTextArea[] DisplayTextPanes = new JTextArea[11];
 private JScrollPane[] DisplayScrollPanes = new JScrollPane[11];
 private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
 private JLabel lsource = new JLabel("Source");
 private JLabel  lold_state = new JLabel("Old State");
 private JLabel  lLF = new JLabel("Lf");
 private JLabel  lResolved_lf = new JLabel("Resolved Lf");
 private JLabel  lResolution = new JLabel("Resolution");
 private JLabel  lDialogue_Move = new JLabel("Dialogue Move");
 private JLabel  lResolved_Dialogue_Move = new JLabel("ResolvedDialogue Move");
 private JLabel  lAbstract_Action = new JLabel("AbstractAction");
 private JLabel  lConcrete_Action = new JLabel("Concrete Action");
 private JLabel  lNew_State = new JLabel("New State");
 private JLabel  lParaphrase = new JLabel("Paraphrase");
 private JLabel DisplayLabels[] = {lsource,lold_state,lLF,lResolved_lf,lResolution,lDialogue_Move,lResolved_Dialogue_Move,
		 lAbstract_Action,lConcrete_Action,lNew_State ,lParaphrase};
 private int temp = 0;
 private String  holdString = "";
 private JMenuItem L_loadMenu;
 public  JMenuItem L_loadMenuItem = new JMenuItem( "Load " );
 public  JMenuItem L_EblloadMenuItem = new JMenuItem( "Ebl Load " );
 public  JMenuItem L_loadDialogueMenuItem = new JMenuItem( "Load Dialogue " );
 public  JMenuItem Load_recognition_MenuItem = new JMenuItem( "Load Speech Recognition " );
 public  JMenuItem close_down_recognition_MenuItem = new JMenuItem( "Close down Speech Recognition " );
 public  JMenuItem Nuance_loadMenuItem = new JMenuItem( "Compile Regulus into Nuance" );
 public  JMenuItem  Compile_Nuance_to_recognicer_MenuItem = new JMenuItem( "Compile Nuance to Recogniser " ); 
 public  JMenuItem  Compile_Nuance_to_recognicer_pcfg_MenuItem = new JMenuItem( "Compile Nuance to Recogniser(PCFG)" );
 private JMenuItem Init_DialogueMenuItem;
 private JMenu History_menu = new JMenu( "History" );
 private boolean record_exist_in_table = false;
 private boolean   end_ofTable;
 private int createHistoryIndex = 0;
 private int saveIndex = 0;
 private JMenuItem[] one_step_back_menuItem = new JMenuItem[20];
 private String speechRecognitionString = "";
 private JMenuItem quit_system_MenuItem;
 private int   holdGridx = 0; 
 private JButton From_Interlingua_Trace_button = new JButton("From Interlingua Trace");
 //private JButton To_Interlingua_Trace_button = new JButton("To Interlingua Trace");
 private JButton To_Discource_Trace_button = new JButton("To Source Discourse Trace very very very very very big");	
 private String strName = "";
 private boolean   remove_finished = false;
 private Component c = null;
 private String ac = "";
 private String bc = "";
 private JInternalFrame[] frames = null;
 private int frameIndex = 0;
 private int holdFrameIndex = 0;
 private JButton Dialoguebtn =     new JButton("Dialogue  ");
 private JButton debugbtn =     new JButton("Debugging Trace");
 private int dialoguecounter = 0;
 private JButton[] btnFix = new JButton[10];
 
 // send name of internal frame 
 public JInternalFrame getInternalFrame() {
  return frame4;
  }
// get pointer to Regulus window
  public RegulusGUI getRegulusGUI() {
	  return regulusWindow;
  }
  
  // set the pointer to the Regulus window
  public void setRegulusGUI(RegulusGUI window) {
	  regulusWindow = window;
  }
	
  public Frame4()
  {
	  
  }
  
  public Frame4(RegulusGUI window,JButton translatebtn, int dialogueCounter,JButton DebugBtn) {
	  regulusWindow = window;
	  Dialoguebtn = translatebtn;
	  debugbtn = DebugBtn;
	  dialoguecounter = dialogueCounter;
	  Dialoguebtn.addMouseListener(this);
	  debugbtn.addMouseListener(this);
			
	  setJMenuBar( bar );  // set the menubar for the JInternalFrame		  
			 
	  frame4 = new JInternalFrame("Dialogue Processing",true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	 // bar.setLayout(new FlowLayout(10,10));
	  
	  Container c2 = frame4.getContentPane();
	  
	  regulusWindow.updateCommandStatus();
	  unavailablecommandsforframe4 = new UnAvailableCommandsForFrame4 (this, getRegulusGUI());
	  availablemenusforframe4 = new AvailableMenusForFrame4(this, getRegulusGUI());
	  unavailablecommandsforframe4.check_unavailable_menus(); 
	  
//	create LOAD  menu and sub menu items
	 
	  JMenu loadMenu = new JMenu( "Load" );
	  loadMenu.setMnemonic( 'L');
//	create LOAD  menu item 
	  
	
	
	  L_loadMenuItem.setToolTipText("Load ");
	  L_loadMenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				createBackGroundJob("LOAD");
	  				
	  			}
	  		}
	  		);
	  regulusWindow.updateCommandStatus();
	  unavailablecommandsforframe4.check_unavailable_menus(); 
	  availablemenusforframe4.check_available_menus();
	  loadMenu.add(L_loadMenuItem);
	  
	 
	  
//	create LOAD  menu item Ebl Load
	  
	
	  L_EblloadMenuItem.setToolTipText("Ebl Load ");
	  L_EblloadMenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				createBackGroundJob("EBL_LOAD");
	  			
	  			}
	  		}
	  		);
	  regulusWindow.updateCommandStatus();
	  unavailablecommandsforframe4.check_unavailable_menus(); 
	  availablemenusforframe4.check_available_menus();
	  loadMenu.add(L_EblloadMenuItem);
	  bar.add(loadMenu);
	  
	  
//	create LOAD  menu item Load Dialogue
	  
	 
	  L_loadDialogueMenuItem.setToolTipText("Load Dialogue ");
	  L_loadDialogueMenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("LOAD_DIALOGUE");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Load Dialogue command succeeded";
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
					else
					{
						String command = regulusWindow.getCommandErrorString();
						regulusWindow.InputText = command;
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
	 				 regulusWindow.updateCommandStatus();
	 				 unavailablecommandsforframe4.check_unavailable_menus(); 
	 				 availablemenusforframe4.check_available_menus();
	  			
	  			}
	  		}
	  		);
	  
	  loadMenu.add( L_loadDialogueMenuItem);
	  bar.add(loadMenu);
	  

	  
//	create LOAD  menu item Load Recognition
	  
	 
	  Load_recognition_MenuItem.setToolTipText("Load Speech Recognition ");
	  Load_recognition_MenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				createSpeechBackGroundJob("LOAD_RECOGNITION");
	  			}
	  		}
	  		);
	  regulusWindow.updateCommandStatus();
	  unavailablecommandsforframe4.check_unavailable_menus(); 
	  availablemenusforframe4.check_available_menus();
	  loadMenu.add( Load_recognition_MenuItem);
	  bar.add(loadMenu);
	  
	  
//	create close down recognition  menu item Load Dialogue
	  
	 
	  close_down_recognition_MenuItem.setToolTipText(" Close down speech recognition");
	  close_down_recognition_MenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("CLOSE_DOWN_RECOGNITION");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Close down recognition command succeeded";
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
					else
					{
						String command = regulusWindow.getCommandErrorString();
						regulusWindow.InputText = command;
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
	 				regulusWindow.updateCommandStatus();
	 				unavailablecommandsforframe4.check_unavailable_menus(); 
	 				availablemenusforframe4.check_available_menus();
	  			}
	  		}
	  		);
	  
	  loadMenu.add( close_down_recognition_MenuItem);
	  bar.add(loadMenu);
	  
//	create Compile Nuance sub menu item to LOAD
	  
	 
	  Nuance_loadMenuItem.setToolTipText("Compile current Regulus grammar into Nuance GSL form");
	  Nuance_loadMenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("NUANCE");
	  				if (regulusWindow.regulus_command_succeeded)
	  				{ 
	  					regulusWindow.InputText = "Nuance command succedeed";
	  					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	  				}
	  				else
	  				{
	  					String command = regulusWindow.getCommandErrorString();	
	  					regulusWindow.InputText = command;
	  					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	  				}
	  				regulusWindow.updateCommandStatus();
	 				unavailablecommandsforframe4.check_unavailable_menus(); 
	 				availablemenusforframe4.check_available_menus();
	  			}
	  		}
	  		);
	  loadMenu.add(Nuance_loadMenuItem);
	  bar.add(loadMenu);
	  
//		create compile Nuance to Recognicer menu item Load Dialogue
	  
	
	  Compile_Nuance_to_recognicer_MenuItem.setToolTipText("Compile Nuance to Recognicer ");
	  Compile_Nuance_to_recognicer_MenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("NUANCE_COMPILE");
	 				if (regulusWindow.regulus_command_succeeded)
	 				{ 
	 					regulusWindow.InputText = "Compile Nuance to Recognicer command succeeded";
	 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	 				}
	 				else
	 				{
	 					String command = regulusWindow.getCommandErrorString();
	 					regulusWindow.InputText = command;
	 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	 				}
	 				regulusWindow.updateCommandStatus();
	 				unavailablecommandsforframe4.check_unavailable_menus(); 
	 				availablemenusforframe4.check_available_menus();
	  			}
	  		}
	  		);
	  
	  loadMenu.add( Compile_Nuance_to_recognicer_MenuItem);
	  bar.add(loadMenu);
	  
//	 	create compile Nuance to Recognicer (PCFG) menu item Load Dialogue
	  
	 
	  Compile_Nuance_to_recognicer_pcfg_MenuItem.setToolTipText("Compile Nuance to Recognicer (PCFG)");
	  Compile_Nuance_to_recognicer_pcfg_MenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("NUANCE_COMPILE_WITH_PCFG");
	 				if (regulusWindow.regulus_command_succeeded)
	 				{ 
	 					regulusWindow.InputText = "Compile Nuance to Recognicer (PCFG) command succeeded";
	 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	 				}
	 				else
	 				{
	 					String command = regulusWindow.getCommandErrorString();
	 					regulusWindow.InputText = command;
	 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
	 				}
	 				regulusWindow.updateCommandStatus();
	 				unavailablecommandsforframe4.check_unavailable_menus(); 
	 				availablemenusforframe4.check_available_menus();
	  			}
	  		}
	  		);
	  
	  loadMenu.add( Compile_Nuance_to_recognicer_pcfg_MenuItem);
	  bar.add(loadMenu);
	  
	  
//	create quit systen menu item
	  
	  quit_system_MenuItem = new JMenuItem( "Exit " );
	  quit_system_MenuItem.setToolTipText(" Exit system");
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
	 // bar.add(loadMenu,gbConstraints);
	  
	 
	  viewMenu = new JMenu("View");
	  viewMenu.setMnemonic('V');
	  bar.add(viewMenu);
	  
//		create Dialogue  menu and sub menu items
	  
	  JMenu dialogueMenu = new JMenu( "Dialogue" );
	  dialogueMenu.setMnemonic( 'D');
//	create Dialogue  menu item InitDialogue
	  
	  Init_DialogueMenuItem = new JMenuItem( "Init Dialogue " );
	  Init_DialogueMenuItem.setToolTipText("Init Dialogue ");
	  Init_DialogueMenuItem.addActionListener(
	  		new ActionListener() {
	  			public void actionPerformed( ActionEvent e)
	  			{
	  				regulusWindow.handleCommand("INIT_DIALOGUE");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Init Dialogue command succeeded";
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
					else
					{
						String command = regulusWindow.getCommandErrorString();
						regulusWindow.InputText = command;
						regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					}
	 				regulusWindow.availablemenus.check_available_menus();
	 				regulusWindow.unavailablecommands.check_unavailable_menus();	  			
	  			}
	  		}
	  		);
	  
	  dialogueMenu.add(Init_DialogueMenuItem);
	  bar.add(dialogueMenu);
	  
	  
	  History_menu = new JMenu("History");
	  History_menu.setToolTipText("get backlog of sentences");
	  History_menu.setEnabled(false);
	  bar.add(History_menu);
 
	 
  
//	 Check if Translate button is pressed down
	  
	  dialogue.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{
		  		dialogueValue  = inputField.getText();
		  		createHistorySentences();
		  		inputField.setText("");
		  		GetDialogueProcessingItems();
		  		WriteDialogueProcessingItems();
		  		}
		  	}
		  );
	  
//	 Check if Translate button is pressed down
	  
	  recognice.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{
		  		  switchOnSpeechRecognicion();
		  		  dialogueValue  = inputField.getText();
		  		  System.out.println("dialogueValue "+dialogueValue);
		  		  createHistorySentences();
		  		  inputField.setText("");
		  		  GetDialogueProcessingItems();
		  		  WriteDialogueProcessingItems();
		  		}
		  	}
		  );
	  

	  
	  // set up the layout
	  gblayout = new GridBagLayout();
	  displayPanel.setLayout(gblayout);
	  // instansiate the gridbag constraints
	  gbConstraints = new GridBagConstraints();
	  

//	Add labels and textboxes
	  
	 // createTestArea();
	  
	 // setCheckBoxes();
	  
//	 add listener to confirm closing and listener to keyboard
	  frame4.addVetoableChangeListener(this);
	  inputField.addKeyListener(this);
	  frame4.addInternalFrameListener(this);
	  
	  c2.add(displayPanel,BorderLayout.CENTER);
	  
	  JPanel inputPanel = new JPanel(new BorderLayout());
	  JPanel barPanel = new JPanel(new BorderLayout());
	  JPanel buttonPanel = new JPanel(new BorderLayout());
	  
	  createTestArea();
	  
	  setCheckBoxes();
	  
	  inputPanel = new JPanel(new BorderLayout());
	  
	  barPanel.add(bar,BorderLayout.NORTH);
	  barPanel.add(recognice,BorderLayout.EAST);
	  
	  buttonPanel.add(inputField,BorderLayout.CENTER);
	  buttonPanel.add(dialogue,BorderLayout.EAST);
	 
	  inputPanel.add(barPanel,BorderLayout.NORTH);
	  inputPanel.add(buttonPanel,BorderLayout.CENTER);
	  
	  c2.add(inputPanel,BorderLayout.NORTH);
	 
	  frame4.setDefaultCloseOperation(
					  WindowConstants.DISPOSE_ON_CLOSE);
	  frame4.pack();
  }
  public void CreateAndLinkdialogue()
  {
	  
	    DialoguePane dialoguepane = new DialoguePane(this,getRegulusGUI(),holdString );
	  	dialoguepane.setRegulusGUI(getRegulusGUI());
	  	
	  	
		 //  display new internal window
	    JInternalFrame dialoguePaneInternalFrame = dialoguepane.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(dialoguePaneInternalFrame);
		dialoguePaneInternalFrame.setVisible(true); 
  }
  
  public void createSpeechBackGroundJob(String strCommand)
  { 
  	String CommandIdent = strCommand;
  	SpeechCommandInBackGround speechcommandinbackground = new SpeechCommandInBackGround(regulusWindow,CommandIdent);
  	progressLoadRecognition  progressload = new progressLoadRecognition(regulusWindow,CommandIdent,speechcommandinbackground );
  	
  	speechcommandinbackground.start();
  	progressload.start();
  }
  public void actionPerformed(ActionEvent e)
  {
	  	
  } 
  
  public void switchOnSpeechRecognicion() {
		 
		 speechRecognitionString  =  regulusWindow. doSpeechRecognition();
		 String b = "ERROR";
		 
		 // check it there is an error or the string is okay
		
		 //if (speechRecognitionString.indexOf(b) != -1) {
		if	 (speechRecognitionString.startsWith(b)) {
			 doSpeechErrorHandling(); 
		 }
		 else
		 {
			 inputField.setText(speechRecognitionString);
		 }
	 }
  
  public void doSpeechErrorHandling() {
		 JOptionPane.showMessageDialog(null, speechRecognitionString 
					,"Warning for SpeechRecognition",JOptionPane.INFORMATION_MESSAGE);
	 }
	 public void switchOffSpeechRecognicion() {
		 regulusWindow.handleCommand("CLOSE_DOWN_RECOGNITION");
			if (regulusWindow.regulus_command_succeeded)
			{ 
				regulusWindow.InputText = "close down recognition command succeeded";
				regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
			}
			else
			{
				String command = regulusWindow.getCommandErrorString();
				regulusWindow.InputText = command;
				regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
			} 
	 }
  public void mousePressed(MouseEvent e)
  {
	  // System.out.println("mousepressed");
	  JButton button = (JButton)e.getSource();
	  String cc = button.getActionCommand();
	  //System.out.println("cc "+cc);
	  if (cc.startsWith("Debugging"))
	  {
		  bc = debugbtn.getActionCommand();
		  getAllFramesDebug();
	  }
	  else if (cc.startsWith("Dialogue"))
	  {
		  ac = Dialoguebtn.getActionCommand();
		  //System.out.println("ac "+ac);
		  getAllFrames();
	  }
  }
  public void mouseClicked(MouseEvent e)
  {
  }
  public void mouseReleased(MouseEvent e)
  {
  }
  public void mouseEntered(MouseEvent e)
  {
  }
  public void mouseExited(MouseEvent e)
  {
  }
  public void keyPressed( KeyEvent e)
  {
	  
	  if (e.getKeyCode() == e.VK_ENTER)
	  {
		 System.out.println("Keypressed");
		 dialogueValue  = inputField.getText();
		 createHistorySentences();
	  	 inputField.setText("");
	  	 GetDialogueProcessingItems();
	  	 WriteDialogueProcessingItems();
	
	  }
		
  }
  public void keyReleased( KeyEvent e)
  {
	  // TODO method stub 
	  System.out.println("key released"+e.getKeyCode());
  }
  public void keyTyped( KeyEvent e)
  {
	  // TODO method stub 
	  System.out.println("key typed"+e.getKeyCode());
  }
  public void createBackGroundJob(String strCommand)
  { 
  	String CommandIdent = strCommand;
  	new CommandInBackGround(regulusWindow,CommandIdent).start();
  	
  	new ReadOneCommandFile(regulusWindow,CommandIdent).start();
  }
  public void createHistorySentences(){
	  compare();
  // if the record does not exist in historytable then add it
	  // if the record does not exist in historytable then add it
	  System.out.println("record_exist_in_table "+record_exist_in_table  );
	  if (record_exist_in_table == false)
	 	{
	  	end_ofTable = false;
	   	ReadTableSaveItem();
	   	callCreateHistoryMenu();
	  	}
	  inputField.setText("");
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
	  if (regulusWindow.SentenceTable[i]!= null && regulusWindow.SentenceTable[i].equals(dialogueValue ))
		  {
			  record_exist_in_table = true;
		  }
  }
  public void checkHowManyItems()
  {
	  for (int i = 1;regulusWindow.SentenceTable[i]!= null ; i++ )
	  {
		  System.out.println("regulusWindow.SentenceTable[i]"+regulusWindow.SentenceTable[i]);
	  }
  }
  public void ReadTableSaveItem()
	 {
		 for (int i = 1;end_ofTable == false ; i++ )
		  {
			 if(regulusWindow.SentenceTable[i] == null)
			 {
			 regulusWindow.SentenceTable[i] = dialogueValue ;
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
	  regulusWindow.SentenceTable[saveIndex] = dialogueValue ;
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
	 one_step_back_menuItem[createHistoryIndex] = new JMenuItem(dialogueValue);
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
		 					dialogueValue = inputField.getText();
		 				  
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
  public void GetDialogueProcessingItems()  {
	 dialogueprocessingresult = regulusWindow.performDialogueProcessing(dialogueValue);
	 source = dialogueprocessingresult.getSource();
	 old_state  = dialogueprocessingresult.getOldState();
	 lf = dialogueprocessingresult.getLF();
	 resolved_lf = dialogueprocessingresult.getResolvedLF();
	 resolution = dialogueprocessingresult.getResolution();
	 dialogue_move = dialogueprocessingresult.getDialogueMove();
	 resolved_dialogue_move = dialogueprocessingresult.getResolvedDialogueMove();
	 abstract_action = dialogueprocessingresult.getAbstractAction();
	 concrete_action = dialogueprocessingresult.getConcreteAction();
	 new_state = dialogueprocessingresult.getNewState();
	 paraphrase = dialogueprocessingresult.getParaphrase();
  }
  public void WriteDialogueProcessingItems()
  {
	   DisplayTextPanes[0].setText((source));
	   DisplayTextPanes[0].setCaretPosition(0);
	   DisplayTextPanes[1].setText((old_state));
	   DisplayTextPanes[1].setCaretPosition(0);
	   DisplayTextPanes[2].setText((lf));
	   DisplayTextPanes[2].setCaretPosition(0);
	   DisplayTextPanes[3].setText((resolved_lf));
	   DisplayTextPanes[3].setCaretPosition(0);
	   DisplayTextPanes[4].setText((resolution));
	   DisplayTextPanes[4].setCaretPosition(0);
	   DisplayTextPanes[5].setText((dialogue_move));
	   DisplayTextPanes[5].setCaretPosition(0);
	   DisplayTextPanes[6].setText((resolved_dialogue_move));
	   DisplayTextPanes[6].setCaretPosition(0);
	   DisplayTextPanes[7].setText((abstract_action));
	   DisplayTextPanes[7].setCaretPosition(0);
	   DisplayTextPanes[8].setText((concrete_action));
	   DisplayTextPanes[8].setCaretPosition(0);
	   DisplayTextPanes[9].setText((new_state));
	   DisplayTextPanes[9].setCaretPosition(0);
	   DisplayTextPanes[10].setText((paraphrase));
	   DisplayTextPanes[10].setCaretPosition(0);
  }
  
  public void createTestArea()
  {
	 
	  for (index = 0; index < 11; index++ ) 
	  {
		 
		  createTest();
		  createCheckBoxes();
	  }
  }

  public void createCheckBoxes()
	  {
	  DisplayCheckBoxes[index] = new JCheckBoxMenuItem(DisplayLabelText[index],true);
	  DisplayCheckBoxes[index].addItemListener(new AnswerCheckBoxHandler());
	  viewMenu.add(DisplayCheckBoxes[index]);
	  }
  public void setCheckBoxes()
  {
	  DisplayCheckBoxes[0].setSelected(true);
      DisplayCheckBoxes[1].setSelected(true); 
	  DisplayCheckBoxes[2].setSelected(true); 
	  DisplayCheckBoxes[3].setSelected(true);
	  DisplayCheckBoxes[4].setSelected(true); 
	  DisplayCheckBoxes[5].setSelected(true);
	  DisplayCheckBoxes[6].setSelected(true);
	  DisplayCheckBoxes[7].setSelected(true);
	  DisplayCheckBoxes[8].setSelected(true);
	  DisplayCheckBoxes[9].setSelected(true);
	  DisplayCheckBoxes[10].setSelected(true);
	
  
 }
 

  
  public void createTest()
  {
	  // add label
	  temp = 2 + index;
	  //gbConstraints.fill = GridBagConstraints.VERTICAL;
	  gbConstraints.fill = GridBagConstraints.VERTICAL;
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = temp;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  gbConstraints.weightx = 0;
	  gbConstraints.weighty = 0.1;
	  gbConstraints.insets = new Insets(8,8,8,8);
	  displayPanel.add(DisplayLabels[index] ,gbConstraints);
	  
	  
	  // add textbox
	
	  DisplayTextPanes[index] = new JTextArea(2,80);
	  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
	  DisplayTextPanes[index].setEditable(true);
	  DisplayScrollPanes[index] = new JScrollPane(DisplayTextPanes[index]);
	  gbConstraints.fill = GridBagConstraints.BOTH;
	  gbConstraints.gridx = 1;
	  gbConstraints.gridy = temp;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 10;
	  gbConstraints.weightx = 10;
	  gbConstraints.weighty = 0.1;
	  gbConstraints.insets = new Insets(1,8,1,8);
	  displayPanel.add(DisplayScrollPanes[index],gbConstraints); 
	  checkListener();
	 
  }
 
  public void checkListener(){
	  if (index == 0)
	  {
		  setMouseListenerSource(); 
	  }
	  else if (index == 1)
	  {
		  setMouseListenerOldState(); 
	  }
	  else if (index == 2)
	  {
		  setMouseListenerLf(); 
	  }
	  else if (index == 3)
	  {
		  setMouseListenerResolvedLf(); 
	  }
	  else if (index == 4)
	  {
		  setMouseListenerResolution(); 
	  }
	  else if (index == 5)
	  {
		  setMouseListenerDialogueMove(); 
	  }
	  else if (index == 6)
	  {
		  setMouseListenerResolvedDialogueMove(); 
	  }
	  else if (index == 7)
	  {
		  setMouseListenerAbstractAction(); 
	  }
	  else if (index == 8)
	  {
		  setMouseListenerConcreteAction(); 
	  }
	  else if (index == 9)
	  {
		  setMouseListenerNewState(); 
	  }
	  else if (index == 10)
	  {
		  setMouseListenerParaphrase(); 
	  }
  }
  public void setMouseListenerSource()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerSource()); 
  }
  public void setMouseListenerOldState()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerOldState()); 
  }
  public void setMouseListenerLf()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerLf()); 
  }
  public void setMouseListenerResolvedLf()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerResolvedLf()); 
  }
  public void setMouseListenerResolution()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerResolution()); 
  }
  public void setMouseListenerDialogueMove()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerDialogueMove());
  }
  public void setMouseListenerResolvedDialogueMove()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerResolvedDialogueMove());
  }
  public void setMouseListenerAbstractAction()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerAbstractAction());
  }
  public void setMouseListenerConcreteAction()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerConcreteAction());
  }
  public void setMouseListenerNewState()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerNewState());
  }
  public void setMouseListenerParaphrase()
  {
	  DisplayTextPanes[index].addMouseListener(new MouseHandlerParaphrase());
  }
   public void handleDisplay()
  {
	  CreateAndLinkdialogue();
  }
   
	  public void closeFrame(int count){
		  // Iterate ower the frames, closing them if they are open
		 		count = count - 1;
			  for (int j = 0; j < count; j++ ){
			  JInternalFrame f = allFrames[j];
			  if((f.isClosed()== false) || (f.isIcon()== true))
			  try{
			  f.setIcon(true);
			  f.dispose();
			  }
			  catch (PropertyVetoException ex) {}
			  }
			}
				
			  public void vetoableChange(PropertyChangeEvent event)
				throws PropertyVetoException {
				 
				JInternalFrame frame4 = (JInternalFrame) event.getSource();
				String name = event.getPropertyName();
				Object value = event.getNewValue();
				int count = 0;
				
				// we only want to check attempts to close a frame
				
				if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
					int result = JOptionPane.showConfirmDialog(frame4,
							"         OK to close?",
							"Close Confirm Pane",
							JOptionPane.OK_CANCEL_OPTION); 
							
					// if the user doesn't agree veto the close
					if (result == JOptionPane.CANCEL_OPTION)
						throw new PropertyVetoException("User cancelled close", event);
					else
					//count = aPerformed();
					//System.out.println("count "+count);
					// If in bidirectional mode, essential to call BIDIRECTIONAL_OFF to kill the
					// remote server and free up the port.
					regulusWindow.handleCommand("BIDIRECTIONAL_OFF");
					setVisible(false);
					dispose();
					return;
				}
				
			}
			  
		
			  
			  public void lookForIcon() {
					for (int i = 0; remove_finished == false ; i ++)  {
						 c = regulusWindow. barPanel.getComponent(i);
						 String strname = c.toString();
						 //String strName = c.getName();
						 String b = strName;
						 b = "Dialogue ";
						 if (strname.indexOf(b) != -1) {
							 //regulusWindow.barPanel.remove(Dialoguebtn);
							 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));
							 regulusWindow.barPanel.repaint();
							// regulusWindow.deleteButton();
							 remove_finished = true;
						 }
						 else
						 {
							
							 
						 }
					 }
				}
			  
			  public void internalFrameClosing(InternalFrameEvent e) {
				  regulusWindow.translationCounter--;
				   remove_finished = false;
				   lookForIcon();
					
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
				    	Dialoguebtn =     new JButton(strName);
				    	regulusWindow.barPanel.add(Dialoguebtn);
				    	Dialoguebtn.addMouseListener(this);
				    	getselectedFrame();
				    	
				    }

				    public void internalFrameActivated(InternalFrameEvent e) {
				    	
				    }

				    public void internalFrameDeactivated(InternalFrameEvent e) {
					
				    }
				    
				    public void getAllFrames(){
				    	 frames = regulusWindow.desktop.getAllFrames();
				    	 frameIndex = 0;
				         int countFrames = frames.length;
				         for (int i = 0; i < countFrames; i++) {
				          String strFrames = frames[i].toString();
			 	         holdFrameIndex = i;
				         //String b = ac;
				         String b = "Dialogue";
				        // System.out.println("b "+b);
						 if (strFrames.indexOf(b) != -1) {
			  				try {
			  					//System.out.println("strFrames "+strFrames);
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
				         String b = "Dialogue";
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
			 	         String b = "Dialogue";
			  			 if (strFrames.indexOf(b) != -1) {
			  				 
			    				if (frames[i].isIcon())
			    				{
			    					strName = frames[i].getTitle();
			     				}
			    			 
			  			}
			 	      }
			 	  }	    	
				   
				        
			  
			
  class AnswerCheckBoxHandler implements ItemListener
  {
	  public void itemStateChanged(ItemEvent event)
	  {
		  for (index = 0; index < 11; index++ ) {
			  if (event.getSource() == DisplayCheckBoxes[index]){
				  DisplayLabels[index].setVisible(DisplayCheckBoxes[index].isSelected());
				  DisplayScrollPanes[index].setVisible(DisplayCheckBoxes[index].isSelected());
			  }
		  }
			 
	  }
}
  class MouseHandlerSource extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  source; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerOldState extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  old_state; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerLf extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  lf; 
			  handleDisplay();
			
		  }
	  } 
  }
  class MouseHandlerResolvedLf extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  resolved_lf; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerResolution extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  resolution; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerDialogueMove extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  dialogue_move; 
			  handleDisplay();
			
		  }
	  } 
  }
  class MouseHandlerResolvedDialogueMove extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  resolved_dialogue_move; 
			  handleDisplay();
			
		  }
	  } 
  }
  class MouseHandlerAbstractAction extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  abstract_action; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerConcreteAction extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  concrete_action; 
			  handleDisplay();
			
		  }
	  } 
  }
  
  class MouseHandlerNewState extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  new_state; 
			  handleDisplay();
			
		  }
	  } 
  }
  class MouseHandlerParaphrase extends MouseAdapter{
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			//  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			//  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  holdString =  paraphrase; 
			  handleDisplay();
			
		  }
	  } 
  }
}
