package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;
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
import java.awt.event.MouseListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.ButtonGroup;
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
import javax.swing.WindowConstants;
import RegulusGUI.Frame3.AnswerCheckBoxHandler;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


public class CreateFrame3 extends JFrame
						implements  ActionListener,VetoableChangeListener,KeyListener,MouseListener{
	
	
//	 pointer to the reguluswindow which has created stepper window 
	 private RegulusGUI regulusWindow = null;
	 private JInternalFrame createframe3 = null;
	 private Frame3     frame3       = null;
	 public JInternalFrame[] allFrames = null;
	 private GridBagLayout gblayout;
	 private GridBagConstraints gbConstraints;
	 public JTextField inputField = new JTextField(75);
	 public JButton translate = new JButton("Translate");
	 private JButton From_Interlingua_Trace_button = new JButton("From Interlingua Trace");
	 private JButton To_Interlingua_Trace_button = new JButton("To Interlingua Trace");
	 private JButton To_Discource_Trace_button = new JButton("To Source Discourse Trace");
	 private JRadioButton remote, notRemote;
	 private ButtonGroup radioGroup;
	 private JMenuItem L_loadMenu;
	 private JMenuItem L_loadMenuItem;
	 private JMenuItem L_EblloadMenuItem;
	 private JMenuItem answer_ellipsis_off_menu;
	 private JMenuItem answer_ellipsis_on_menu;
	 private JMenu FlagMenu = new JMenu( "Set Flags" );
	 private JMenu Translate_menu;
	 private JMenuItem translate_trace_off_menu;
	 private JMenuItem translate_trace_on_menu;
	 public  JMenu CorpusMenu = new JMenu( "Corpus" );
	 public  JMenu JudgeMenus = new JMenu( "Judge" );
	 private JMenu History_menu = new JMenu( "History" );
	 private JMenu BiDirectional_menu = new JMenu( "Bidirectional mode" );
	 private JMenuItem BiDirectional_on_menuitem;
	 private JMenuItem BiDirectional_off_menuitem;
	 private JMenuItem Translate_Corpus_MenuItem;
	 private JMenuItem Translate_Corpus_MenuItem_arg;
	 private JMenuItem Translate_speech_Corpus_MenuItem; 
	 private JMenuItem Translate_speech_Corpus_Arg_MenuItem; 
	 private JMenuItem Translate_speech_Corpus_Again_MenuItem; 
	 private JMenuItem Translate_speech_Corpus_Again_Arg_MenuItem;
	 private String translateval = "";
	 private TranslationResult translationresult;
	 private String source = "";
	 private String target;
	 private String n_parses;
	 private String parse_time;
	 private String source_representation;
	 private String source_discourse;
	 private String resolved_source_discourse;
	 private String resolution_processing;
	 private String interlingua;
	 private String target_representation;
	 private String n_generations;
	 private String generation_time;
	 private String other_translations;
	 private String tagged_translations;
	 private String To_Source_Discourse_Trace;
	 private String To_Interlingua_Trace;
	 private String gloss_translation;
	 private String interlingua_surface;
	 private String original_script_translation;
	 private String From_Interlingua_Trace;
	 private JLabel lsource = new JLabel("Source");
	 private JLabel ltarget = new JLabel("Target");
	 private JLabel ln_parses = new JLabel("Parses");
	 private JLabel lparse_time = new JLabel("Parse Time");
	 private JLabel lsource_representation = new JLabel("Source Rep");
	 private JLabel lsource_discourse = new JLabel("Source Discourse");
	 private JLabel lresolved_source_discourse = new JLabel("Resolved Source Discourse");
	 private JLabel lresolution_processing = new JLabel("Resolution  processing");
	 private JLabel linterlingua = new JLabel("Interlingua");
	 private JLabel ltarget_representation = new JLabel("Target Representation");
	 private JLabel ln_generations = new JLabel("Number Generations");
	 private JLabel lgeneration_time = new JLabel("Generation Time");
	 private JLabel lother_translations = new JLabel("Other Translations");
	 private JLabel ltagged_translations = new JLabel("Tagged Translations;");
	 private JLabel lto_sourcediscource_trace = new JLabel("To Source Discourse Trace");
	 private JLabel lto_Interlingua_trace = new JLabel("To Interlingua Trace");
	 private JLabel lfrom_Interlingua_trace = new JLabel("From Interlingua Trace");
	 private JLabel lgloss_translation = new JLabel("Gloss Translation");
	 private JLabel linterlingua_surface = new JLabel("Interlingua Surface");
	 private JLabel loriginal_script_translation = new JLabel("Original Script Translation");
	 private Container c2; 
	 private JLabel DisplayLabels[] = {lsource,ltarget,lgloss_translation,loriginal_script_translation,ln_parses,lparse_time,lsource_representation,lto_sourcediscource_trace,
			 lsource_discourse,lresolved_source_discourse,lresolution_processing,lto_Interlingua_trace,linterlingua,linterlingua_surface, 
			 lfrom_Interlingua_trace,ltarget_representation,ln_generations, lgeneration_time,
			 lother_translations,ltagged_translations,};
	 private JTextArea[] DisplayTextPanes = new JTextArea[20];
	 private JTextArea traceTest = new JTextArea(2,2);
	 private JScrollPane[] DisplayScrollPanes = new JScrollPane[20];
	 private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
	 private JMenuItem[] one_step_back_menuItem = new JMenuItem[20];
	 private int index;
	 private int temp;
	 private JMenu viewMenu;
	 private JCheckBoxMenuItem[] DisplayCheckBoxes = new JCheckBoxMenuItem[20];
	 private final String[] DisplayLabelText = {"Source","Target","Gloss Translation","Original Script Translation",
			 "Parses","Parse Time","Source Rep", "To Source Discourse Trace", "Source Discourse","Resolved Source Discourse",
			 "Resolution  processing","To Interlingua Trace","Interlingua","Interlingua Surface","From Interlingua Trace",
			 "Target Representation","Number Generations","Generation Time","Other Translations",
			 "Tagged Translations"};
	 private JMenuBar bar = new JMenuBar();  // create menubar
	 public JPanel displayPanel = new JPanel();
	 private JPanel inputPanel = new JPanel(new BorderLayout());
	 private int Source_rep_h = 6;
	 private int From_Interlingua_Trace_h = 14;
	 private final int Interlingua_h = 12; 
	 private final int To_Interlingua_Trace_h = 11; 
	 private final int trace2 = 11;
	 private final int trace3 = 14;
	 private final int smallTextArea1 = 0;
	 private final int smallTextArea2 = 1;
	 private final int smallTextArea3 = 2;
	 private final int smallTextArea4 = 3;
	 private final int smallTextArea5 = 4;
	 private final int smallTextArea6 = 13;
	 private final int smallTextArea7 = 16;
	 private final int smallTextArea8 = 17;
	 private final int smallTextArea9 = 18;
	 private final int smallTextArea10 = 19;
	 private final int smallTextArea11 = 20;
	 private String[] SentenceTable = new String[20];
	 private boolean record_exist_in_table = false;
	 private int saveIndex = 0;
	 public  JMenuItem[] corpus_menuitem = new JMenuItem[9];
	 public  JMenuItem[] judge_menuitem = new JMenuItem[9];

	 public  String[] corpus_items = new String[10];
	 public  String[] judge_items =  new String[10];
	 public  int corpusIndex = 0;
	 public  int judgeIndex = 0;
	 public  String holdSentence = "";
	 public  int len = 0;
	 public  char[] tempCharArray;
	 public  String[] myLetterArray = new String[36];
	 public  boolean Command_Could_Hold_Argument = false;
	 public  char tempChar;
	 public  boolean argument_finnished = false;
	 public  boolean Command_Holds_Argument = false;
	 public  boolean Translate_Corpus_arg_Command_exists = false;
	 public  int charIndex = 0;
	 public  int result = 0;
	 public  String CorpusMenuString = "";
	 public  String JudgeMenuString = "";
	 public  CreateCorpusMeny createcorpusmeny = null; 
	 public  JudgeMenu judgemenu = null;
	 private  boolean remote_button = false;
	 private  boolean bidirectional_mode_on = false;
	 private  boolean file_written = false;
	 public String CorpusString;
	 public String corpusId2;
	 private int startPos = 0;
	 private int endPos = 0;
	 private String Trace = "";
	 private String[] TraceSentenceTable = new String[50];
	 private String[] TraceRuleTable = new String[50];
	 private int sentenceindex = 0;
	 private int savestartPos = 0;
	 private int saveendPos = 0;
	 private int length = 0;
	 private int firstDigit = 0;
	 private int indexCount = 0;
	 private boolean found_character = false;
	 private int numberIndex = 0;
	 private int RuleIndexStart = 0;
	 private int RuleIndexEnd = 0;
	 private JList summaryList ; 
	 private DefaultListModel listModel;
	 private String holdTextArea = "";
	 private int saveSentenceIndex = 0;
	 private String TraceFileName = "";
	 private boolean discource_trace = false;
	 private String WhichTraceFile = "";
	 public  String[] Source_Rep_items =  new String[8];
	 private int Source_Rep_length = 0;
	 private int lines_used = 0;
	 private int lines_pooled = 0;
	 private  int holdLength = 0;
	 private int lines_left = 0;
	 private int lines_extra = 0;
	 private String whichProgram = "";
	 private String FromProg = "";
	 private String FromProgram = "";
	 private String translatevalue = "";
	 private int  Source_Rep_Hight = 0;
	 private int From_Interlingua_Trace_Hight = 0;
	 private int To_Interlingua_Trace_Hight = 0;
	 private int Interlingua_Hight = 0;
	 private int traceCounter = 0;
	 private int endOfSentenceCounter = 0;
	 
	 
	  // send name of internal frame 
		 public JInternalFrame getInternalFrame() {
		  return createframe3;
		  }
	  // get pointer to Regulus window
		  public RegulusGUI getRegulusGUI() {
			  return regulusWindow;
		  }
		  
		  // set the pointer to the Regulus window
		  public void setRegulusGUI(RegulusGUI window) {
			  regulusWindow = window;
		  }
//			 set the pointer to the Frame3 window
		  public void setFrame3(Frame3 window) {
			  frame3 = window;
		  }
		  public  CreateFrame3()
		  {
			  
		  }
		  
	  public  CreateFrame3(RegulusGUI window,String translateval,int Source_num, int From_Inter_Trace, int Interlingua_num, int To_Inter_Trace) {
		  regulusWindow = window;
		  translatevalue = translateval;
		  Source_Rep_Hight = Source_num;
		  From_Interlingua_Trace_Hight = From_Inter_Trace;
		  Interlingua_Hight = Interlingua_num;
		  To_Interlingua_Trace_Hight = To_Inter_Trace;
		  createcorpusmeny = new CreateCorpusMeny(frame3,this, window,whichProgram);
		  whichProgram = "CreateFrame3";
		  
		  judgemenu = new JudgeMenu(frame3, this, window);
			
	   setJMenuBar( bar );  // set the menubar for the JInternalFrame		  
				 
	 createframe3 = new JInternalFrame("Translator",true,true,true,true);

	 c2 = createframe3.getContentPane();
	 
	 
	  
//	create LOAD  menu item and sub menu items
	 JMenu loadMenu = new JMenu( "Load" );
	 loadMenu.setMnemonic( 'L');
	 L_loadMenu = new JMenuItem( "Load Translate" );
	 L_loadMenu.setToolTipText("Load translation related files");
	 L_loadMenu.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("LOAD_TRANSLATE");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Load Translate command succeeded";
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
	 
	 loadMenu.add(L_loadMenu);
	 bar.add(loadMenu);
	 
	 L_loadMenuItem = new JMenuItem( "Load " );
	 L_loadMenuItem.setToolTipText("Load ");
	 L_loadMenuItem.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("LOAD");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Load command succeeded";
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
	 
	 loadMenu.add(L_loadMenuItem);
	 bar.add(loadMenu);
	 
//	create LOAD  menu item 
	 
	 L_EblloadMenuItem = new JMenuItem( "Ebl Load " );
	 L_EblloadMenuItem.setToolTipText("Ebl Load ");
	 L_EblloadMenuItem.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("EBL_LOAD");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Ebl Load command succeeded";
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
	 
	 loadMenu.add(L_EblloadMenuItem);
	 bar.add(loadMenu);
	 
//	create Set Flags  menu item
	 
	  FlagMenu.setMnemonic( 'F');
	 //create Mode sub menu item to Set Flags
	 FlagMenu.addSeparator();
	 JMenu modeMenu  = new JMenu( "Mode" );
//	create Ellipsis sub menu to Set Ellipsis off
	 
	 modeMenu.addSeparator();
	 JMenu Answer_Ellipsis_Menu  = new JMenu( "Answer Elipsis" );
	 answer_ellipsis_off_menu = new JMenuItem("Answer Ellipsis off");
	 answer_ellipsis_off_menu.setToolTipText("Switch off answer ellipsis (default)");
	 answer_ellipsis_off_menu.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("ANSWER_ELLIPSIS_OFF");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Answer Ellipsis off command succeeded";
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

	 modeMenu.add(Answer_Ellipsis_Menu);
	 Answer_Ellipsis_Menu.add(answer_ellipsis_off_menu);
	 
//	create Ellipsis sub menu to Set Ellipsis on

	 answer_ellipsis_on_menu = new JMenuItem("Answer Ellipsis on");
	 answer_ellipsis_on_menu.setToolTipText("Switch on answer ellipsis");
	 answer_ellipsis_on_menu.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("ANSWER_ELLIPSIS_ON");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Answer Ellipsis on command succeeded";
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
	 modeMenu.add(Answer_Ellipsis_Menu);
	 Answer_Ellipsis_Menu.add(answer_ellipsis_on_menu);
	 
//	create Translate menu under Mode submenu
	 
	 Translate_menu = new JMenu("Translate Trace");

	 translate_trace_off_menu = new JMenuItem("Translate Trace off");
	 translate_trace_off_menu.setToolTipText("switch off translation tracing (default)");
	 translate_trace_off_menu.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("TRANSLATE_TRACE_OFF");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Translate Trace off command succeeded";
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
	 modeMenu.add(Translate_menu);
	 Translate_menu.add(translate_trace_off_menu);
	 
//	create Translate_off submenu under Mode  Translate submenu
	 translate_trace_on_menu = new JMenuItem("Translate Trace on");
	 translate_trace_on_menu.setToolTipText("switch on translation tracing ");
	 translate_trace_on_menu.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("TRANSLATE_TRACE_ON");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Translate Trace on command succeeded";
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
	 modeMenu.add(Translate_menu);
	 Translate_menu.add(translate_trace_on_menu);
	 FlagMenu.add(modeMenu);
	 bar.add(FlagMenu);
	 
//	create Corpus  menu item
	 
	 CorpusMenu.setMnemonic( 'C');
	 CorpusMenu.addSeparator();
	 
	 createcorpusmeny.createCorpusMenuItemsTable();
	 FromProg = "CreateFrame3";
	 createcorpusmeny.createCorpusMenuStrings(FromProg);
	
	 
	 bar.add(CorpusMenu);
	 
//	create Judge menu item
	 JudgeMenus.setMnemonic( 'J');
	 JudgeMenus.addSeparator();
	 judgemenu.createJudgeMenuItemsTable();
	 FromProgram = "CreateFrame3";
	 judgemenu.createJudgeMenuString(FromProgram);
	 
	 bar.add(JudgeMenus);
	 
	 viewMenu = new JMenu("View");
	 viewMenu.setMnemonic('V');
	 bar.add(viewMenu);
	 
	 History_menu = new JMenu("History");
	 History_menu.setToolTipText("get backlog of sentences");
	// bar.add(History_menu);
	 
//	create Bidirectional  menu item and sub menu items
	 BiDirectional_menu  = new JMenu( "Bidirectional mode" );
	 BiDirectional_menu .setMnemonic( 'B');
	 BiDirectional_on_menuitem = new JMenuItem( "Bidirectional mode on" );
	 BiDirectional_on_menuitem.setToolTipText("Bidirectional mode is switched on");
	 BiDirectional_on_menuitem.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("BIDIRECTIONAL_ON");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Bidirectional on command succeeded";
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
	 
	 BiDirectional_menu.add(BiDirectional_on_menuitem);
	 
	 
	 BiDirectional_off_menuitem = new JMenuItem( "Bidirectional mode off" );
	 BiDirectional_off_menuitem.setToolTipText("Bidirectional mode is switched off");
	 BiDirectional_off_menuitem.addActionListener(
	 		new ActionListener() {
	 			public void actionPerformed( ActionEvent e)
	 			{
	 				regulusWindow.handleCommand("BIDIRECTIONAL_OFF");
	 				if (regulusWindow.regulus_command_succeeded)
					{ 
	 					regulusWindow.InputText = "Bidirectional off command succeeded";
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
	 
	 BiDirectional_menu.add(BiDirectional_off_menuitem);
	 bar.add(BiDirectional_menu);
	 
	 remote = new JRadioButton("Answer",false);
	 bar.add(remote);
	 notRemote = new JRadioButton("Question",true);
	 bar.add(notRemote);
	 radioGroup = new ButtonGroup();
	 radioGroup.add(remote);
	 radioGroup.add(notRemote);
	 
	 // create frame3
	 
	 setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	 // add listener to confirm closing and listener to keyboard
	 inputField.addKeyListener(this);

	  createframe3.addVetoableChangeListener(this);
	  
	  
	  // set up the layout
	  gblayout = new GridBagLayout();
	  displayPanel.setLayout(gblayout);
	  // instansiate the gridbag constraints
	  gbConstraints = new GridBagConstraints();
	  
//	Add labels and textboxes
	  
	  createTestArea();
	  
	  setCheckBoxes();
	  
	  inputField.setText(translatevalue);
	  createTranslationResult();
	  
	 // DisplayScrollPanes[13].addMouseListener(this);
	  DisplayScrollPanes[0].addMouseListener(this);
	  
	  // check if the remote radio button is clicked on
	  remote.addActionListener(this);
	  notRemote.addActionListener(this);
	  setMouseListenerInputField();
	 
	  
	  // Check if Translate button is pressed down
	  
	  translate.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{  
		  		createTranslationResult();
		  		}
		  	}
		  );
	  

// Check if To Source Discource Trace button is pressed down
	  
	  To_Discource_Trace_button.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{  
		  		 TraceFileName =  To_Source_Discourse_Trace;
				  discource_trace = true;
				  WhichTraceFile = "Discourse";
		  		  file_written = false;
				  startPos = 0;
				  creataEmptyTable();
				  callCreateTraceTable();
				  CreateAndLinkrules(WhichTraceFile);
		  		}
		  	}
		  );
	  
// Check if To Interlingua Trace button is pressed down
	  
	  To_Interlingua_Trace_button.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{ 
		  		 TraceFileName = To_Interlingua_Trace;
				  discource_trace = false;
				  WhichTraceFile = "Interlingua";
		  		  file_written = false;
				  startPos = 0;
				  creataEmptyTable();
				  callCreateTraceTable();
				  CreateAndLinkrules(WhichTraceFile);
		  		}
		  	}
		  );
	  
// Check if From Interlingua Trace button is pressed down
	  
	  From_Interlingua_Trace_button.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{  
		  		 TraceFileName = From_Interlingua_Trace;
				  discource_trace = false;
				  WhichTraceFile = "Interlingua";
		  		  file_written = false;
				  startPos = 0;
				  creataEmptyTable();
				  callCreateTraceTable();
				  CreateAndLinkrules(WhichTraceFile);
		  		}
		  	}
		  );
	  
	  
	  inputPanel.add(inputField , BorderLayout.CENTER);
	  inputPanel.add(translate,BorderLayout.EAST);
	  inputPanel.add(bar,BorderLayout.NORTH);
	  c2.add(inputPanel,BorderLayout.NORTH  );
	  c2.add(displayPanel);
	  createframe3.setDefaultCloseOperation(
					  WindowConstants.DISPOSE_ON_CLOSE);
	  createframe3.pack();
	  
	  }

	  public void CreateAndLinkrules(String WhichTraceFile)
	  {
		  
		    rulesPane rulespane = new rulesPane(frame3,this, getRegulusGUI(),WhichTraceFile,saveSentenceIndex,TraceSentenceTable ,TraceRuleTable);
		  	rulespane.setRegulusGUI(getRegulusGUI());
		  	
		  	
			 //  display new internal window
		    JInternalFrame rulesPaneInternalFrame = rulespane.getInternalFrame();
		  //add the internal frame to the desktop
			regulusWindow.desktop.add(rulesPaneInternalFrame,JLayeredPane.DEFAULT_LAYER);
			rulesPaneInternalFrame.setVisible(true); 
	  }
	 
	 // public void makeAndLinkNewFrame3() {
	//	  regulusWindow.handleCommand("TRANSLATE_TRACE_ON");
			 
	//			Frame3 frame3 = new Frame3(getRegulusGUI(),JButton translatebtn, int translateCounter,JButton DebugBtn);
	//			frame3.setRegulusGUI(getRegulusGUI());
				
//	//			display new internal window
	//		  	JInternalFrame frame3InternalFrame = frame3.getInternalFrame();	
//	//		 add the internal frame to the desktop	
	//		  	regulusWindow.desktop.add(frame3InternalFrame);
	//		  	frame3InternalFrame.setVisible(true);
	//		 }
	  public void mouseClicked(MouseEvent e)
	  {
	  }
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			  System.out.println("Left button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			 // System.out.println("right button pressed");
			  //HandleRightClick();
		  }
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
			  createTranslationResult();
		  }
			
	  }
	  public void keyReleased( KeyEvent e)
	  {
		  // TODO method stub 
		 // System.out.println("key released"+e.getKeyCode());
	  }
	  public void keyTyped( KeyEvent e)
	  {
		  // TODO method stub 
		  //System.out.println("key typed"+e.getKeyCode());
	  }
	  public void HandleRightClick()
	  {
		  file_written = false;
		  startPos = 0;
		  creataEmptyTable();
		  callCreateTraceTable();
		  CreateAndLinkrules(WhichTraceFile);
	  }
	 public void creataEmptyTable() {
		 for (int i = 0; i < 20; i++ )
		 {
			 TraceSentenceTable[i] = null;
		 }
	 }
	  public void SetCorpusId(String Corpusident)
	  {
	  	corpusId2 = Corpusident;
	  }
	  public void actionPerformed(ActionEvent e)
	  {
		  if (e.getSource() == remote)
		  {
			  remote_button = true;
			 
		  }
		  else if (e.getSource() == notRemote)
		  {
			  remote_button = false;
			  
		  }
	  }
	  public void checkTranslationResult()
	  {
		  translateval = inputField.getText();
		  if (translateval.equals(" "))
		  {
			  System.out.println("The input field is blank");
		  }
		  else
		  {
			  createTranslationResult();
		  }
	  }
	  public void createTranslationResult()
	  {
		  translateval = inputField.getText();
			inputField.setText("");
			// call module to see if bidirectional mode is on or off
			bidirectional_mode_on = regulusWindow.checkIfInBidirectionalMode();
			// if bidirectional mode is on
			if (bidirectional_mode_on == true)
			{
				 bidirectionalModeIsOn();
			}
			// if bidirectional mode is off
			else
			{
				bidirectionalModeIsOff();
			}
			//remote_button = false;
			GetTranslationItems();
			WriteTranslationItems();
			record_exist_in_table = false;
	  }
	  public void bidirectionalModeIsOn()
	  {
		  if (remote_button == false)
			{
			translationresult = regulusWindow.performTranslation(translateval);
			remote.setSelected(true);
			notRemote.setSelected(false);
			remote_button = true;
			
			}
		else
			{
			regulusWindow.handleRemoteCommand("TRANSLATE_TRACE_ON");
			translationresult = regulusWindow.performRemoteTranslation(translateval);
			remote.setSelected(false);
			notRemote.setSelected(true);
			remote_button = false;
			}
	  }
	  public void bidirectionalModeIsOff()
	  {
			translationresult = regulusWindow.performTranslation(translateval);
			remote.setSelected(false);
			notRemote.setSelected(true);
			remote_button = false;
	  }
	  public void makeandLinkNewCreateCorpusMeny(){
		  whichProgram = "CreateFrame3";
		  CreateCorpusMeny createcorpusmeny = new CreateCorpusMeny(frame3,this,regulusWindow,whichProgram);
		 
	  }
		  public void checkIndex()
		  {
			  for (int i = 0; i < 19; i++ )
			  {
				  int moveIndex = 0;
				  moveIndex = i + 1;
				  SentenceTable[i] = SentenceTable[moveIndex];

			  }
		  }
		  public void createButtonArea() {
			  
//			     Discource Source Trace button
			  To_Discource_Trace_button.setToolTipText("Show Discource Source Trace ");
			  gbConstraints.weightx = 0.5;
			  gbConstraints.gridx = 0;
			  gbConstraints.gridy = 0;
			  gbConstraints.insets = new Insets(1,8,1,1);
			  displayPanel.add(To_Discource_Trace_button,gbConstraints);

//				  To Interlingua Trace button
			  To_Interlingua_Trace_button.setToolTipText("Show From Interlingua Trace ");
			  gbConstraints.weightx = 0.5;
			  gbConstraints.gridx = 1;
			  gbConstraints.gridy = 0;
			  gbConstraints.insets = new Insets(1,8,1,1);
			  displayPanel.add(To_Interlingua_Trace_button,gbConstraints);
			  
//				 From Interlingua Trace button
			  From_Interlingua_Trace_button.setToolTipText("Show From Interlingua Trace ");
			  gbConstraints.weightx = 0.5;
			  gbConstraints.gridx = 2;
			  gbConstraints.gridy = 0;
			  gbConstraints.insets = new Insets(1,8,1,1);
			  displayPanel.add(From_Interlingua_Trace_button,gbConstraints); 
			  
	  
		}
		  
		  public void createTestArea()
		  {
			  createButtonArea();
			  for (index = 0; index < 20; index++ ) 
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
			  DisplayCheckBoxes[2].setSelected(false); 
			  DisplayCheckBoxes[3].setSelected(false); 
			  DisplayCheckBoxes[4].setSelected(false); 
			  DisplayCheckBoxes[5].setSelected(false);
			  DisplayCheckBoxes[7].setSelected(false);
			  DisplayCheckBoxes[8].setSelected(true);
			  DisplayCheckBoxes[9].setSelected(true);
			  DisplayCheckBoxes[10].setSelected(false);
			  DisplayCheckBoxes[11].setSelected(false);
			  DisplayCheckBoxes[13].setSelected(false);
			  DisplayCheckBoxes[14].setSelected(false);
			  DisplayCheckBoxes[15].setSelected(true);
			  DisplayCheckBoxes[16].setSelected(false);
			  DisplayCheckBoxes[17].setSelected(false);
			  DisplayCheckBoxes[18].setSelected(false);
			  DisplayCheckBoxes[19].setSelected(false);
		  
		 }
		  public void createTest()
		  {
			  // add label
			  temp = 3 + index;
			  temp = index+1;
			  setLabelArea();
			 // setTraceLabel();
			  gbConstraints.fill = GridBagConstraints.VERTICAL;
			  gbConstraints.gridx = 0;
			  gbConstraints.gridy = temp;
			  gbConstraints.gridheight = 1;
			  gbConstraints.gridwidth = 1;
			  gbConstraints.weightx = 0;
			  displayPanel.add(DisplayLabels[index] ,gbConstraints);
			  
			  
			  // add textbox
			
			  setTextArea();
			 // setTraceArea();
			  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
			  DisplayTextPanes[index].setEditable(true);
			  DisplayScrollPanes[index] = new JScrollPane(DisplayTextPanes[index]);
			  gbConstraints.fill = GridBagConstraints.BOTH;
			  gbConstraints.gridx = 1;
			  gbConstraints.gridy = temp;
			  gbConstraints.gridheight = 1;
			  gbConstraints.gridwidth = 10;
			  gbConstraints.weightx = 10;
			  setAddArea();
			  checkListener();
		  }
		
		  public void checkListener(){
			  if (index == 14)
			  {
				  setMouseListenerTraceFrom(); 
			  }
			 
			  if (index == 11)
			  {
				  setMouseListenerTraceTo(); 
			  } 
			 if (index == 7)
			  {
				  setMouseListenerTraceDiscource(); 
			  }  
		  }
		  public void setMouseListenerTraceTo()
		  {
			  DisplayTextPanes[index].addMouseListener(new MouseHandlerTraceTo()); 
		  }
		  
		  public void setMouseListenerTraceFrom()
		  {
			  DisplayTextPanes[index].addMouseListener(new MouseHandlerTraceFrom()); 
		  }
		 public void setMouseListenerTraceDiscource()
		 {
			 DisplayTextPanes[index].addMouseListener(new MouseHandlerTraceDiscource());  
		 }
		 public void setMouseListenerInputField()
		 {
			 inputField.addMouseListener(new MouseHandlerInputField());
		 }
		  public void setLabelArea()
		  {
			  if (index == smallTextArea1  ||index == smallTextArea2  || index == smallTextArea3  ||
					  index == smallTextArea4  || index == smallTextArea5  || index == smallTextArea6 ||  index == smallTextArea7
					  ||  index == smallTextArea8 ||  index == smallTextArea9 ||  index == smallTextArea10 ||  index == smallTextArea11)
			  	{
				  	gbConstraints.weighty = 0.1;
			  	}
				else 
				  {
					gbConstraints.weighty = 0.7;
				  }
				
			  }
		  public void setTraceLabel()
		  {
			if (  index == trace2 || index == trace3)
				{
					gbConstraints.weighty = 1;
				}
		  }
	
		  public void setTextArea()
			{
			  if (index == smallTextArea1  ||index == smallTextArea2  || index == smallTextArea3  || 
					  index == smallTextArea4  || index == smallTextArea5  || index == smallTextArea6  || index == smallTextArea7
					  || index == smallTextArea8 || index == smallTextArea9 || index == smallTextArea10 || index == smallTextArea11)	
			  {
				  DisplayTextPanes[index] = new JTextArea(1,90);
			  		gbConstraints.weighty = 0.1;
			  }
			  else 
			  {
				  DisplayTextPanes[index] = new JTextArea(4,90);
				  gbConstraints.weighty = 0.7;
			  }
			
			}
	
		  public void setAddArea()
			{
			  displayPanel.add(DisplayScrollPanes[index],gbConstraints);
			}
		  
		  public void GetTranslationItems()
		  {
			  source = translationresult.getSource();
			  target = translationresult.getTarget();
			  n_parses = translationresult.getNParses();
			  parse_time = translationresult.getParseTime();
			  source_representation = translationresult.getSourceRepresentation();
			  source_discourse = translationresult.getSourceDiscourse();
			  resolved_source_discourse = translationresult.getResolvedSourceDiscourse();
			  resolution_processing = translationresult.getResolutionProcessing();
			  interlingua = translationresult.getInterlingua();
			  target_representation = translationresult.getTargetRepresentation();
			  n_generations = translationresult.getNGenerations();
			  generation_time = translationresult.getGenerationTime();
			  other_translations = translationresult.getOtherTranslations();
			  tagged_translations = translationresult.getTaggedTranslations();
			  To_Source_Discourse_Trace = translationresult.getToSourceDiscourseTrace();
			  To_Interlingua_Trace = translationresult.getToInterlinguaTrace();
			  From_Interlingua_Trace = translationresult.getFromInterlinguaTrace();
			  gloss_translation = translationresult.getGlossTranslation();
			  interlingua_surface = translationresult.getInterlinguaSurface();
			 // calculateLinesUsed();
			  // See if we have an original script translation - if so, we should really have a character encoding too
			  String raw_original_script_translation = translationresult.getOriginalScriptTranslation();
			  String character_encoding = translationresult.getCharacterEncoding();
			  // If have both, unpack the string to bytes and then put it together again using the character encoding
			  if ( raw_original_script_translation != null && character_encoding != null ) {
				  try {
					  original_script_translation = new String(raw_original_script_translation.getBytes(), character_encoding);
				  } catch (Exception e ) {
					  regulusWindow.text.append("\nException " + e);
				  }
			  } 
			  // We shouldn't really have just an original script translation on its own, but let it though if we do
			  // and print a warning
			  else if ( raw_original_script_translation != null ) {
				  original_script_translation = raw_original_script_translation;
				  regulusWindow.text.append("\nWarning: original script translation given, but no character encoding found\n");
			  }
		  }
		 
		
		  public void callCreateTraceTable()
		  {
			   
			  length = TraceFileName.length();
			  for (index = 0; file_written == false ;  index++ ) 
			  {
				  checkIfFileBeginning();
				  if (file_written == false)
				  {
					  whichFile();
				  }
				  else
				  {
					 // System.out.println("table has been created");
				  }
				 
			  }
		  }
		  public void whichFile()
		  {
			 if ( discource_trace == false)
			 {
				 endOfSentenceCounter = 0;
				 traceCounter = 130;
				 createTraceTable(); 
			 }
			 else
			 {
				 createTraceTableWithDiscourse();
			 }
		  }
		  public void checkIfFileBeginning()
		  {
			  int FileendPos = startPos + 68;
			  
			  Trace = TraceFileName.substring(startPos,FileendPos);
			  String b = "FILES";
			  	if (Trace.indexOf(b) != -1) {
			  		TraceSentenceTable[index] = Trace;
			  		//System.out.println("TraceSentenceTable[index] "+TraceSentenceTable[index]);
			  		saveSentenceIndex = index;
			  		//System.out.println("saveSentenceIndex "+saveSentenceIndex);
			  		startPos = FileendPos + 1;
			  		checkIfFile();
			  	}
		  }
		  
		  public void checkIfFile()
		  {
			int FileendPos = startPos + 70;
			Trace = TraceFileName.substring(startPos,length);
			TraceSentenceTable[index] = Trace;
			//System.out.println("TraceSentenceTable[index] "+TraceSentenceTable[index]);
			file_written = true;
		  }
		
		  
		  public void createTraceTable()
		  { 
			  endPos = startPos +  traceCounter;
			  //endPos = startPos + 130;
			  savestartPos = startPos;
			  Trace = TraceFileName.substring(startPos,endPos);
			  String b = "INTERLINGUA";
			  int holdPos = 0;
			  	if (Trace.indexOf(b) != -1) {
				holdPos = Trace.indexOf(b);
				RuleIndexStart = holdPos + startPos;
				startPos = startPos + holdPos;
				endPos = startPos + 47;
				Trace = TraceFileName.substring(startPos,endPos);	
				getEndOfSentence();
				getEndPosition();
			  	}
				else
			  	{
			  	 traceCounter = traceCounter + 15;
			  	createTraceTable();
			  	}
			}

		  public void createTraceTableWithDiscourse()
		  { 
			  
			  endPos = startPos + 130;
			  savestartPos = startPos;
			  Trace = TraceFileName.substring(startPos,endPos);
			  String b = "DISCOURSE";
			  int holdPos = 0;
			  	if (Trace.indexOf(b) != -1) {
				holdPos = Trace.indexOf(b);
				RuleIndexStart = holdPos + startPos;
				RuleIndexStart = RuleIndexStart - 18;
				startPos = startPos + holdPos;
				endPos = startPos + 37;
				Trace = TraceFileName.substring(startPos,endPos);
				getEndOfSentence();
				getEndPosition();
				
			  }
			} 
		public void getEndOfSentence()
		{
			int holdNumber = 0;
			String c = ":";
			if (Trace.indexOf(c) != -1) {
				holdNumber = Trace.indexOf(c);
				startPos = startPos + holdNumber;
				endPos = startPos + 12;
				Trace = TraceFileName.substring(startPos,endPos);
			}
		}
		
		public void getEndPosition()
		{
			int holdCountNumber = 0;
			String bb = "]";
			 if (Trace .indexOf(bb) != -1) {
			    holdCountNumber = Trace.indexOf(bb);
			    // add one to the index number as index starts with zero
			    holdCountNumber = holdCountNumber + 1;
				endPos = startPos + holdCountNumber;
				Trace = TraceFileName.substring(savestartPos,endPos);
				TraceSentenceTable[index] = Trace;
				startPos = endPos + 1;
				RuleIndexEnd = endPos;
				createTraceRuleTable();
			 }
			 else
				{
					endPos = endPos + 10;
					Trace = TraceFileName.substring(startPos,endPos);
					getEndOfSentence();
				}
		}
		public void createTraceRuleTable()
		{
		  Trace = TraceFileName.substring(RuleIndexStart,RuleIndexEnd);
		  TraceRuleTable[index] =  Trace;
		}
	  public void WriteTranslationItems()
	  {
		   DisplayTextPanes[0].setText((source));
		   DisplayTextPanes[0].setCaretPosition(0);
		   DisplayTextPanes[1].setText((target));
		   DisplayTextPanes[1].setCaretPosition(0);
		   DisplayTextPanes[2].setText((gloss_translation));
		   DisplayTextPanes[2].setCaretPosition(0);
		   DisplayTextPanes[3].setText((original_script_translation));
		   DisplayTextPanes[3].setCaretPosition(0);
		   DisplayTextPanes[4].setText((n_parses));
		   DisplayTextPanes[4].setCaretPosition(0);
		   DisplayTextPanes[5].setText((parse_time));
		   DisplayTextPanes[5].setCaretPosition(0);
		   DisplayTextPanes[6].setText((source_representation));
		   DisplayTextPanes[6].setCaretPosition(0);
		   DisplayTextPanes[7].setText(( To_Source_Discourse_Trace));
		   DisplayTextPanes[7].setCaretPosition(0);
		   DisplayTextPanes[8].setText((source_discourse));
		   DisplayTextPanes[8].setCaretPosition(0);
		   DisplayTextPanes[9].setText((resolved_source_discourse));
		   DisplayTextPanes[9].setCaretPosition(0);
		   DisplayTextPanes[10] .setText((resolution_processing));
		   DisplayTextPanes[10].setCaretPosition(0);
		   DisplayTextPanes[11].setText((To_Interlingua_Trace));
		   DisplayTextPanes[11].setCaretPosition(0);
		   DisplayTextPanes[12].setText((interlingua));
		   DisplayTextPanes[12].setCaretPosition(0);
		   DisplayTextPanes[13].setText((interlingua_surface));
		   DisplayTextPanes[13].setCaretPosition(0);
		   DisplayTextPanes[14].setText((From_Interlingua_Trace));
		   DisplayTextPanes[14].setCaretPosition(0);
		   DisplayTextPanes[15].setText((target_representation));
		   DisplayTextPanes[15].setCaretPosition(0);
		   DisplayTextPanes[16] .setText((n_generations));
		   DisplayTextPanes[16].setCaretPosition(0);
		   DisplayTextPanes[17].setText((generation_time));
		   DisplayTextPanes[17].setCaretPosition(0);
		   DisplayTextPanes[18] .setText((other_translations));
		   DisplayTextPanes[18].setCaretPosition(0);
		   DisplayTextPanes[19].setText((tagged_translations));
		   DisplayTextPanes[19].setCaretPosition(0);
		  
	  }
	  
	  public void moveCharacterToStringTable()
	  {
		  for (int ind = 0; ind < 18; ind++)
		  {
			  char myChar = tempCharArray[ind];
			  myLetterArray[ind] = new Character(myChar).toString();
		  }
	  }

	  //public int aPerformed() {  
		 // How many frames do we have
		//  allFrames = regulusWindow.desktop.getAllFrames();
		//  int count = allFrames.length;
		//  return count;
		//  }
					 
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
			 
			JInternalFrame frame2 = (JInternalFrame) event.getSource();
			String name = event.getPropertyName();
			Object value = event.getNewValue();
			int count = 0;
			
			// we only want to check attempts to close a frame
			
			if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
				int result = JOptionPane.showInternalConfirmDialog(frame2,
						" OK to close?");
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
		  class AnswerCheckBoxHandler implements ItemListener
		  {
			  public void itemStateChanged(ItemEvent event)
			  {
				  for (index = 0; index < 20; index++ ) {
					  if (event.getSource() == DisplayCheckBoxes[index]){
						  DisplayLabels[index].setVisible(DisplayCheckBoxes[index].isSelected());
						  DisplayScrollPanes[index].setVisible(DisplayCheckBoxes[index].isSelected());
					  }
				  }
					 
			  }
		  	
		  }
		  class MouseHandlerTraceFrom extends MouseAdapter{
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
					//  System.out.println("trace from");
					  TraceFileName = From_Interlingua_Trace;
					  discource_trace = false;
					  WhichTraceFile = "Interlingua";
					  HandleRightClick();
				  }
			  } 
		  }
		  class MouseHandlerTraceTo extends MouseAdapter{
			  public void mousePressed(MouseEvent e)
			  {
				  int modifiers = e.getModifiers();
				  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
					  System.out.println("Left button pressed");
				  }
				  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
					  System.out.println("Middle button pressed");
				  }
				  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
					 // System.out.println("trace to");
					  TraceFileName = To_Interlingua_Trace;
					  discource_trace = false;
					  WhichTraceFile = "Interlingua";
					  HandleRightClick();
				  }
			  } 
		  }
		  class MouseHandlerTraceDiscource extends MouseAdapter{
			  public void mousePressed(MouseEvent e)
			  {
				  int modifiers = e.getModifiers();
				  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
					  System.out.println("Left button pressed");
				  }
				  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
					  System.out.println("Middle button pressed");
				  }
				  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
					  System.out.println("trace discource");
					  TraceFileName =  To_Source_Discourse_Trace;
					  discource_trace = true;
					  WhichTraceFile = "Discourse";
					  HandleRightClick();
				  }
			  } 
		  }
		  class MouseHandlerInputField extends MouseAdapter{ 
			  public void mousePressed(MouseEvent e)
			  {
				  int modifiers = e.getModifiers();
				  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
					  System.out.println("Left button pressed");
					  createframe3.dispose();
					 // makeAndLinkNewFrame3();
				  }
				  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
					  System.out.println("Middle button pressed");
				  }
				  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
					  System.out.println("right button pressed");
					 
					
				  }
			  } 
		  }
		 
	}
