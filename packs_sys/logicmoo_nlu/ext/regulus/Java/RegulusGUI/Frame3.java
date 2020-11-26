package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLayeredPane;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


public class Frame3 extends JFrame 
				implements  ActionListener,VetoableChangeListener,KeyListener,MouseListener,ItemListener,InternalFrameListener{
	
//	 pointer to the reguluswindow which has created stepper window 
 private RegulusGUI regulusWindow = null;
 private CreateFrame3 createframe3 = null;
 private LoadErrorPane loaderrorpane = null;
 private JInternalFrame frame3 = null;
 private UnavailableCommandsForFrame3 unavailablecommandsforframe3 = null;
 private AvailableMenusForFrame3 availablemenusforframe3 = null;
 public  JInternalFrame[] allFrames = null;
 private GridBagLayout gblayout;
 private GridBagConstraints gbConstraints;
 public  JTextField inputField = new JTextField(55);
 public  JButton translate = new JButton("Translate");
 private JButton From_Interlingua_Trace_button = new JButton("From Interlingua Trace");
 private JButton To_Interlingua_Trace_button = new JButton("To Interlingua Trace");
 private JButton To_Discource_Trace_button = new JButton("To Source Discourse Trace");
 private JRadioButton remote, notRemote;
 private JButton recognice = new JButton("Recognise") ;
 private ButtonGroup radioGroup;
 public  JMenuItem L_loadMenuItem = new JMenuItem( "Load " );
 public  JMenuItem L_EblloadMenuItem = new JMenuItem( "Ebl Load " );
 public  JMenuItem loadRecognitionMenuItem = new JMenuItem( "Load Speech Recognition " );
 public  JMenuItem close_down_recognition_MenuItem = new JMenuItem( "Close down Speech Recognition " );
 public  JMenuItem  Surface_Patterns_loadMenuItem = new JMenuItem( "Load Surface Patterns" );
 public  JMenuItem Compile_Nuance_to_recognicer_MenuItem = new JMenuItem( "Compile Nuance to Recogniser " );
 public  JMenuItem Compile_Nuance_to_recognicer_pcfg_MenuItem = new JMenuItem( "Compile Nuance to Recogniser(PCFG)" );
 public  JMenuItem Compile_Ellipsis_Patterns_loadMenuItem = new JMenuItem( "Compile Ellipsis Patterns" ); 
 public  JMenuItem Nuance_loadMenuItem = new JMenuItem( "Compile Regulus into Nuance" );
 public  JMenuItem Gemini_loadMenuItem = new JMenuItem( "Compile Regulus into Gemini" );
 private JMenu FlagMenu = new JMenu( "Set Flags" );
 private JMenu Translate_menu;
 public  JMenuItem L_loadMenu = new JMenuItem( "Load Translate" );
 private JMenuItem translate_trace_off_menu;
 private JMenuItem translate_trace_on_menu;
 public  JMenu CorpusMenu = new JMenu( "Corpus" );
 public  JMenu JudgeMenus = new JMenu( "Judge" );
 private JMenu History_menu = new JMenu( "History Text" );
 private JMenu History_Speech_menu = new JMenu( "Speech History " );
 private JMenu BiDirectional_menu = new JMenu( "Bidirectional mode" );
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
 public JPanel buttonPanel = new JPanel();
 private JPanel inputPanel = new JPanel(new BorderLayout());
 private JPanel barPanel = new JPanel(new BorderLayout());
 private final int trace1 = 7;
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
 private boolean record_exist_in_table = false;
 private int saveIndex = 0;
 public  JMenuItem[] corpus_menuitem = new JMenuItem[9];
 public  JMenuItem[] judge_menuitem = new JMenuItem[9];
 public  char[] tempCharArray;
 public  String[] myLetterArray = new String[36];
 public  int charIndex = 0;
 public  int result = 0;
 public  CreateCorpusMeny createcorpusmeny = null; 
 public  JudgeMenu judgemenu = null;
 private  boolean remote_button = false;
 private  boolean bidirectional_mode_on = false;
 private  boolean file_written = false;
 public String corpusId2;
 private int startPos = 0;
 private int endPos = 0;
 private String Trace = "";
 private  String[] TraceSentenceTable = new String[50];
 private  String[] TraceRuleTable = new String[50];
 private int sentenceindex = 0;
 private int savestartPos = 0;
 private int saveendPos = 0;
 private int length = 0;
 public  int holdIndex = 0;
 private int firstDigit = 0;
 private int indexCount = 0;
 private boolean found_character = false;
 private int numberIndex = 0;
 private int RuleIndexStart = 0;
 private int RuleIndexEnd = 0;
 private JList summaryList ; 
 private DefaultListModel listModel;
 private String holdTextArea = "";
 public  int saveSentenceIndex = 0;
 private String TraceFileName = "";
 private boolean discource_trace = false;
 private String WhichTraceFile = "";
 public  String[] Source_Rep_items =  new String[8];
 private int Source_Rep_length = 0;
 private int Interlingua_length = 0;
 private int lines_used = 0;
 private int lines_pooled = 0;
 private int holdLength = 0;
 private int lines_left = 0;
 private int lines_extra = 0;
 private String whichProgram = "";
 private String FromProg = "";
 private String FromProgram = "";
 private String translatevalue = "";
 private int From_Interlingua_Trace_Hight = 0;
 private int  To_Interlingua_Trace_Hight = 0;
 private int  Source_Rep_Hight = 0;
 private int Interlingua_Hight = 0;
 private int given_hight = 0;
 private int counted_hight = 0;
 private int createHistoryIndex = 0;
 private boolean   end_ofTable;
 private int Sum_of_Frame3 = 0;
 public JProgressBar progressBar;
 private int traceCounter = 0;
 private int endOfSentenceCounter = 0;
 private String speechRecognitionString = "";
 private JCheckBox check_Bidirectional_mode_on;
 private JCheckBox check_Bidirectional_mode_off;
 private JCheckBox check_Answer_Ellipsis_on;
 private JCheckBox check_Answer_Ellipsis_off;
 private JCheckBox check_Translate_Trace_on;
 private JCheckBox check_Translate_Trace_off;
 private String inputString = "";
 private int check_index = 0;
 private JMenuItem quit_system_MenuItem;
 private JButton Translatebtn =     new JButton("Translator ");
 private JButton debugbtn =     new JButton("Debugging Trace");
 private int TranslateCounter = 0;
 private String ac = "";
 private String bc = "";
 private JInternalFrame[] frames = null;
 private int frameIndex = 0;
 private int holdFrameIndex = 0;
 private String strName = "";
 private boolean   remove_finished = false;
 private Component c = null;
 private int speechTableIndex = 0;
 private JMenuItem[] speech_Table_menuItem = new JMenuItem[40];
 private int createSpeechIndex = 0;
 private String speechval = "";
 private String speechFileName = "";
 public int numOfUnavailableCommands = 0; 
 private int traceWindowCounter = 0;
 public  JButton buttonThree;
 public  JButton buttonFour;
 public  JButton buttonFive;
 private int traceWindowFromCounter = 0;
 private int traceWindowDiscourceCounter = 0;
 private int checkIndex = 0;
 private int frameCounter = 0;
 private boolean   to_clicked = false;
 private boolean   from_clicked = false;
 private boolean   discource_clicked = false;
 private int traceCount = 0;
 
 
  // send name of internal frame 
	 public JInternalFrame getInternalFrame() {
	  return frame3;
	  }
  // get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
 // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
		// set the pointer to the CreateFrame3 window
		  public void setCreateFrame3(CreateFrame3 framewindow) {
			  createframe3 = framewindow;
	  }
	  public Frame3()
	  {
		  
	  }
	  
  public Frame3(RegulusGUI window,JButton translatebtn, int translateCounter,JButton DebugBtn) {
	  regulusWindow = window;
	  Translatebtn = translatebtn;
	  debugbtn = DebugBtn;
	  TranslateCounter = translateCounter;
	  Translatebtn.addMouseListener(this);
	  debugbtn.addMouseListener(this);
	  
	  createcorpusmeny = new CreateCorpusMeny(this,createframe3, window,whichProgram);
	  
	  judgemenu = new JudgeMenu(this,createframe3, window);
		
	  setJMenuBar( bar );  // set the menubar for the JInternalFrame	
	  
	  frame3 = new JInternalFrame("Translator "+TranslateCounter,true,true,true,true);

	  c2 = frame3.getContentPane();
 
	  regulusWindow.updateCommandStatus();
	 // System.out.println("regulusWindow.num_Available "+regulusWindow.num_Available);
	  numOfUnavailableCommands = regulusWindow.num_UnAvailable;
	  unavailablecommandsforframe3 = new UnavailableCommandsForFrame3(this, getRegulusGUI());
	  availablemenusforframe3 = new AvailableMenusForFrame3(this, getRegulusGUI());
	  unavailablecommandsforframe3.check_unavailable_menus(); 
	  availablemenusforframe3.check_available_menus(); 
	  
//	create LOAD  menu item and sub menu items
	  
	  JMenu loadMenu = new JMenu( "Load" );
	  loadMenu.setMnemonic( 'L');
	 
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
	 					createLoadErrorWindow();
	 					
	 				}
	  				 regulusWindow.updateCommandStatus();
	  				 unavailablecommandsforframe3.check_unavailable_menus(); 
	  				 availablemenusforframe3.check_available_menus(); 
	  				
	  			}
	  		}
	  		);
	  
	  loadMenu.add(L_loadMenuItem);
	  bar.add(loadMenu);
	  
//	create ebl LOAD  menu item 
	
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
	  				regulusWindow.updateCommandStatus();
	  				unavailablecommandsforframe3.check_unavailable_menus(); 
	  				availablemenusforframe3.check_available_menus(); 
	  				
	  			}
	  		}
	  		);
	  
	  loadMenu.add(L_EblloadMenuItem);
	  bar.add(loadMenu);
  
//create LOAD  Translate menu item 

 
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
 				regulusWindow.updateCommandStatus();
  				unavailablecommandsforframe3.check_unavailable_menus(); 
  				availablemenusforframe3.check_available_menus(); 
 			
 			}
 		}
 		);
 
 loadMenu.add(L_loadMenu);
 bar.add(loadMenu);
 
 
//create LOAD speech Recognition menu item 
 
 loadRecognitionMenuItem = new JMenuItem( "Load Speech Recognition " );
 loadRecognitionMenuItem.setToolTipText("Load speech Recognition ");
 loadRecognitionMenuItem.addActionListener(
 		new ActionListener() {
 			public void actionPerformed( ActionEvent e)
 			{
 				createSpeechBackGroundJob("LOAD_RECOGNITION");
  			}
 			
 		}
 		);
 regulusWindow.updateCommandStatus();
 unavailablecommandsforframe3.check_unavailable_menus(); 
 availablemenusforframe3.check_available_menus(); 
 close_down_recognition_MenuItem.setEnabled(true);
 loadMenu.add(loadRecognitionMenuItem);
 bar.add(loadMenu);
 
//	create close down recognition  menu item Load Dialogue
 
 
 close_down_recognition_MenuItem.setToolTipText("Close down Speech Recognition ");
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
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
				
 			}
 		}
 		);
 
 loadMenu.add( close_down_recognition_MenuItem);
 bar.add(loadMenu);
 
//create LOAD_Surface_Patterns sub menu item to LOAD
 

 Surface_Patterns_loadMenuItem.setToolTipText("Load current surface patterns and associated files");
 Surface_Patterns_loadMenuItem.addActionListener(
 		new ActionListener() {
 			public void actionPerformed( ActionEvent e)
 			{
 				regulusWindow.handleCommand("LOAD_SURFACE_PATTERNS");
 				if (regulusWindow.regulus_command_succeeded)
 				{ 
 					regulusWindow.InputText = "Load Surface Patterns command succeeded";
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				else
 				{
 					String command = regulusWindow.getCommandErrorString();	
 					regulusWindow.InputText = command;
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				regulusWindow.updateCommandStatus();
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
  				
 			}
 		}
 		);
 loadMenu.add(Surface_Patterns_loadMenuItem);
 bar.add(loadMenu);
 
 
//	create compile Nuance to Recognicer menu item Load Dialogue
 
 
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
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
		
 			}
 		}
 		);
 
 loadMenu.add( Compile_Nuance_to_recognicer_MenuItem);
 bar.add(loadMenu);
 
//	create compile Nuance to Recognicer (PCFG) menu item Load Dialogue
 
 
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
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
 			}
 		}
 		);
 
 loadMenu.add( Compile_Nuance_to_recognicer_pcfg_MenuItem);
 bar.add(loadMenu);
 
//create Compile_Ellipsis_Patterns sub menu item to LOAD
 
 
 Compile_Ellipsis_Patterns_loadMenuItem.setToolTipText("Compile patterns used for ellipsis processing");
 Compile_Ellipsis_Patterns_loadMenuItem.addActionListener(
 		new ActionListener() {
 			public void actionPerformed( ActionEvent e)
 			{
 				regulusWindow.handleCommand("COMPILE_ELLIPSIS_PATTERNS");
 				if (regulusWindow.regulus_command_succeeded)
 				{ 
 					regulusWindow.InputText = "Compile Elipsis Patterns command succeeded";
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				else
 				{
 					String command = regulusWindow.getCommandErrorString();	
 					regulusWindow.InputText = command;
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				regulusWindow.updateCommandStatus();
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
  			}
 		}
 		);
 loadMenu.add(Compile_Ellipsis_Patterns_loadMenuItem);
 bar.add(loadMenu);

//create Compile Nuance sub menu item to LOAD
 
 
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
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
  			}
 		}
 		);
 loadMenu.add(Nuance_loadMenuItem);
 bar.add(loadMenu);
 
//create Gemini sub menu item to LOAD
 
 
 Gemini_loadMenuItem.setToolTipText("Compile current Regulus grammar into Gemini form");
 Gemini_loadMenuItem.addActionListener(
 		new ActionListener() {
 			public void actionPerformed( ActionEvent e)
 			{
 				regulusWindow.handleCommand("GEMINI");
 				if (regulusWindow.regulus_command_succeeded)
 				{ 
 					regulusWindow.InputText = "Gemini command succeeded";
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				else
 				{
 					String command = regulusWindow.getCommandErrorString();	
 					regulusWindow.InputText = command;
 					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
 				}
 				regulusWindow.updateCommandStatus();
				unavailablecommandsforframe3.check_unavailable_menus(); 
				availablemenusforframe3.check_available_menus(); 
   			}
 		}
 		);
 loadMenu.add(Gemini_loadMenuItem);
 bar.add(loadMenu);
 
//	create exit system menu item
 
 quit_system_MenuItem = new JMenuItem( "Exit System" );
 quit_system_MenuItem.setToolTipText("Exit System");
 quit_system_MenuItem.addActionListener(
 		new ActionListener() {
 			public void actionPerformed( ActionEvent e)
 			{
 				System.exit(0);
  			}
 		}
 		);
 
 loadMenu.add(quit_system_MenuItem );
 bar.add(loadMenu);
//create Set Flags  menu item
 
  FlagMenu.setMnemonic( 'F');
 //create Mode sub menu item to Set Flags
 FlagMenu.addSeparator();
 JMenu modeMenu  = new JMenu( "Mode" );
//create Ellipsis sub menu to Set Ellipsis off
 
 modeMenu.addSeparator();
 JMenu Answer_Ellipsis_Menu  = new JMenu( "Answer Elipsis" );
 check_Answer_Ellipsis_off = new JCheckBox("Answer Ellipsis off");
 check_Answer_Ellipsis_off.setToolTipText("Switch off answer ellipsis");
 
 modeMenu.add(Answer_Ellipsis_Menu);
 Answer_Ellipsis_Menu.add(check_Answer_Ellipsis_off );
 check_Answer_Ellipsis_off.addItemListener(this);
 	
 
//create Ellipsis sub menu to Set Ellipsis on
 check_Answer_Ellipsis_on = new JCheckBox("Answer Ellipsis on");
 check_Answer_Ellipsis_on.setToolTipText("Switch on answer ellipsis");
 
 modeMenu.add(Answer_Ellipsis_Menu);
 Answer_Ellipsis_Menu.add(check_Answer_Ellipsis_on );
 check_Answer_Ellipsis_on.addItemListener(this);
//create Translate menu under Mode submenu
 
 Translate_menu = new JMenu("Translate Trace");

 check_Translate_Trace_off = new JCheckBox("Translate Trace off");
 check_Translate_Trace_off.setToolTipText("switch off translation tracing ");
 
 modeMenu.add(Translate_menu);
 Translate_menu.add(check_Translate_Trace_off );
 check_Translate_Trace_off.addItemListener(this);
 
//create Translate_off submenu under Mode  Translate submenu
 check_Translate_Trace_on = new JCheckBox("Translate Trace on");
 check_Translate_Trace_on.setToolTipText("switch on translation tracing ");
 
 modeMenu.add(Translate_menu);
 Translate_menu.add(check_Translate_Trace_on);
 check_Translate_Trace_on.addItemListener(this);
 
 FlagMenu.add(modeMenu);
 bar.add(FlagMenu);
 
//create Corpus  menu item
 
 CorpusMenu.setMnemonic( 'C');
 CorpusMenu.addSeparator();
 CorpusMenu.setEnabled(false);
 createcorpusmeny.createCorpusMenuItemsTable();
 FromProg = "Frame3";
 createcorpusmeny.createCorpusMenu(FromProg);
  
 
 bar.add(CorpusMenu);
 
//create Judge menu item
 JudgeMenus.setMnemonic( 'J');
 JudgeMenus.addSeparator();
 JudgeMenus.setEnabled(false);
 judgemenu.createJudgeMenuItemsTable();
 FromProgram = "Frame3";
 judgemenu.createJudgeMenu(FromProgram);
 
 
 bar.add(JudgeMenus);
 
 viewMenu = new JMenu("View");
 viewMenu.setMnemonic('V');
 bar.add(viewMenu);
 
 History_menu = new JMenu(" Text History");
 History_menu.setToolTipText("get backlog of sentences");
 History_menu.setMnemonic( 'H');
 History_menu.setEnabled(false);
 
// callCreateHistoryMenu();
 
 History_Speech_menu.setEnabled(false);
 bar.add(History_menu);
 
 bar.add(History_Speech_menu);

 
//create Bidirectional  menu item and sub menu items
 BiDirectional_menu  = new JMenu( "Bidirectional mode" );
 BiDirectional_menu .setMnemonic( 'B');
 
 
 check_Bidirectional_mode_on = new JCheckBox("Bidirectional mode on" );
 check_Bidirectional_mode_off = new JCheckBox("Bidirectional mode off" );
 check_Bidirectional_mode_on.setSelected(false);
 check_Bidirectional_mode_off.setSelected(false);
 
 // register listener for the checkboxes
 check_Bidirectional_mode_on.addItemListener(this); 
 check_Bidirectional_mode_off.addItemListener(this); 
 
 BiDirectional_menu.add(check_Bidirectional_mode_on);
 BiDirectional_menu.add(check_Bidirectional_mode_off);
 

 bar.add(BiDirectional_menu);
 
 remote = new JRadioButton("Answer",false);
 bar.add(remote);
 notRemote = new JRadioButton("Question",true);
 bar.add(notRemote);
 
 radioGroup = new ButtonGroup();
 radioGroup.add(remote);
 radioGroup.add(notRemote);
 
 bar.add(recognice,BorderLayout.EAST);

 // create frame3
 
 setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
 // add listener to confirm closing and listener to keyboard
 inputField.addKeyListener(this);

 frame3.addVetoableChangeListener(this);
  
  
  // set up the layout
  gblayout = new GridBagLayout();;
  displayPanel.setLayout(gblayout);
  buttonPanel.setLayout(gblayout);
  // instansiate the gridbag constraints
  gbConstraints = new GridBagConstraints();
  
 
 
  // check if the remote radio button is clicked on
  remote.addActionListener(this);
  notRemote.addActionListener(this);
 
  // Check if Translate button is pressed down
  
  translate.addActionListener(
	  	new ActionListener() {
	  		public void actionPerformed( ActionEvent e)
	  		{
	  		remove_finished = false;
	  		setCounter();
	  		checkToFrame();
	  		checkToFrameWindow();
	  		checkFromFrame();
	  		checkFromFrameWindow();
	  		checkDiscourseFrame();
	  		checkDiscourseFrameWindow();
	  		to_clicked = false;
	  		from_clicked = false;
	  		discource_clicked = false;
	  		translatevalue = inputField.getText();
	  		createTranslationResult();
	  		
	  		}
	  	}
	  );
  
//Check if To Source Discource Trace button is pressed down
  
  To_Discource_Trace_button.addActionListener(
	  	new ActionListener() {
	  		public void actionPerformed( ActionEvent e)
	  		{  
	  		  discource_clicked = true;
	  		  TraceFileName =  To_Source_Discourse_Trace;
			  discource_trace = true;
			  WhichTraceFile = "Discourse";
	  		  file_written = false;
			  startPos = 0;
			  creataEmptyTable();
			  callCreateTraceTable();
			  CreateAndLinkToSourceDiscourceTrace(WhichTraceFile);
	  		}
	  	}
	  );
  
//Check if To Interlingua Trace button is pressed down
  
  To_Interlingua_Trace_button.addActionListener(
	  	new ActionListener() {
	  		public void actionPerformed( ActionEvent e)
	  		{ 
	  		 to_clicked = true;
	  		 TraceFileName = To_Interlingua_Trace;
			 discource_trace = false;
			  WhichTraceFile = "Interlingua";
	  		  file_written = false;
			  startPos = 0;
			  creataEmptyTable();
			  callCreateTraceTable();
			  CreateAndLinkToInterlinguaTrace(WhichTraceFile);
	  		}
	  	}
	  );
  
//Check if From Interlingua Trace button is pressed down
  
  From_Interlingua_Trace_button.addActionListener(
	  	new ActionListener() {
	  		public void actionPerformed( ActionEvent e)
	  		{  
	  		  from_clicked = true;
	  		  TraceFileName = From_Interlingua_Trace;
			  discource_trace = false;
			  WhichTraceFile = "Interlingua";
	  		  file_written = false;
			  startPos = 0;
			  creataEmptyTable();
			  callCreateTraceTable();
			  CreateAndLinkFromInterlinguaTrace(WhichTraceFile);
	  		}
	  	}
	  );
  
//Check if Recognice button is pressed down
  
  recognice.addActionListener(
		  	new ActionListener() {
		  		public void actionPerformed( ActionEvent e)
		  		{
		  		  switchOnSpeechRecognicion();
		  		  translatevalue = inputField.getText();
				 
				  	if (translatevalue.equals(null))
					{
				  		System.out.println("no input value");
					}
				  else
				  {
				   createTranslationResult();
				   createSpeechHistoryMenuTable();
				   callCreateSpeechMenu();
				  }
		  		
		  		}
		  	}
		  );
  

  
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
  buttonPanel.add(translate,BorderLayout.EAST);
  
  inputPanel.add( barPanel,BorderLayout.NORTH); 
  inputPanel.add( buttonPanel,BorderLayout.CENTER); 
  
  c2.add(inputPanel,BorderLayout.NORTH  );

  frame3.addInternalFrameListener(this);
  
  frame3.setDefaultCloseOperation(
				  WindowConstants.DISPOSE_ON_CLOSE);
  //frame3.pack();
  frame3.setSize(950,600);
 // treePane.setSize(450,650);
  
  }
  public void setCounter() {
	  //System.out.println("to_clicked "+to_clicked +"from_clicked "+from_clicked +"discource_clicked "+discource_clicked);
		 if (to_clicked && from_clicked && discource_clicked)
		 {
		 traceCount = 5;
		 }
		 else if (to_clicked == false && from_clicked == true && discource_clicked == false)
		 {
			 traceCount = 3; 
		 }
		 else if (to_clicked == false && from_clicked == false && discource_clicked == true)
		 {
			 traceCount = 3; 
		 }
		 else if (to_clicked && from_clicked && discource_clicked == false)
		 {
		 traceCount = 4;
		 }
		 else if (to_clicked && from_clicked == false && discource_clicked)
		 {
		 traceCount = 4;
		 }
		 else if (to_clicked == false && from_clicked && discource_clicked)
		 {
		 traceCount = 4;
		 }
		 else if (to_clicked == false && from_clicked == false && discource_clicked == false)
		 {
			 traceCount = 2; 
		 }
		 else 
		 {
			traceCount = 3; 
		 }
	 }
  public void setCounterAfterTo() {
	  if (to_clicked && from_clicked == false && discource_clicked == false)
		  traceCount = traceCount -1; 
	  else if (to_clicked && from_clicked &&discource_clicked == false)
		  traceCount = traceCount -1;
	  else if (to_clicked && from_clicked &&discource_clicked )
		  traceCount = traceCount -1;
	  else if (to_clicked && from_clicked == false &&discource_clicked )
		  traceCount = traceCount -1;
  }
   
 public void checkToFrame() {
	 frames = regulusWindow.desktop.getAllFrames();
	 frameCounter = frames.length;
	 frameCounter = frameCounter - 1;
	 int counter = regulusWindow.desktop.getComponentCount();
     for (int i = 0; i < traceCount; i++) {
	 c = regulusWindow. barPanel.getComponent(i);
	 String strname = c.toString();
     String b = "To";
		 if (strname.indexOf(b) != -1) {
			 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
			 regulusWindow.barPanel.repaint();
			 setCounterAfterTo();
		}
	 }
 }
 

 
 public void checkToFrameWindow(){
 	 frames = regulusWindow.desktop.getAllFrames();
 	 frameIndex = 0;
      int countFrames = frames.length;
      for (int i = 0; i < frameCounter; i++) {
       String strFrames = frames[i].toString();
       String b = "To";
		 if (strFrames.indexOf(b) != -1) {
			 
			if (frames[i].isShowing())
			{
				frames[i].dispose();
				}
		 
		}
   }
}	    	
 public void checkFromFrame() {
	 frames = regulusWindow.desktop.getAllFrames();
     int countFrames = frames.length;
     int counter = regulusWindow.desktop.getComponentCount();
     for (int i = 0; i < traceCount; i++) {
	 c = regulusWindow. barPanel.getComponent(i);
	 String strname = c.toString();
     String b = "From";
		 if (strname.indexOf(b) != -1) {
			 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
			 regulusWindow.barPanel.repaint();
			 traceCount = traceCount -1;
		}
	 }
 }
 
 public void checkFromFrameWindow(){
 	 frames = regulusWindow.desktop.getAllFrames();
 	 frameIndex = 0;
      int countFrames = frames.length;
      for (int i = 0; i < countFrames; i++) {
       String strFrames = frames[i].toString();
       String b = "From";
		 if (strFrames.indexOf(b) != -1) {
			 
			if (frames[i].isShowing())
			{
				frames[i].dispose();
				}
		 
		}
   }
}	
 public void checkDiscourseFrame() {
	 frames = regulusWindow.desktop.getAllFrames();
	 
     int countFrames = frames.length;
     int counter = regulusWindow.desktop.getComponentCount();
      for (int i = 0; i < traceCount; i++) {
	 c = regulusWindow. barPanel.getComponent(i);
	 String strname = c.toString();
     String b = "Discource";
		 if (strname.indexOf(b) != -1) {
			 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
			 regulusWindow.barPanel.repaint();
			 
		}
	 }
 }
 
 public void checkDiscourseFrameWindow(){
 	 frames = regulusWindow.desktop.getAllFrames();
 	 frameIndex = 0;
      int countFrames = frames.length;
      for (int i = 0; i < countFrames; i++) {
        String strFrames = frames[i].toString();
       String b = "Discource";
		 if (strFrames.indexOf(b) != -1) {
			 
			if (frames[i].isShowing())
			{
				frames[i].dispose();
				}
		 
		}
   }
}	    	
  public void createSpeechBackGroundJob(String strCommand)
  { 
  	String CommandIdent = strCommand;
  	SpeechCommandInBackGround speechcommandinbackground = new SpeechCommandInBackGround(regulusWindow,CommandIdent);
  	progressLoadRecognition  progressload = new progressLoadRecognition(regulusWindow,CommandIdent,speechcommandinbackground );
  	
  	speechcommandinbackground.start();
  	progressload.start();
  }
  
  public void createLoadErrorWindow() {
	  LoadErrorPane loaderrorpane = new LoadErrorPane(this, getRegulusGUI());
	  JInternalFrame LoadErrorPaneInternalFrame = loaderrorpane.getInternalFrame();
	  regulusWindow.desktop.add(LoadErrorPaneInternalFrame);
	  LoadErrorPaneInternalFrame.setVisible(true);
  }
 public void itemStateChanged(ItemEvent e) {
	 
	 Object source = e.getItemSelectable();
	 if (source == check_Bidirectional_mode_on) {
		// System.out.println(" I have selected bidirectional mode on");
		 check_index = 1;
	}else if (source == check_Bidirectional_mode_off) {
			//System.out.println(" I have selected bidirectional mode off");
			 check_index = 2;
	 
 	}else if (source == check_Answer_Ellipsis_on) {
		//System.out.println(" I have selected bidirectional mode off");
		 check_index = 3;
 		
 	}else if (source == check_Answer_Ellipsis_off) {
		//System.out.println(" I have selected bidirectional mode off");
		 check_index = 4;
 	}else if (source == check_Translate_Trace_on) {
		//System.out.println(" I have selected bidirectional mode off");
		 check_index = 5;
 	}else if (source == check_Translate_Trace_off) {
		//System.out.println(" I have selected bidirectional mode off");
		 check_index = 6;
		}
	 if (e.getStateChange() == ItemEvent.SELECTED) {
		// System.out.println(" Item is selected");
		 moveCommand();
	 }
 }
 
 
     public void moveCommand() {
    	if (check_index == 1) {
    	 regulusWindow.handleCommand("BIDIRECTIONAL_ON");
    	 inputString = "Bidirectional on command succeeded";
    	 checkRegulusCommand();
    	 check_Bidirectional_mode_on.setSelected(true);
    	 check_Bidirectional_mode_off.setSelected(false);
    	
     }	else if (check_index == 2) {
    	regulusWindow.handleCommand("BIDIRECTIONAL_OFF");
    	inputString = "Bidirectional off command succeeded";
		checkRegulusCommand();
		check_Bidirectional_mode_on.setSelected(false);
			 check_Bidirectional_mode_off.setSelected(true);
     }	else if (check_index == 3) {
    	 regulusWindow.handleCommand("ANSWER_ELLIPSIS_ON");
    	 inputString = "Ellipsis on command succeeded";
		 checkRegulusCommand();
		 check_Answer_Ellipsis_on.setSelected(true);	 
		 check_Answer_Ellipsis_off.setSelected(false);	
     }	else if (check_index == 4) {
    	 regulusWindow.handleCommand("ANSWER_ELLIPSIS_OFF");
    	 inputString = "Ellipsis off command succeeded";
		 checkRegulusCommand();
		 check_Answer_Ellipsis_on.setSelected(false);	 
		 check_Answer_Ellipsis_off.setSelected(true);	
     }	else if (check_index == 5) {
    	 regulusWindow.handleCommand("TRANSLATE_TRACE_ON");
    	 inputString = "Translate Trace on command succeeded";
		 checkRegulusCommand();
		 check_Translate_Trace_on.setSelected(true);	 
		 check_Translate_Trace_off.setSelected(false);	
     }	else if (check_index == 6) {
    	 regulusWindow.handleCommand("TRANSLATE_TRACE_OFF");
    	 inputString = "Translate Trace off command succeeded";
		 checkRegulusCommand();
		 check_Translate_Trace_on.setSelected(false);	 
		 check_Translate_Trace_off.setSelected(true);	
     	}	
     }
     
    
	 public void checkRegulusCommand() {
		 if (regulusWindow.regulus_command_succeeded)
			{ 
				regulusWindow.InputText = inputString;
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
  public void CreateAndLinkrules(String WhichTraceFile)
  {
	    rulesPane rulespane = new rulesPane(this,createframe3, getRegulusGUI(),WhichTraceFile ,saveSentenceIndex,TraceSentenceTable,TraceRuleTable);
	  	rulespane.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
	    JInternalFrame rulesPaneInternalFrame = rulespane.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(rulesPaneInternalFrame,JLayeredPane.DEFAULT_LAYER);
		rulesPaneInternalFrame.setVisible(true); 
  }
  
  public void CreateAndLinkToInterlinguaTrace(String WhichTraceFile)
  {
//		 update the taskbar on the desktop
	  traceWindowCounter  = traceWindowCounter  + 1;
		
		buttonThree = new JButton("To interlingua Trace " );
		regulusWindow.barPanel.add( buttonThree);
	    ToInterlinguaTrace tointerlinguatrace = new ToInterlinguaTrace(this,createframe3, getRegulusGUI(),WhichTraceFile ,saveSentenceIndex,TraceSentenceTable,
	    		TraceRuleTable,buttonThree,traceWindowCounter);
	    tointerlinguatrace.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
	    JInternalFrame tointerlinguatraceInternalFrame = tointerlinguatrace.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(tointerlinguatraceInternalFrame,JLayeredPane.DEFAULT_LAYER);
		tointerlinguatraceInternalFrame.setVisible(true); 
		//tointerlinguatraceInternalFrame.toFront();
  }
  
  public void CreateAndLinkFromInterlinguaTrace(String WhichTraceFile)
  {
//		 update the taskbar on the desktop
	  traceWindowFromCounter  = traceWindowFromCounter  + 1;
		
		buttonFour = new JButton("From interlingua Trace ");
		regulusWindow.barPanel.add( buttonFour);
	    FromInterlinguaTrace frominterlinguatrace = new FromInterlinguaTrace(this,createframe3, getRegulusGUI(),WhichTraceFile ,saveSentenceIndex,TraceSentenceTable,
	    		TraceRuleTable,buttonFour,traceWindowFromCounter);
	    frominterlinguatrace.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
	    JInternalFrame frominterlinguatraceInternalFrame = frominterlinguatrace.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(frominterlinguatraceInternalFrame,JLayeredPane.DEFAULT_LAYER);
		frominterlinguatraceInternalFrame.setVisible(true); 
		//frominterlinguatraceInternalFrame.toFront();
  }
  
  public void CreateAndLinkToSourceDiscourceTrace(String WhichTraceFile)
  {
//		 update the taskbar on the desktop
	  traceWindowDiscourceCounter  = traceWindowDiscourceCounter  + 1;
		
		buttonFive = new JButton("Source Discource Trace " );
		regulusWindow.barPanel.add( buttonFive);
	    ToSourceDiscourceTrace tosourcediscourcetrace = new ToSourceDiscourceTrace (this,createframe3, getRegulusGUI(),WhichTraceFile ,saveSentenceIndex,TraceSentenceTable,
	    		TraceRuleTable,buttonFive,traceWindowDiscourceCounter);
	    tosourcediscourcetrace.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
	    JInternalFrame tosourcediscourcetraceInternalFrame = tosourcediscourcetrace.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(tosourcediscourcetraceInternalFrame,JLayeredPane.DEFAULT_LAYER);
		tosourcediscourcetraceInternalFrame.setVisible(true); 
		
  }
 
  public void mouseClicked(MouseEvent me)
  {
	  SwingUtilities.isRightMouseButton(me);
   
    {
       // it is right mouse click
      
    }
  }
  public void mousePressed(MouseEvent e)
  {
	 // System.out.println("mousepressed");
	  JButton button = (JButton)e.getSource();
	  String cc = button.getActionCommand();
	  System.out.println("cc "+cc);
	  if (cc.startsWith("Debugging"))
	  {
		  bc = debugbtn.getActionCommand();
		  getAllFramesDebug();
	  }
	  else if (cc.startsWith("Translator"))
	  {
		  ac = Translatebtn.getActionCommand();
		  System.out.println("ac "+ac);
		  getAllFrames();
	  }
  	
   
  }
  
	public void lookForIcon() {
		for (int i = 0; remove_finished == false ; i ++)  {
			 c = regulusWindow. barPanel.getComponent(i);
			 String strname = c.toString();
			 //String strName = c.getName();
			 String b = strName;
			 b = "Translator";
			 if (strname.indexOf(b) != -1) {
				 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
				 regulusWindow.barPanel.repaint();
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
	    	Translatebtn =     new JButton(strName);
	    	regulusWindow.barPanel.add(Translatebtn);
	    	Translatebtn.addMouseListener(this);
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
	         String b = ac;
	         System.out.println("b "+b);
			 if (strFrames.indexOf(b) != -1) {
  				try {
  					System.out.println("strFrames "+strFrames);
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
 	         String b = "Translator";
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
  }
  public void mouseExited(MouseEvent e)
  {
  }
  public void keyPressed( KeyEvent e)
  {
	  
	  if (e.getKeyCode() == e.VK_ENTER)
	  {
		 System.out.println("Keypressed");
		 remove_finished = false;
	  	setCounter();
	  	checkToFrame();
	  	checkToFrameWindow();
	  	checkFromFrame();
	  	checkFromFrameWindow();
	  	checkDiscourseFrame();
	  	checkDiscourseFrameWindow();
	  	to_clicked = false;
	  	from_clicked = false;
	  	discource_clicked = false;
		translatevalue = inputField.getText();
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
 public void switchOnSpeechRecognicion() {
	 
	 speechRecognitionString  =  regulusWindow. doSpeechRecognition();
	 String b = "ERROR";
	 
	 // check it there is an error or the string is okay
 
	 if	 (speechRecognitionString.startsWith(b)) {
		 doSpeechErrorHandling(); 
	 }
	 else
	 {
		 inputField.setText(speechRecognitionString);
	 }
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
 public void doSpeechErrorHandling() {
	 JOptionPane.showMessageDialog(null, speechRecognitionString 
				,"Warning for SpeechRecognition",JOptionPane.INFORMATION_MESSAGE);
 }
   public void createTranslationResult()
  {
	  translateval = inputField.getText();
	  compare();
	  
	  // if the record does not exist in historytable then add it
	  System.out.println("record_exist_in_table "+record_exist_in_table  );
	  	if (record_exist_in_table == false)
	 		{
	  			end_ofTable = false;
	   			ReadTableSaveItem();
	   			callCreateHistoryMenu();
	   			//saveItem();
	  		}
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
   
   public void createSpeechHistoryMenuTable() {
	   RegulusGUI.WavfileInfo latestWavfile = regulusWindow.getWavfileInfo(1)[0];
	   speechFileName = latestWavfile.getFileName();
	   String strTime = latestWavfile.getTimeStamp();
	   String strWords = latestWavfile.getWords();
	   String fullSpeechItem = strTime + strWords;
	   regulusWindow.speechSentenceTable[speechTableIndex] = speechFileName;
	  // System.out.println("regulusWindow.speechSentenceTable[speechTableIndex] "+regulusWindow.speechSentenceTable[speechTableIndex]);
	  // System.out.println("speechTableIndex "+speechTableIndex);
	   speechval = fullSpeechItem;
	   speechTableIndex++;
   
   }
   
   public void callCreateSpeechMenu()
	 {
		 for (int i = 0;regulusWindow.speechSentenceTable[i]!= null ; i++ )
		  {
			 createSpeechIndex = i;
		  }
		 createSpeechMenu(); 
	 }


	public void createSpeechMenu()
	{
		speech_Table_menuItem[createSpeechIndex] = new JMenuItem(speechval);
		
		speech_Table_menuItem[createSpeechIndex].addActionListener(
			 		new ActionListener() {
			 			public void actionPerformed( ActionEvent e)
			 			{
			 				for (int menuIndex = 0; menuIndex < 39; menuIndex++ ) 
			 				{
			 					if (e.getSource() == speech_Table_menuItem[menuIndex])
			 					{ 		
				 				String menuString = speech_Table_menuItem[menuIndex].getText();
				 				
				 				String tableFile =  regulusWindow.speechSentenceTable[menuIndex];
				 				String SpeechResult = regulusWindow.doSpeechRecognitionFromWavfile(tableFile);
				 				//System.out.println("SpeechResult "+SpeechResult);
				 				inputField.setText(SpeechResult);
			 					translatevalue = inputField.getText();
			 				  
			 					}
			 				}
			 							 				
			 			}
			 		}
	 		);
		History_Speech_menu.setEnabled(true);
		History_Speech_menu.add(speech_Table_menuItem[createSpeechIndex]);
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
 
	  
	  public void compare()
	  {
		  for (int i = 0; i < 19; i++ )
		  {
			  compareItem(i); 
		  }
	  }
	  public void compareItem(int i)
	  {
		  if (regulusWindow.SentenceTable[i]!= null && regulusWindow.SentenceTable[i].equals(translateval))
			  {
				  record_exist_in_table = true;
			  }
	  }
	  public void checkHowManyItems()
	  {
		  for (int i = 1;regulusWindow.SentenceTable[i]!= null ; i++ )
		  {
			 // System.out.println("regulusWindow.SentenceTable[i]"+regulusWindow.SentenceTable[i]);
		  }
	  }
	  public void ReadTableSaveItem()
		 {
			 for (int i = 1;end_ofTable == false ; i++ )
			  {
				 if(regulusWindow.SentenceTable[i] == null)
				 {
				 regulusWindow.SentenceTable[i] = translateval;
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
		  regulusWindow.SentenceTable[saveIndex] = translateval;
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
		 one_step_back_menuItem[createHistoryIndex] = new JMenuItem(translateval);
		
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
			 					translatevalue = inputField.getText();
			 				  
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
	  public void createInputArea() {
  
//		     Discource Source Trace button
		  To_Discource_Trace_button.setToolTipText("Show Discource Source Trace ");
		  gbConstraints.weightx = 0.5;
		  ////gbConstraints.weighty = 1;
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = 0;
		  gbConstraints.insets = new Insets(1,8,1,1);
		  displayPanel.add(To_Discource_Trace_button,gbConstraints);
		
		  
//		  From Interlingua Trace button
		  To_Interlingua_Trace_button.setToolTipText("Show From Interlingua Trace ");
		  gbConstraints.weightx = 0.5;
		  //gbConstraints.weighty = 1;
		  gbConstraints.gridx = 1;
		  gbConstraints.gridy = 0;
		  gbConstraints.insets = new Insets(1,8,1,1);
		  displayPanel.add(To_Interlingua_Trace_button,gbConstraints);
		 
		  
//			 From Interlingua Trace button
		  From_Interlingua_Trace_button.setToolTipText("Show From Interlingua Trace ");
		  gbConstraints.weightx = 0.5;
		 // gbConstraints.weighty = 1;
		  gbConstraints.gridx = 2;
		  gbConstraints.gridy = 0;
		  gbConstraints.insets = new Insets(1,8,1,1);
		  displayPanel.add(From_Interlingua_Trace_button,gbConstraints); 
		 
		  
	}
	  public void createTestArea()
	  {
		  createInputArea();
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
		  temp = index;
		  temp = index + 1;
		  setLabelArea();
		 // setTraceLabel();
		  gbConstraints.fill = GridBagConstraints.VERTICAL;
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = temp;
		  gbConstraints.gridheight = 1;
		  gbConstraints.gridwidth = 1;
		  gbConstraints.weightx = 0;
		  gbConstraints.insets = new Insets(8,8,8,8);
		  displayPanel.add(DisplayLabels[index] ,gbConstraints);
		  
		  
		  // add textbox
		
		  setTextArea();
		  //setTraceArea();
		  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
		  DisplayTextPanes[index].setEditable(true);
		  DisplayScrollPanes[index] = new JScrollPane(DisplayTextPanes[index]);
		  gbConstraints.fill = GridBagConstraints.BOTH;
		  gbConstraints.gridx = 1;
		  gbConstraints.gridy = temp;
		  gbConstraints.gridheight = 1;
		  gbConstraints.gridwidth = 10;
		  gbConstraints.weightx = 10;
		  gbConstraints.insets = new Insets(1,8,1,8);
		  setAddArea();
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
		  System.out.println("length "+length );
		  for (index = 0; file_written == false ;  index++ ) 
		  {
			  checkIfFileBeginning();
			  if (file_written == false)
			  {
				  whichFile();
			  }
			  else
			  {
				//  System.out.println("table has been created");
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
		  		saveSentenceIndex = index;
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
		 //System.out.println(" I am in the beginning of createTraceTable");
		  endPos = startPos +  traceCounter;
		  savestartPos = startPos;
		  Trace = TraceFileName.substring(startPos,endPos);
		  String b = "INTERLINGUA";
		  int holdPos = 0;
		  	if (Trace.indexOf(b) != -1) {
			holdPos = Trace.indexOf(b);
			RuleIndexStart = holdPos + startPos;
			startPos = startPos + holdPos;
			endPos = startPos + 37;
			Trace = TraceFileName.substring(startPos,endPos);
			getEndOfSentence();
			getEndPosition();
		  }
		  	else
		  	{
		  	 traceCounter = traceCounter + 10;
		  	//System.out.println("endPos "+endPos);
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
		else
		{
			endPos = endPos + 10;
			Trace = TraceFileName.substring(startPos,endPos);
			getEndOfSentence();
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
			//System.out.println("TraceSentenceTable[index] "+TraceSentenceTable[index]  );
			startPos = endPos + 1;
			RuleIndexEnd = endPos;
			createTraceRuleTable();
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
		 
		JInternalFrame frame3 = (JInternalFrame) event.getSource();
		String name = event.getPropertyName();
		Object value = event.getNewValue();
		int count = 0;
		
		// we only want to check attempts to close a frame
		
		if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
			int result = JOptionPane.showConfirmDialog(frame3,
					"         OK to close?",
					"Close Confirm Pane",
					JOptionPane.OK_CANCEL_OPTION); 
					
			// if the user doesn't agree veto the close
			if (result == JOptionPane.CANCEL_OPTION)
				throw new PropertyVetoException("User cancelled close", event);
			else
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
	 
	
}
