package RegulusGUI;

import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


public class RegulusMenus extends JFrame implements ItemListener 
{
  private RegulusGUI regulusWindow = null;
  private String InputText = "";
  private int  check_index = 0;
  private String inputString = "";
  
  public RegulusMenus(){
	  
  }
  public RegulusMenus(RegulusGUI window)
  {
	  regulusWindow = window;
	 //  create LOAD  menu item and sub menu items
	 
	regulusWindow.loadMenu = new JMenu( "Load" ); 
	regulusWindow.loadMenu.setMnemonic( 'L');
    regulusWindow.L_loadMenu = new JMenuItem( "Load" );
    regulusWindow.L_loadMenu.setToolTipText("Load current Regulus grammar in DCG and left-corner form");
    regulusWindow.L_loadMenu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
     				createBackGroundJob("LOAD");
     				 int length = regulusWindow.text.getLineCount();
     				 regulusWindow.textLength = regulusWindow.textLength + length;
     				 regulusWindow.text.setCaretPosition( regulusWindow.textLength);
    				
     			}
    		}
  );
    regulusWindow.loadMenu.add(regulusWindow.L_loadMenu);
    if (regulusWindow.config_file_loaded )
    	regulusWindow.loadMenu.setEnabled(true);
    else
    	regulusWindow.loadMenu.setEnabled(false);	
    	
       
//  create EBL_LOAD sub menu item to LOAD
    
    regulusWindow.EBL_loadMenuItem = new JMenuItem( "EBL Load" );
    regulusWindow.EBL_loadMenuItem.setToolTipText("Load current specialised Regulus grammar in DCG and left-corner form ");
    regulusWindow.EBL_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				createBackGroundJob("EBL_LOAD");
    				 int length = regulusWindow.text.getLineCount();
    				 System.out.println("length " + length);
        			}
    		}
    		);
    ;
    regulusWindow.loadMenu.add(regulusWindow.EBL_loadMenuItem);
    
//  create EBL_LOAD_GENERATION sub menu item to LOAD
    
    regulusWindow.EBL_Generation_loadMenuItem = new JMenuItem( "EBL Load Generation" );
    regulusWindow.EBL_Generation_loadMenuItem.setToolTipText("Compile and load current specialised Regulus grammar for generation");
    regulusWindow.EBL_Generation_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("EBL_LOAD_GENERATION");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Ebl Load Generation command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.EBL_Generation_loadMenuItem);
    
//  create EBL_LOAD_GENERATION_Arg sub menu item to LOAD
    
    regulusWindow.EBL_Generation_Arg_loadMenuItem = new JMenuItem( "EBL Load Generation Arg" );
    regulusWindow.EBL_Generation_Arg_loadMenuItem.setToolTipText("Compile and load designated version of current specialised Regulus grammar for generation"); 
    regulusWindow.EBL_Generation_Arg_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("EBL_LOAD_GENERATION_Arg");
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.EBL_Generation_Arg_loadMenuItem);
    
//  create LOAD_Dialogue sub menu item to LOAD
    
    regulusWindow.Dialogue_loadMenuItem = new JMenuItem( "Load Dialogue" );
    regulusWindow.Dialogue_loadMenuItem.setToolTipText("Load dialogue related files");
    regulusWindow.Dialogue_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("LOAD_DIALOGUE");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Load Dialogue command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    			
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Dialogue_loadMenuItem);
    
//  create LOAD_Generation sub menu item to LOAD
    
    regulusWindow.Generation_loadMenuItem = new JMenuItem( "Load Generation" );
    regulusWindow.Generation_loadMenuItem.setToolTipText("Compile and load current generator grammar");
    regulusWindow.Generation_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("LOAD_GENERATION");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Load Generation command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Generation_loadMenuItem);
    

//  create LOAD_Generation_Args sub menu item to LOAD
    
    regulusWindow.Generation_Arg_loadMenuItem = new JMenuItem( "Load Generation Arg" );
    regulusWindow.Generation_Arg_loadMenuItem.setToolTipText("Compile and load current generator grammar, and store as designated subdomain grammar");
    regulusWindow.Generation_Arg_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("LOAD_GENERATION_ARG");
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Generation_Arg_loadMenuItem);
    

    
//  create LOAD_Surface_Patterns sub menu item to LOAD
    
    regulusWindow.Surface_Patterns_loadMenuItem = new JMenuItem( "Load Surface Patterns" );
    regulusWindow.Surface_Patterns_loadMenuItem.setToolTipText("Load current surface patterns and associated files");
    regulusWindow.Surface_Patterns_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("LOAD_SURFACE_PATTERNS");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Load Surface Patterns command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Surface_Patterns_loadMenuItem);
    
//  create LOAD_Translate sub menu item to LOAD
    
    regulusWindow.Translate_loadMenuItem = new JMenuItem( "Load Translate" );
    regulusWindow.Translate_loadMenuItem.setToolTipText("Load translation-related files");
    regulusWindow.Translate_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				createBackGroundJob("LOAD_TRANSLATE");
     				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Translate_loadMenuItem);
    
//  create Compile_Ellipsis_Patterns sub menu item to LOAD
    
    regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem = new JMenuItem( "Compile Ellipsis Patterns" );
    regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem.setToolTipText("Compile patterns used for ellipsis processing");
    regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("COMPILE_ELLIPSIS_PATTERNS");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Compile Elipsis Patterns command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem);
    
//  create Compile Nuance sub menu item to LOAD
    
    regulusWindow.Nuance_loadMenuItem = new JMenuItem( "Compile Regulus into Nuance" );
    regulusWindow.Nuance_loadMenuItem.setToolTipText("Compile current Regulus grammar into Nuance GSL form");
    regulusWindow.Nuance_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("NUANCE");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Nuance command succedeed";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Nuance_loadMenuItem);
    
//  create Gemini sub menu item to LOAD
    
    regulusWindow.Gemini_loadMenuItem = new JMenuItem( "Compile Regulus into Gemini" );
    regulusWindow.Gemini_loadMenuItem.setToolTipText("Compile current Regulus grammar into Gemini form");
    regulusWindow.Gemini_loadMenuItem.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("GEMINI");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Gemini command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.Gemini_loadMenuItem);
   
//  create load_recognition sub menu item to LOAD
    
    regulusWindow.load_recognition_loadMenuItem = new JMenuItem( "Load speech recognition" );
    regulusWindow.load_recognition_loadMenuItem .setToolTipText("Load speech recognition");
    regulusWindow.load_recognition_loadMenuItem .addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				createSpeechBackGroundJob("LOAD_RECOGNITION");
    				
    			}
    		}
	);
    		
    regulusWindow.loadMenu.add(regulusWindow.load_recognition_loadMenuItem );
    
//  create close_down_recognition sub menu item to LOAD
    
    regulusWindow.close_down_recognition_loadMenuItem = new JMenuItem( "Close down speech recognition" );
    regulusWindow.close_down_recognition_loadMenuItem .setToolTipText("Close down speech recognition");
    regulusWindow.close_down_recognition_loadMenuItem .addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("CLOSE_DOWN_RECOGNITION");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Close down recognition command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    regulusWindow.loadMenu.add(regulusWindow.close_down_recognition_loadMenuItem );
    
//  create compile Nuance to recognicer sub menu item to LOAD
    
    regulusWindow.Compile_Nuance_to_recognicer_meuItem = new JMenuItem( "Compile Nuance to recognicer" );
    regulusWindow.Compile_Nuance_to_recognicer_meuItem .setToolTipText("Compile Nuance to recognicer");
    regulusWindow.Compile_Nuance_to_recognicer_meuItem .addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("NUANCE_COMPILE");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Compile Nuance to recognicer command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    
    regulusWindow.loadMenu.add(regulusWindow.Compile_Nuance_to_recognicer_meuItem );
    
//  create compile Nuance to recognicer (PCFG) sub menu item to LOAD
    
    regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem = new JMenuItem( "Compile Nuance to recognicer (PCFG)" );
    regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem .setToolTipText("Compile Nuance to recognicer (PCFG)");
    regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem .addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("NUANCE_COMPILE_WITH_PCFG");
    				if (regulusWindow.regulus_command_succeeded)
    				{ 
    					InputText = "Compile Nuance to recognicer (PCFG) command succeeded";
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				else
    				{
    					String command = regulusWindow.getCommandErrorString();	
    					InputText = command;
    					regulusWindow.txtBoxDisplayPositive(InputText);
    				}
    				checkMenuStatus();
    				
    			}
    		}
    		);
    
    regulusWindow.loadMenu.add(regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem );
    
    
    regulusWindow.FlagMenu.setMnemonic( 'F');
    
//  create Mode sub menu item to Set Flags
    regulusWindow.FlagMenu.addSeparator();
    JMenu modeMenu  = new JMenu( "Mode" );
    regulusWindow.normal_processing_menu = new JMenuItem("Normal_Processing");
    regulusWindow.normal_processing_menu.setToolTipText("Do normal processing on input sentences");
    regulusWindow.normal_processing_menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("NORMAL_PROCESSING");
    				checkMenuStatus();
    				
    			}
    		}
    		);
     
    regulusWindow.FlagMenu.add(modeMenu);
    modeMenu.add(regulusWindow.normal_processing_menu);
    if (regulusWindow.config_file_loaded )
    	regulusWindow.FlagMenu.setEnabled(true);
    else
    	regulusWindow.FlagMenu.setEnabled(false);
   
   
   // create Dialogue menu under Mode submenu
    
    regulusWindow.Dialogue_menu = new JMenuItem("Dialogue");
    regulusWindow.Dialogue_menu.setToolTipText("Do dialogue-style processing on input sentences");
    regulusWindow.Dialogue_menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("DIALOGUE");
    				checkMenuStatus();
  	
    			}
    		}
    		);
    modeMenu.add(regulusWindow.Dialogue_menu);
    
// create Translate menu under Mode submenu
    
    regulusWindow.Translate_menu = new JMenuItem("Translate");
    regulusWindow.Translate_menu.setToolTipText("Do translation-style processing on input sentences");
    regulusWindow.Translate_menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("TRANSLATE");
    				checkMenuStatus();
      				
    			}
    		}
    		);
    modeMenu.add(regulusWindow.Translate_menu);
    
// create Generation menu under Mode submenu
    
    regulusWindow.Generation_menu = new JMenuItem("Generation");
    regulusWindow.Generation_menu.setToolTipText("Generate from parsed input sentences");
    regulusWindow.Generation_menu.addActionListener(
    		new ActionListener() {
    			public void actionPerformed( ActionEvent e)
    			{
    				regulusWindow.handleCommand("GENERATION");
    				checkMenuStatus();
      			}
    		}
    		);
    modeMenu.add(regulusWindow.Generation_menu);
    
//  create Ellipsis sub menu to Set Ellipsis off
    
     regulusWindow.FlagMenu.addSeparator();
    regulusWindow.Answer_Ellipsis_Menu  = new JMenu( "Answer Elipsis" );
    
    regulusWindow.check_Answer_Ellipsis_off = new JCheckBox("Answer Ellipsis off");
    regulusWindow.check_Answer_Ellipsis_off.setToolTipText("Switch off answer ellipsis");
    regulusWindow.FlagMenu.add(regulusWindow.Answer_Ellipsis_Menu);
    regulusWindow.Answer_Ellipsis_Menu.add( regulusWindow.check_Answer_Ellipsis_off );
    regulusWindow.check_Answer_Ellipsis_off.addItemListener(this);
    
       
//  create Ellipsis sub menu to Set Ellipsis on
    
    regulusWindow.check_Answer_Ellipsis_on = new JCheckBox("Answer Ellipsis on");
    regulusWindow.check_Answer_Ellipsis_on.setToolTipText("Switch on answer ellipsis");
    regulusWindow.Answer_Ellipsis_Menu.add( regulusWindow.check_Answer_Ellipsis_on );
    regulusWindow.check_Answer_Ellipsis_on.addItemListener(this);
    
//  create Compaction sub menu to Set Compaction off
    
    regulusWindow.FlagMenu.addSeparator();
    regulusWindow.Compaction_Menu  = new JMenu( "Compaction" );
    
    regulusWindow.check_compaction_off = new JCheckBox("No Compaction");
    regulusWindow.check_compaction_off.setToolTipText("Switch off compaction processing for Regulus to Nuance conversion");
    regulusWindow.FlagMenu.add(regulusWindow.Compaction_Menu);
    regulusWindow.Compaction_Menu.add(regulusWindow.check_compaction_off);
    regulusWindow.check_compaction_off.addItemListener(this);
    
//  create Compaction sub menu to Set Compaction on
    
    regulusWindow.check_compaction_on = new JCheckBox("Compaction");
    regulusWindow.check_compaction_on.setToolTipText("Switch on compaction processing for Regulus to Nuance conversion");
    regulusWindow.Compaction_Menu.add(regulusWindow.check_compaction_on);
    regulusWindow.check_compaction_on.addItemListener(this);
    
//  create Parser sub menu and DCG Parser Menu
    
    regulusWindow.FlagMenu.addSeparator();
    regulusWindow.Parser_Menu  = new JMenu( "Parser" );
    regulusWindow.check_dcg_parser = new JCheckBox("Use Dcg Parser");
    regulusWindow.check_dcg_parser.setToolTipText("Use Dcg Parser");
    
    regulusWindow.FlagMenu.add(regulusWindow.Parser_Menu);
    regulusWindow.Parser_Menu.add(regulusWindow.check_dcg_parser);
    regulusWindow.check_dcg_parser.addItemListener(this);
    
//  create DCG Parser Menu under Parser sub menu
    
    regulusWindow.check_lc_parser = new JCheckBox("Use left-corner parser");
    regulusWindow.check_lc_parser.setToolTipText("Use left-corner parser");
    
    regulusWindow.FlagMenu.add(regulusWindow.Parser_Menu);
    regulusWindow.Parser_Menu.add(regulusWindow.check_lc_parser);
    regulusWindow.check_lc_parser.addItemListener(this);
    regulusWindow.bar.add(regulusWindow.FlagMenu);
    
//  create Surface Menu under Parser sub menu
    
    regulusWindow.check_surface_parser = new JCheckBox("Use surface parser ");
    regulusWindow.check_surface_parser.setToolTipText("Use surface pattern-matching parser");
    
    regulusWindow.FlagMenu.add(regulusWindow.Parser_Menu);
    regulusWindow.Parser_Menu.add(regulusWindow.check_surface_parser);
    regulusWindow.check_surface_parser.addItemListener(this);
    
//  create Echo sub menu and Echo off Menu under Echo Menu
    
    regulusWindow.FlagMenu.addSeparator();
    regulusWindow.Echo_Menu  = new JMenu( "Echo" );
    regulusWindow.check_echo_off = new JCheckBox("Echo off");
    regulusWindow.check_echo_off.setToolTipText("Don't echo input sentences (default)");
   
    regulusWindow.FlagMenu.add(regulusWindow.Echo_Menu);
    regulusWindow.Echo_Menu.add(regulusWindow.check_echo_off);
    regulusWindow.check_echo_off.addItemListener(this);
    
//  create echo on menu under Echo sub menu
    
    regulusWindow.check_echo_on = new JCheckBox("Echo on");
    regulusWindow.check_echo_on.setToolTipText("Echo input sentences (normally useful only in batch mode");
  
    regulusWindow.Echo_Menu.add(regulusWindow.check_echo_on);
    regulusWindow.check_echo_on.addItemListener(this);
}
  public void itemStateChanged(ItemEvent e) {
		 
		 Object source = e.getItemSelectable();
		 if (source ==  regulusWindow.check_Answer_Ellipsis_on) {
			// System.out.println(" I have selected bidirectional mode on");
			 check_index = 1;
	 	}else if (source ==  regulusWindow.check_Answer_Ellipsis_off) {
			//System.out.println(" I have selected bidirectional mode off");
			 check_index = 2;
  		}else if (source ==  regulusWindow.check_compaction_off) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 3;
  		}else if (source ==  regulusWindow.check_compaction_on) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 4;
  		}else if (source ==  regulusWindow.check_echo_off) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 5;
  		}else if (source ==  regulusWindow.check_echo_on) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 6;
  		}else if (source ==  regulusWindow.check_dcg_parser) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 7;
  		}else if (source ==  regulusWindow.check_lc_parser) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 8;
  		}else if (source ==  regulusWindow.check_surface_parser) {
  			//System.out.println(" I have selected bidirectional mode off");
  			check_index = 9;
  		}
		 if (e.getStateChange() == ItemEvent.SELECTED) {
			// System.out.println(" Item is selected");
			 moveCommand();
		
		 }
	 }
 
  public void moveCommand() {
  	if (check_index == 1) {
  		 regulusWindow.handleCommand("ANSWER_ELLIPSIS_ON");
  	  	 inputString = "Ellipsis on command succeeded";
  	   regulusWindow.check_Answer_Ellipsis_on.setSelected(true);	 
		 regulusWindow.check_Answer_Ellipsis_off.setSelected(false);
  		 checkRegulusCommand();
   }	else if (check_index == 2) {
	   	regulusWindow.handleCommand("ANSWER_ELLIPSIS_OFF");
	  	inputString = "Ellipsis off command succeeded";
	  	regulusWindow.check_Answer_Ellipsis_on.setSelected(false);	 
		regulusWindow.check_Answer_Ellipsis_off.setSelected(true);	
		checkRegulusCommand();
  }		else if (check_index == 3) {
	   	regulusWindow.handleCommand("NO_COMPACTION");
	  	inputString = "Compaction off command succeeded";
	  	regulusWindow.check_compaction_on.setSelected(false);	 
		regulusWindow.check_compaction_off.setSelected(true);
	  	checkRegulusCommand();
  }		else if (check_index == 4) {
	   	regulusWindow.handleCommand("COMPACTION");
	  	inputString = "Compaction command succeeded";
	  	regulusWindow.check_compaction_on.setSelected(true);	 
		regulusWindow.check_compaction_off.setSelected(false);	
	  	checkRegulusCommand();
  }		else if (check_index == 5) {
	   	regulusWindow.handleCommand("ECHO_OFF");
	  	inputString = "Echo off command succeeded";
	  	regulusWindow.check_echo_off.setSelected(true);	 
		regulusWindow.check_echo_on.setSelected(false);	
	  	checkRegulusCommand();
  }		else if (check_index == 6) {
	   	regulusWindow.handleCommand("ECHO_ON");
	  	inputString = "Echo on command succeeded";
	  	regulusWindow.check_echo_on.setSelected(true);	 
		regulusWindow.check_echo_off.setSelected(false);	
	  	checkRegulusCommand();
  }		else if (check_index == 7) {
	   	regulusWindow.handleCommand("DCG");
	  	inputString = "Dcg Parser command succeeded";
	  	regulusWindow.check_dcg_parser.setSelected(true);	 
		regulusWindow.check_lc_parser.setSelected(false);
		regulusWindow.check_surface_parser.setSelected(false);
	  	checkRegulusCommand();
  }		else if (check_index == 8) {
	   	regulusWindow.handleCommand("LC");
	  	inputString = "Lc Parser command succeeded";
	  	regulusWindow.check_dcg_parser.setSelected(false);	 
		regulusWindow.check_lc_parser.setSelected(true);
		regulusWindow.check_surface_parser.setSelected(false);
	  	checkRegulusCommand();
  }		else if (check_index == 9) {
	   	regulusWindow.handleCommand("SURFACE");
	  	inputString = "Surface Parser command succeeded";
	  	regulusWindow.check_dcg_parser.setSelected(false);	 
		regulusWindow.check_lc_parser.setSelected(false);
		regulusWindow.check_surface_parser.setSelected(true);
	  	checkRegulusCommand();
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
	 
	 
  public void createBackGroundJob(String strCommand)
  { 
  	String CommandIdent = strCommand;
  	new CommandInBackGround(regulusWindow,CommandIdent).start();
  	
  	new ReadOneCommandFile(regulusWindow,CommandIdent).start();
  }
  
  public void createSpeechBackGroundJob(String strCommand)
  { 
  	String CommandIdent = strCommand;
  	SpeechCommandInBackGround speechcommandinbackground = new SpeechCommandInBackGround(regulusWindow,CommandIdent);
  	progressLoadRecognition  progressload = new progressLoadRecognition(regulusWindow,CommandIdent,speechcommandinbackground );
  	
  	speechcommandinbackground.start();
  	progressload.start();
  }
  public void checkMenuStatus()
  {
	regulusWindow.updateCommandStatus();
	regulusWindow.availablemenus.check_available_menus();
	regulusWindow.unavailablecommands.check_unavailable_menus();
  }
}