package RegulusGUI;
import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class AvailableMenus extends JFrame
{
	private RegulusGUI regulusWindow = null;
	private int i = 0;
	 
	public AvailableMenus()
	{
	}
	public AvailableMenus(RegulusGUI window)
	 {
		  regulusWindow = window;
	 }
	public void check_available_menus() 
	{
//		 Check if  Command is available
		 
		 
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("LOAD"))
				 regulusWindow.L_loadMenu.setEnabled(true);
			 }
			 for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_LOAD"))
				 regulusWindow.EBL_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_LOAD_GENERATION"))
				 regulusWindow.EBL_Generation_loadMenuItem.setEnabled(true);
			 regulusWindow.EBL_Generation_Arg_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
				
			 if (regulusWindow.availableCommands[i].equals("LOAD_DIALOGUE"))
				 regulusWindow.Dialogue_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("LOAD_GENERATION"))
				 regulusWindow.Generation_loadMenuItem.setEnabled(true);
			 regulusWindow.Generation_Arg_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("LOAD_SURFACE_PATTERNS"))
				 regulusWindow.Surface_Patterns_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("LOAD_TRANSLATE"))
				 regulusWindow.Translate_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("COMPILE_ELLIPSIS_PATTERNS"))
				 regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("GEMINI"))
				 regulusWindow.Gemini_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("DIALOGUE"))
				 regulusWindow.Dialogue_menu.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("ANSWER_ELLIPSIS_OFF"))
				 regulusWindow.check_Answer_Ellipsis_off.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("ANSWER_ELLIPSIS_ON"))
				 regulusWindow.check_Answer_Ellipsis_on.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("TRANSLATE"))
				 regulusWindow.Translate_menu.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
					 
			 if (regulusWindow.availableCommands[i].equals("NUANCE"))
				 regulusWindow.Nuance_loadMenuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NORMAL_PROCESSING"))
				 regulusWindow.normal_processing_menu.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("DCG"))
				 regulusWindow.check_dcg_parser.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NO_COMPACTION"))
				 regulusWindow.check_compaction_off.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("COMPACTION"))
				 regulusWindow.check_compaction_on.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("LC"))
				 regulusWindow.check_lc_parser.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("ECHO_OFF"))
				 regulusWindow.check_echo_off.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("ECHO_ON"))
				 regulusWindow.check_echo_on.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("STEPPER"))
				 regulusWindow.stepper_Menu.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("GENERATION"))
				 regulusWindow.Generation_menu.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL"))
				 regulusWindow.Ebl_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_TRAIN"))
				 regulusWindow.Ebl_Train_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_ANALYSIS"))
				 regulusWindow.Ebl_Analysis_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_GEMINI"))
				 regulusWindow.Ebl_Gemini_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_GENERATION"))
				 regulusWindow.Ebl_Generation_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_TREEBANK"))
				 regulusWindow.Ebl_Treebank_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_GRAMMAR_PROBS"))
				 regulusWindow.Ebl_Grammar_Probs_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_POSTPROCESS"))
				 regulusWindow.Ebl_Postprocess_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("EBL_NUANCE"))
				 regulusWindow.Ebl_Nuance_menuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NUANCE_COMPILE"))
				 regulusWindow.Compile_Nuance_to_recognicer_meuItem.setEnabled(true);
			 }
			 for ( i = 1 ; i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NUANCE_COMPILE_WITH_PCFG"))
				 regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem.setEnabled(true);
			 }
		}
	
	
}
