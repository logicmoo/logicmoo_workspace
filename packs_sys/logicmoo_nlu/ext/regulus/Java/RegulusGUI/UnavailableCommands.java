package RegulusGUI;

import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class UnavailableCommands  extends JFrame
{
	private RegulusGUI regulusWindow = null;
	private int i = 0;
	 
	public UnavailableCommands()
	{
	}
	public UnavailableCommands(RegulusGUI window)
	  {
		  regulusWindow = window;
	  }
	  
		public void check_unavailable_menus() 
		{
		  for ( i = 1 ;i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_LOAD"))
				 regulusWindow.EBL_loadMenuItem.setEnabled(false);
			 }
		  for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("LOAD"))
				 regulusWindow.L_loadMenu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_LOAD_GENERATION"))
				 regulusWindow.EBL_Generation_loadMenuItem.setEnabled(false);
			 regulusWindow.EBL_Generation_Arg_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("LOAD_DIALOGUE"))
				 regulusWindow.Dialogue_loadMenuItem.setEnabled(false);
			 }
			 
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
		     if (regulusWindow.unavailableCommands[i].equals("LOAD_GENERATION"))
		    	 regulusWindow.Generation_loadMenuItem.setEnabled(false);
		     regulusWindow.Generation_Arg_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("LOAD_SURFACE_PATTERNS"))
				 regulusWindow.Surface_Patterns_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("LOAD_TRANSLATE"))
				 regulusWindow.Translate_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("COMPILE_ELLIPSIS_PATTERNS"))
				 regulusWindow.Compile_Ellipsis_Patterns_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("GEMINI"))
				 regulusWindow.Gemini_loadMenuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("DIALOGUE"))
				 regulusWindow.Dialogue_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("ANSWER_ELLIPSIS_OFF"))
				 regulusWindow.check_Answer_Ellipsis_off.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("ANSWER_ELLIPSIS_ON"))
				 regulusWindow.check_Answer_Ellipsis_on.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("TRANSLATE"))
				 regulusWindow.Translate_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
					 
			 if (regulusWindow.unavailableCommands[i].equals("STEPPER"))
				 regulusWindow.stepper_Menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("NORMAL_PROCESSING"))
				 regulusWindow.normal_processing_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("DCG"))
				 regulusWindow.check_dcg_parser.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("NO_COMPACTION"))
				 regulusWindow.compaction_off_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("COMPACTION"))
				 regulusWindow.compaction_on_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("LC"))
				 regulusWindow.check_lc_parser.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("ECHO_OFF"))
				 regulusWindow.check_echo_off.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("ECHO_ON"))
				 regulusWindow.check_echo_on.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("GENERATION"))
				 regulusWindow.Generation_menu.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL"))
				 regulusWindow.Ebl_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_TRAIN"))
				 regulusWindow.Ebl_Train_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_ANALYSIS"))
				 regulusWindow.Ebl_Analysis_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_GEMINI"))
				 regulusWindow.Ebl_Gemini_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_GENERATION"))
				 regulusWindow.Ebl_Generation_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_TREEBANK"))
				 regulusWindow.Ebl_Treebank_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_GRAMMAR_PROBS"))
				 regulusWindow.Ebl_Grammar_Probs_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_POSTPROCESS"))
				 regulusWindow.Ebl_Postprocess_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("EBL_NUANCE"))
				 regulusWindow.Ebl_Nuance_menuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("NUANCE_COMPILE"))
				 regulusWindow.Compile_Nuance_to_recognicer_meuItem.setEnabled(false);
			 }
			 for ( i = 1 ; i < regulusWindow.num_UnAvailable ; i++ )
			 {
			 if (regulusWindow.unavailableCommands[i].equals("NUANCE_COMPILE_WITH_PCFG"))
				 regulusWindow.Compile_Nuance_to_recognicer_pcfg_meuItem.setEnabled(false);
			 }
		 }
}
