package RegulusGUI;
import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class AvailableMenusForFrame4 extends JFrame{
	private RegulusGUI regulusWindow = null;
	private Frame4 frame4 = null;
	private int i = 0;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	  get the pointer to frame4 window
	  public Frame4 getFrame4() {
		  return frame4;
	  }
	  
//	 set the pointer to the Frame4 window
	  public void setFrame4(Frame4 window) {
		  frame4 = window;
	  }
	  
	  public   AvailableMenusForFrame4 ()
	  {
			
	  }
	  public   AvailableMenusForFrame4 (Frame4 frame, RegulusGUI regulusgui)
	  {
			frame4 = frame;
			setRegulusGUI(regulusgui);	
	  }
	  
	  public void check_available_menus() 
		{
			
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  if (regulusWindow.availableCommands[i].equals("EBL_LOAD"))
				 frame4.L_EblloadMenuItem.setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("LOAD"))
				 frame4.L_loadMenuItem.setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  
			 if (regulusWindow.availableCommands[i].equals("LOAD_DIALOGUE"))
				 frame4.L_loadDialogueMenuItem.setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("LOAD_RECOGNITION"))
				 frame4.Load_recognition_MenuItem.setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("CLOSE_DOWN_RECOGNITION"))
				 frame4.close_down_recognition_MenuItem .setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NUANCE"))
				 frame4.Nuance_loadMenuItem .setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NUANCE_COMPILE"))
				 frame4.Compile_Nuance_to_recognicer_MenuItem .setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			 if (regulusWindow.availableCommands[i].equals("NUANCE_COMPILE_WITH_PCFG"))
				 frame4.Compile_Nuance_to_recognicer_pcfg_MenuItem .setEnabled(true);
			 }
		}
}


