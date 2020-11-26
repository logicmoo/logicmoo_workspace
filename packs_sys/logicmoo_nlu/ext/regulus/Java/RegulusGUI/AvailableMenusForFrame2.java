package RegulusGUI;
import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class AvailableMenusForFrame2 extends JFrame{
	private RegulusGUI regulusWindow = null;
	private Frame2 frame2 = null;
	private int i = 0;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	  get the pointer to frame2 window
	  public Frame2 getFrame2() {
		  return frame2;
	  }
	  
//	 set the pointer to the Frame2 window
	  public void setFrame2(Frame2 window) {
		  frame2 = window;
	  }
	public  AvailableMenusForFrame2()
	{
		
	}
	
	public  AvailableMenusForFrame2(Frame2 frame, RegulusGUI regulusgui)
	{
		frame2 = frame;
		setRegulusGUI(regulusgui);
		 
	}
	public void check_available_menus() 
	{
		{
			//System.out.println("regulusWindow.num_UnAvailable in checker"+regulusWindow.num_UnAvailable);
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  
			 if (regulusWindow.availableCommands[i].equals("EBL_LOAD"))
				 frame2. EBL_loadMenuItem.setEnabled(true);
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  
			 if (regulusWindow.availableCommands[i].equals("LOAD"))
				 frame2.L_loadMenu.setEnabled(true);
			
			 }
		 
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  
			 if (regulusWindow.availableCommands[i].equals("LOAD_GENERATION"))
				 frame2.L_Generation_loadMenu.setEnabled(true);
			 	 frame2.Load_Generation_Arg_MenuItem.setEnabled(true);
			
			 }
		  for ( i = 1 ;i < regulusWindow.num_Available ; i++ )
			 {
			  
			 if (regulusWindow.availableCommands[i].equals("EBL_LOAD_GENERATION"))
				 frame2.EBL_load_GenerationMenuItem.setEnabled(true);
			 	 frame2.EBL_load_GenerationMenuItem_Arg.setEnabled(true);	
			 }
		}
	}
}



