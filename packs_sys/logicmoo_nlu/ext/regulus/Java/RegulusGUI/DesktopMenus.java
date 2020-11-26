package RegulusGUI;

import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class DesktopMenus extends JFrame
{
	private RegulusGUI regulusWindow = null;
	  private String InputText = "";
	  
	  public DesktopMenus(){
	  
	  }
	  public DesktopMenus(RegulusGUI window)
	  {
		  regulusWindow = window;
		  
		  // create Start New Second Window menu item
		  
		  regulusWindow.WindowMenu = new JMenu("New Window");
		  regulusWindow.WindowMenu.setMnemonic( 'W');
		    
		  regulusWindow.Start_New_Second_Window_startMenuItem = new JMenuItem("Start second Window");
		  regulusWindow.Start_New_Second_Window_startMenuItem.setToolTipText("Start second new Regulus Window");
		  regulusWindow.Start_New_Second_Window_startMenuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				regulusWindow.runRegulusSecond();
		      			}
		    		}
		    		);
		  regulusWindow.WindowMenu.add(regulusWindow.Start_New_Second_Window_startMenuItem);
		   
	//  create Start New Third Window menu item
		  
	  regulusWindow.Start_New_Third_Window_startMenuItem = new JMenuItem("Start third Window");
	  regulusWindow.Start_New_Third_Window_startMenuItem.setToolTipText("Start third new Regulus Window");
	  regulusWindow.Start_New_Third_Window_startMenuItem.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
	    				regulusWindow.runRegulusThird();
	      			}
	    		}
	    		);
	  regulusWindow.WindowMenu.add(regulusWindow.Start_New_Third_Window_startMenuItem);
	   
//	  create Start New Fourth Window menu item
	  
	  {
		  regulusWindow.Start_New_Fourth_Window_startMenuItem = new JMenuItem("Start fourth Window");
		  regulusWindow.Start_New_Fourth_Window_startMenuItem.setToolTipText("Start fourth new Regulus Window");
		  regulusWindow.Start_New_Fourth_Window_startMenuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				regulusWindow.runRegulusFourth();
		      			}
		    		}
		    		);
		  regulusWindow.WindowMenu.add(regulusWindow.Start_New_Fourth_Window_startMenuItem);
	  }
	  }
}

