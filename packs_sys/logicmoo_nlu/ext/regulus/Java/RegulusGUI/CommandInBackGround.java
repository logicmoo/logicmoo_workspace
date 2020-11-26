package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.IOException;

import javax.swing.JInternalFrame;

public class CommandInBackGround extends Thread {

	private RegulusGUI regulusWindow = null;
	private RegulusMenus  regulusmenus = null;
	private String CommandString = "";
	private boolean thisIsRemoteJob = false;
	private String command = "";
	private int lineCounter = 0;
	private int sum = 0;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
	
//	get pointer to RegulusMenus window
	public  RegulusMenus getRegulusMenus() {
		  return regulusmenus;
	}
	
//	 set the pointer to the RegulusMenus window
	public void setRegulusMenus(RegulusMenus Menuwindow) {
		 regulusmenus  = Menuwindow;
	}
	
	public CommandInBackGround()
	{
		
	}
	
	public CommandInBackGround(RegulusGUI regulusgui,String strCommand)
	{
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		CommandString = strCommand;
	}
	
	public CommandInBackGround(RegulusGUI regulusgui,String strCommand,boolean jobIsRemote)
	{
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		CommandString = strCommand;
		thisIsRemoteJob = jobIsRemote;
		
	}
	public void run() {
		try
		{
			regulusWindow.handleCommand(CommandString,thisIsRemoteJob);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	
	EventQueue.invokeLater( new Runnable () {
		public void run () {
			//System.out.println("done");
				executeCommandFromMenuInBackGround(); 
		}
	});
 }
	
	public void executeCommandFromMenuInBackGround()
	{
		 if (CommandString.equals("EBL_LOAD_GENERATION"))
			{
			regulusWindow.InputText = "Ebl Load Generation command succeeded";
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD_GENERATION"))
			{
			regulusWindow.InputText = "Load Generation command succeeded";
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD"))
			{
			regulusWindow.InputText = "Load command succeeded";
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD_TRANSLATE"))
			{
			regulusWindow.InputText = "Load Translate command succeeded";
			sendMessageLoad();
			}
		 if (CommandString.equals("EBL_LOAD"))
			{
			regulusWindow.InputText = "Ebl Load command succeeded";
			sendMessageLoad();
			}
	}
	 public void sendMessageLoad(){
		
			if (regulusWindow.regulus_command_succeeded)
			{ 
				regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
			}
			else
			{
				command = regulusWindow.getCommandErrorString();
				countLines();
				createLoadErrorWindow();
			}
				checkMenuStatus(); 
	 }
	
	public void countLines() {
		 
		  int len = command.length();
		  if (len > 1000)
		  {
			  System.out.println("len "+len);
			  sum = len / 100;
			  System.out.println("sum "+sum);
		  }
		  else if (len > 100)
		  {
			  System.out.println("len "+len);
			  sum = len / 100;
			  System.out.println("sum "+sum); 
		  }
		  else 
		  {
			 sum = len; 
		  }
			
	}
	 public void createLoadErrorWindow() {
			  LoadErrorPane loaderrorpane = new LoadErrorPane(this,getRegulusMenus(), getRegulusGUI(),sum);
			  JInternalFrame LoadErrorPaneInternalFrame = loaderrorpane.getInternalFrame();
			  regulusWindow.desktop.add(LoadErrorPaneInternalFrame);
			  LoadErrorPaneInternalFrame.setVisible(true);
		  }
	 
	 public void checkMenuStatus()
	  {
		regulusWindow.updateCommandStatus();
		regulusWindow.availablemenus.check_available_menus();
		regulusWindow.unavailablecommands.check_unavailable_menus();
	  }
	
}
			


	
		
	
	