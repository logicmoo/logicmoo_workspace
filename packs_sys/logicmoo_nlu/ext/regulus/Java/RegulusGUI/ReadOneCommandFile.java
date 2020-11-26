package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import se.sics.prologbeans.*;
import java.io.*;
import javax.swing.*;

public class ReadOneCommandFile extends Thread{

	private RegulusGUI regulusWindow = null;
	private CommandInBackGroundList commandinbackgroundlist = null;
	private String CommandString = "";
	private String record = null;
	private Integer recCount = 1;
	private int  nLines = 0;
	private String progressFile = "";
	private ProgressMonitor monitor;
	private boolean thisIsRemoteJob = false;
	private boolean jobWillRunInBackGround = false;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	get pointer to CommandInBackGroundList 	
	public CommandInBackGroundList getCommandInBackGroundList() {
		return  commandinbackgroundlist;
	}
//	set pointer to CommandInBackGroundList 
	public void setCommandInBackGroundList(CommandInBackGroundList commandInBackGround) {
		 commandinbackgroundlist = commandInBackGround;
	}
	
	public ReadOneCommandFile()
	{
		
	}
	
	public ReadOneCommandFile(RegulusGUI regulusgui, String strCommand)
	{
		
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
	
		CommandString = strCommand;
		
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CommandString);
		progressFile = infoArray[0].getFile();
		nLines = infoArray[0].getNumberOfLines();
		System.out.println("nLines "+nLines);
	}
	public  ReadOneCommandFile(RegulusGUI regulusgui,CommandInBackGroundList commandinback,
			String strCommand,boolean jobIsRemote,boolean jobRunInBackGround)
	{
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		setCommandInBackGroundList(commandinback);
		commandinbackgroundlist = commandinback;
		CommandString = strCommand;
		
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CommandString);
		progressFile = infoArray[0].getFile();
		//System.out.println("progressFile "+progressFile);
		//nLines = infoArray[0].getNumberOfLines();
		System.out.println("nLines "+nLines);
		thisIsRemoteJob = jobIsRemote;
		jobWillRunInBackGround = jobRunInBackGround;
		regulusWindow.execution_is_ready = false;
	}
	public void run(){
		 try {
			 FileInputStream fileIn = new FileInputStream(progressFile);
            ProgressMonitorInputStream progressIn
            = new ProgressMonitorInputStream(null,
                  "Reading textfile" , fileIn);
            InputStreamReader inReader
               = new InputStreamReader(progressIn);
            BufferedReader in = new BufferedReader(inReader);
            monitor = new ProgressMonitor(null,"monitoring progress",null,0,nLines);
		while ((record = in.readLine())!= null) {
			//System.out.println("record "+record);
			recCount++;
			monitor.setProgress(recCount);
			try{
				sleep(100);
			}
			catch (InterruptedException e){
			}
			}
		in.close();
		EventQueue.invokeLater( new Runnable () {
			public void run () {
				//System.out.println("done");
					// executeCommandFromMenuInBackGround(); 
					}
			});
		 }
		 catch (IOException e ) {
		 }
	}
	public void executeCommandFromMenuInBackGround()
	{
		 if (CommandString.equals("EBL_LOAD_GENERATION"))
			{
			regulusWindow.InputText = "Ebl Load Generation command succeeded";
			regulusWindow.regulus_command_succeeded = true;
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD_GENERATION"))
			{
			 System.out.println(" I am here");
			regulusWindow.InputText = "Load Generation command succeeded";
			regulusWindow.regulus_command_succeeded = true;
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD"))
			{
			regulusWindow.InputText = "Load command succeeded";
			regulusWindow.regulus_command_succeeded = true;
			sendMessageLoad();
			}
		 if (CommandString.equals("LOAD_TRANSLATE"))
			{
			regulusWindow.InputText = "Load Translate command succeeded";
			regulusWindow.regulus_command_succeeded = true;
			sendMessageLoad();
			}
		 if (CommandString.equals("EBL_LOAD"))
			{
			regulusWindow.InputText = "Ebl Load command succeeded";
			regulusWindow.regulus_command_succeeded = true;
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
				String command = regulusWindow.getCommandErrorString();	
				regulusWindow.InputText = command;
				regulusWindow.txtBoxDisplayNegative(regulusWindow.InputText);
			}
				checkMenuStatus(); 
	 }
	 public void checkMenuStatus()
	  {
		regulusWindow.updateCommandStatus();
		regulusWindow.availablemenus.check_available_menus();
		regulusWindow.unavailablecommands.check_unavailable_menus();
	  }
	
}