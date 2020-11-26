package RegulusGUI;
import java.awt.*;
import java.io.BufferedReader;
import java.io.*;
import java.io.FileReader;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.JTextArea;
import javax.swing.ProgressMonitor;
import javax.swing.WindowConstants;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

public class ReadManyCommands extends Thread {

	private RegulusGUI regulusWindow = null;
	private String CommandString = "";
	private boolean thisIsRemoteJob = false;
	private boolean command_is_finnished = false;
	private int  nLines = 0;
	public  int sumLines = 0;
	private String progressFile = "";
	private boolean load_continuing = false;
	private BufferedReader br;
	private String record = null;
	public Integer recCount = 1;
	public Integer oldrecCount = 0;
	public Integer recCounteachfile = 1;
	public Integer saverecCounteachfile = 1;
	public JProgressBar progressbar;
	private int min = 0;
	private int max = 0;
	private int[] nLineTable = new int[10];
	private int nLineIndex = 0;
	private JProgressBar progressbarfull;
	private JProgressBar progressbarpartial;
	private JTextArea out;
	private  int partialmax = 0;
	public JInternalFrame[] allFrames = null;
	private int count = 0;
	private static int acount = 0;
	private boolean file_closed = false;
	private ProgressMonitor monitor;
	
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
	

	public ReadManyCommands()
	{
		
	}
	
	public ReadManyCommands (RegulusGUI regulusgui)
	{
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		
		// prepare data for progressbar
		readCommandFileInfo();
		
	}
	
	public void readCommandFileInfo()
	{
		nLineIndex = 0;
		load_continuing = false;
		int j = 0;
		while (regulusWindow.Commands_In_Back_Ground[j] != null)
		{ 
			CommandString = regulusWindow.Commands_In_Back_Ground[j];
			checkIfLoad();
			j++;
		}
	
	}
	public void checkIfLoad()
	{
		String b = "EBL_LOAD";
		String c = "LOAD_TRANSLATE";
		String d = "LOAD";
		if (CommandString.indexOf(b) != -1) {
			getFileInfo();
			load_continuing = true;
			sumLines = sumLines + nLines;
			nLineTable[nLineIndex] = nLines;
			nLineIndex++;
			}
			else if (CommandString.indexOf(c) != -1) {
				getFileInfo();
				load_continuing = true;
				sumLines = sumLines + nLines;
				nLineTable[nLineIndex] = nLines;
				nLineIndex++;
			}
			else if (CommandString.indexOf(d) != -1) {
				getFileInfo();
				load_continuing = true;
				sumLines = sumLines + nLines;
				nLineTable[nLineIndex] = nLines;
				nLineIndex++;
			}
			
	}
	public void getFileInfo()
	{
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CommandString);
		progressFile = infoArray[0].getFile();
		nLines = infoArray[0].getNumberOfLines();
		
	}
	
		public void run() {
		recCount = 0;
		int j = 0;
		load_continuing = true;
		nLineIndex = 0;
		boolean this_is_first_time_in = true;
		 monitor = new ProgressMonitor(null,"monitoring progress",null,0,sumLines );
			while (regulusWindow.Commands_In_Back_Ground[j] != null)
			{ 
				if (this_is_first_time_in)
				{
					monitor.setProgress(recCount);
					this_is_first_time_in = false;
				}
				CommandString = regulusWindow.Commands_In_Back_Ground[j];
				thisIsRemoteJob =  regulusWindow.True_Or_False[j];
				checkCommandString();
				j++;
				nLineIndex++;
			}
			EventQueue.invokeLater( new Runnable () {
				public void run () {
					load_continuing = false;
					JOptionPane.showMessageDialog(null, "Loading finished"
								,"Load Information",JOptionPane.INFORMATION_MESSAGE);
					}
				});
		}
	
	
		private void checkCommandString()
		{
			String b = "EBL_LOAD";
			String c = "LOAD_TRANSLATE";
			String d = "LOAD";
			try {
				
			if (CommandString.indexOf(b) != -1) {
			load_continuing = true;
			putMessageLoad(); 
			
			}
			else if (CommandString.indexOf(c) != -1) {
				load_continuing = true;
				putMessageLoad(); 
			}
			else if (CommandString.indexOf(d) != -1) {
				load_continuing = true;
				putMessageLoad(); 
			}
			else {
				load_continuing = false;
				putMessageNotLoad();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		}
		
	   private synchronized void putMessageLoad() 
	   throws InterruptedException
       { 
		 regulusWindow.handleCommand(CommandString,thisIsRemoteJob);
		 boolean command_run_in_Background = true; 
		 try {
	   			sleep(1000);
	   		}
	   		catch (InterruptedException e){
		}
		// System.out.println(" CommandString in heavy "+CommandString);
		// System.out.println("thisIsRemoteJob in heavy "+thisIsRemoteJob);
		 new readFile().start();
       } 
	   private void putMessageNotLoad() 
	   throws InterruptedException
       { 
		   regulusWindow.handleCommand(CommandString,thisIsRemoteJob); 
		 //  System.out.println(" CommandString in light "+CommandString);
		  // System.out.println("thisIsRemoteJob in light "+thisIsRemoteJob);
       }
		
		 
	
		class readFile extends Thread {
			public readFile()
			{
			recCounteachfile = 0;
			RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CommandString);
			progressFile = infoArray[0].getFile();
			nLines = infoArray[0].getNumberOfLines();
			partialmax = nLines;
			
			}
			public void run() {
			 try {
				 br = new BufferedReader(new FileReader(progressFile));
			while ((record = br.readLine())!= null) {
				recCount++;
				recCounteachfile++;
				monitor.setProgress(recCount);
			
		   		try {
		   			sleep(50);
		   		}
		   		catch (InterruptedException e){
			}
			}
			br.close();
			regulusWindow.deleteRegulusFile(progressFile);
			 }
			 catch (IOException e ) {
			 }
			}
		}
		
		
		
		
			  public int aPerformed() {
				   // How many frames do we have
				     allFrames = regulusWindow.desktop.getAllFrames();
				    count = allFrames.length;
				    return count;
				     }
				   
				   public void closeFrame(int count){
				   	  // Iterate ower the frames, closing them if they are open
				   	  		count = count - 1;
				   		  for (int closej = 0; closej < count; closej++ ){
				   			  JInternalFrame f = allFrames[closej];
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
				   
				   	JInternalFrame progresslistpane = (JInternalFrame) event.getSource();
				   	String name = event.getPropertyName();
				   	Object value = event.getNewValue();
				   	int count = 0;
				   
				   	// we only want to check attempts to close a frame
				   
				   	if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
				   		int result = JOptionPane.showInternalConfirmDialog(progresslistpane,
				   				" OK to close?");
				   		// if the user doesn't agree veto the close
				   		if (result == JOptionPane.CANCEL_OPTION)
				   			throw new PropertyVetoException("User cancelled close", event);
				   		else
				   		count = aPerformed();
				   
				   		progresslistpane.setVisible(false);
				   		progresslistpane.dispose();
				   		return;
				   		}
				     }
				   }
		




