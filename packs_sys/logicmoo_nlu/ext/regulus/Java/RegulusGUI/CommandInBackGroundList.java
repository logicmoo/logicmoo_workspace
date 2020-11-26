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
import javax.swing.WindowConstants;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

public class CommandInBackGroundList extends Thread  {
	
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
	
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
	

	public CommandInBackGroundList()
	{
		
	}
	
	public CommandInBackGroundList (RegulusGUI regulusgui)
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
			while (regulusWindow.Commands_In_Back_Ground[j] != null)
			{ 
				if (this_is_first_time_in)
				{
					CreateAndLinkprogress();
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
					//checkLinesEnding();
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
		 public void CreateAndLinkprogress()
		 {
			 oldrecCount = nLineTable[nLineIndex];
			 progressListPane progresslistpane = new progressListPane();
			 JInternalFrame progressListPaneInternalFrame = progresslistpane.getInternalFrame();
			 regulusWindow.desktop.add(progressListPaneInternalFrame);
			 progressListPaneInternalFrame.setVisible(true);
		 }
		 
	
		class readFile extends Thread {
			public readFile()
			{
			recCounteachfile = 0;
			RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CommandString);
			progressFile = infoArray[0].getFile();
			System.out.println("progressFile "+progressFile);
			nLines = infoArray[0].getNumberOfLines();
			System.out.println("nLines "+nLines);
			partialmax = nLines;
			
			}
			public void run() {
			 try {
				 br = new BufferedReader(new FileReader(progressFile));
			while ((record = br.readLine())!= null) {
				recCount++;
				recCounteachfile++;
				new Thread(new thread1()).start(); // start the first thread 
		   		new Thread(new thread2()).start(); // start the second thread 
		   		checkLines();
		   		CreateAndLinkprogress();
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
		
		private void checkLinesEnding()
		{
			if (recCount < sumLines)
			{
				System.out.println("do  something");
				recCount = sumLines;
				new Thread(new thread1()).start(); // start the first thread 
		   		new Thread(new thread2()).start(); // start the second thread 
		   		checkLines();
		   		CreateAndLinkprogress();
			}
			else 
			{
				System.out.println("do  nothing");
			}
		}
		private void checkLines()
		{
			System.out.println("recCount in checkLines "+recCount);
			System.out.println("sumLines in checkLines "+sumLines);
			if (sumLines == recCount)
			{
				System.out.println("I am setting switch");
				file_closed = true;
			}
		}
	
		
//		The Thread class handling progressbar for both files
		public class thread1 implements Runnable{
			public void run(){
				
				progressbarfull.setValue(recCount);
				progressbarfull.repaint();
				//System.out.println("recCount "+recCount);
				try { Thread.sleep(10);
				}
					catch (InterruptedException err){}
				}
			
			}
//		The Thread2 class handling progressbar for each individul heavy job
		public class thread2 implements Runnable{
			public void run(){
				progressbarpartial.setValue(recCounteachfile);
				progressbarpartial.repaint();
				//System.out.println("recCounteachfile "+recCounteachfile);
				try { Thread.sleep(10); }
					catch (InterruptedException err){}
				}
			
			}
		class progressListPane extends Thread implements  VetoableChangeListener{
			private JInternalFrame progresslistpane = null;
			 // send name of internal frame 
			
			  public JInternalFrame getInternalFrame() {
				  return progresslistpane;
			  }
			  public progressListPane() {
				  progresslistpane = new JInternalFrame("Progress",true,true,true,true);
				  progresslistpane.setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
				  
				  progressbarpartial = new JProgressBar(0,partialmax);
				  
				  progressbarfull = new JProgressBar(0,sumLines);
				  
				  progressbarfull.setStringPainted(true);
				  progressbarpartial.setStringPainted(true);
				  
				  Container c2 = progresslistpane.getContentPane();
					
					out = new JTextArea(4,28);
					out.setMargin(new Insets(5,5,5,5));
					out.setEditable(false);
					out.append("Creating and Reading ConfigurationFiles please be patient"+"\n" );	
					c2.add(progressbarfull, BorderLayout.PAGE_START);
					c2.add(progressbarpartial,BorderLayout.PAGE_END);
					c2.add(out,BorderLayout.CENTER );
					progresslistpane.addVetoableChangeListener(this);
					progresslistpane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
					progresslistpane.setLocation(250,250);
					progresslistpane.setSize(40,40);
					progresslistpane.pack();
					if (file_closed)
					{
						file_closed = false;
						acount = aPerformed();
						closeFrame(acount);
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
		}


