package RegulusGUI;

import java.awt.BorderLayout;

import javax.swing.JInternalFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

public class LoadErrorPane {
	private JInternalFrame loaderrorpane = null;
	private RegulusGUI regulusWindow = null;
	private Frame2     frame2       = null;
	private Frame3     frame3       = null;
	private CommandInBackGround commandinbackground = null;
	private RegulusMenus  regulusmenus = null;
	private  JTextArea text;
	private int getSum = 0;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
	
	 // send name of internal frame 
	
	  public JInternalFrame getInternalFrame() {
		  return loaderrorpane;
	  }
	  
//	  get the pointer to frame2 window
	  public Frame2 getFrame2() {
		  return frame2;
	  }
	  
//	 set the pointer to the Frame2 window
	  public void setFrame2(Frame2 window) {
		  frame2 = window;
	  }
	  
//	  get the pointer to frame3 window
	  public Frame3 getFrame3() {
		  return frame3;
	  }
	  
//	 set the pointer to the Frame2 window
	  public void setFrame3(Frame3 window) {
		  frame3 = window;
	  }
	  
//	  get the pointer to CommandInBackGround window
	  public CommandInBackGround getCommandInBackGround() {
		  return commandinbackground ;
	  }
	  
//	 set the pointer to the CommandInBackGround window
	  public void setCommandInBackGround(CommandInBackGround Commandwindow) {
		  commandinbackground = Commandwindow;
	  }
	  
//	  get the pointer to RegulusMenus window
	  public RegulusMenus  getRegulusMenus () {
		  return regulusmenus  ;
	  }
	  
//	 set the pointer to the RegulusMenus  window
	  public void setRegulusMenus(RegulusMenus Menuwindow) {
		  regulusmenus = Menuwindow;
	  }
	  
  public LoadErrorPane() {
		  
	  }
	  
	public LoadErrorPane(CommandInBackGround frame,RegulusMenus MenuFrame ,RegulusGUI regulusgui,int lines) {
		
		loaderrorpane = new JInternalFrame("LoadError",true,true,true,true);
		setCommandInBackGround(frame);
		setRegulusMenus(MenuFrame);
		setRegulusGUI(regulusgui);
		getSum = lines;
		String command = regulusWindow.getCommandErrorString();	
		//System.out.println("getSum "+getSum);
		text = new JTextArea(40, getSum);
		text.append(command);
		loaderrorpane.add(new JScrollPane(text));
		 
		loaderrorpane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		loaderrorpane.setLocation(200,150);
			
		loaderrorpane.setSize(600,400);
		
		  
	}
	
public LoadErrorPane(Frame2 frame, RegulusGUI regulusgui,int lines) {
		
		loaderrorpane = new JInternalFrame("LoadError",true,true,true,true);
		setFrame2(frame);
		setRegulusGUI(regulusgui);
		String command = regulusWindow.getCommandErrorString();	
		text = new JTextArea(40, 20);
		text.append(command);
		loaderrorpane.add(new JScrollPane(text));
		 
		loaderrorpane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		loaderrorpane.setLocation(200,150);
			
		loaderrorpane.setSize(600,400);
		}

public LoadErrorPane(Frame3 frame, RegulusGUI regulusgui) {
	
	loaderrorpane = new JInternalFrame("LoadError",true,true,true,true);
	setFrame3(frame);
	setRegulusGUI(regulusgui);
	String command = regulusWindow.getCommandErrorString();	
	text = new JTextArea(40, 20);
	text.append(command);
	loaderrorpane.add(new JScrollPane(text));
	 
	loaderrorpane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	loaderrorpane.setLocation(200,150);
		
	loaderrorpane.setSize(600,400);
	}

	}
	

