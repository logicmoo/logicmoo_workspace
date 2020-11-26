package RegulusGUI;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;

import javax.swing.tree.DefaultMutableTreeNode;
import se.sics.prologbeans.PBString;



import java.io.*;

public class CorpusInBackGround extends Thread {
	
	private RegulusGUI regulusWindow = null;
	private Frame3 frame3 = null;
	private String CorpusString = "";
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	set the pointer to the Frame3 window
	public void setFrame3(Frame3 window) {
		  frame3 = window;
	}
	public CorpusInBackGround()
	{
		
	}
	public CorpusInBackGround(Frame3 frame, RegulusGUI regulusgui,String strCorpus)
	{
		setFrame3(frame);
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		CorpusString = strCorpus;
	}

	public void run() {
			try
			{
				System.out.println("I am doing things in batch");
				//regulusWindow.handleCommand("TRANSLATE_CORPUS");
				regulusWindow.handleCommand(CorpusString);
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
	
	}


