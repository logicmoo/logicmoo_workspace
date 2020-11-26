package RegulusGUI;

import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class SpecialisationMenu extends JFrame
{
	private RegulusGUI regulusWindow = null;
	private String InputText = "";
	  
	  public SpecialisationMenu(){
		  
	  }
	  
	  public SpecialisationMenu(RegulusGUI window)
	  {
		  regulusWindow = window;
		  
		 
		  //  create Specialised  menu item and sub menu item
		  regulusWindow.specialised_Menu = new JMenu( "Specialisation" );
		  regulusWindow.specialised_Menu.setMnemonic( 'S');
		  regulusWindow.Ebl_menuItem = new JMenuItem( "Ebl" );
		  regulusWindow.Ebl_menuItem.setToolTipText("Do main EBL Processing");
		  regulusWindow.Ebl_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				checkMenuStatus();
		  	
		  				}  	
		        	}
		        );	
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_menuItem);
		  
//		  create Ebl_Train sub menu item to Specialised
		    
		  regulusWindow.Ebl_Train_menuItem = new JMenuItem( "Ebl Train" );
		  regulusWindow.Ebl_Train_menuItem.setToolTipText("Do EBL training on current treebank");
		  regulusWindow.Ebl_Train_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
	    				regulusWindow.handleCommand("EBL_TRAIN");
	    				checkMenuStatus();
		    				
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Train_menuItem);
		   
//		  create Ebl_Analysis sub menu item to Specialised
		    
		  regulusWindow.Ebl_Analysis_menuItem = new JMenuItem( "Ebl Analysis" );
		  regulusWindow.Ebl_Analysis_menuItem.setToolTipText("Do main EBL Processing ");
		  regulusWindow.Ebl_Analysis_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		   				regulusWindow.handleCommand("EBL_ANALYSIS");
		   				checkMenuStatus();
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Analysis_menuItem);
		   
//		  create Ebl_Gemini sub menu item to Specialised
		    
		  regulusWindow.Ebl_Gemini_menuItem = new JMenuItem( "Ebl Gemini" );
		  regulusWindow.Ebl_Gemini_menuItem.setToolTipText("Compile current specialised Regulus grammar into Gemini form ");
		  regulusWindow.Ebl_Gemini_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		   				regulusWindow.handleCommand("EBL_GEMINI");
		   				checkMenuStatus();
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Gemini_menuItem);
		  
		  //  create Ebl_Generation sub menu item to Specialised
		    
		  regulusWindow.Ebl_Generation_menuItem = new JMenuItem( "Ebl Generation" );
		  regulusWindow.Ebl_Generation_menuItem.setToolTipText("Do main generation EBL processing ");
		  regulusWindow.Ebl_Generation_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
	    				regulusWindow.handleCommand("EBL_GENERATION");
	    				checkMenuStatus();
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Generation_menuItem);
		  
//		  create Ebl_Treebank sub menu item to Specialised
		    
		  regulusWindow.Ebl_Treebank_menuItem = new JMenuItem( "Ebl Treebank" );
		  regulusWindow.Ebl_Treebank_menuItem.setToolTipText("Parse all sentences in current EBL training set into treebank form ");
		  regulusWindow.Ebl_Treebank_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				
		    				 regulusWindow.handleCommand("EBL_TREEBANK");
		    				 checkMenuStatus();
		    				
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add( regulusWindow.Ebl_Treebank_menuItem);
		  

		    
//		  create Ebl_Grammar_Probs sub menu item to Specialised
		    
		  regulusWindow.Ebl_Grammar_Probs_menuItem = new JMenuItem( "Ebl Grammar Probs" );
		  regulusWindow.Ebl_Grammar_Probs_menuItem.setToolTipText("Create Nuance grammar probs training set from current EBL training set ");
		  regulusWindow.Ebl_Grammar_Probs_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				regulusWindow.handleCommand("EBL_GRAMMAR_PROBS");
		    				checkMenuStatus();
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Grammar_Probs_menuItem);

		    
//		  create Ebl_Postprocess sub menu item to Specialised
		    
		  regulusWindow.Ebl_Postprocess_menuItem = new JMenuItem( "Ebl Postprocess" );
		  regulusWindow.Ebl_Postprocess_menuItem.setToolTipText("Postprocess results of EBL training into specialised Regulus grammar ");
		  regulusWindow.Ebl_Postprocess_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				regulusWindow.handleCommand("EBL_POSTPROCESS");
		    				checkMenuStatus();
		    				
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Postprocess_menuItem);
		  

		    
//		  create Ebl_Nuance sub menu item to Specialised
		    
		  regulusWindow.Ebl_Nuance_menuItem = new JMenuItem( "Ebl Nuance" );
		  regulusWindow.Ebl_Nuance_menuItem.setToolTipText("Compile current specialised Regulus grammar into Nuance GSL form");
		  regulusWindow.Ebl_Nuance_menuItem.addActionListener(
		    		new ActionListener() {
		    			public void actionPerformed( ActionEvent e)
		    			{
		    				regulusWindow.handleCommand("EBL_NUANCE");
		    				checkMenuStatus();
		    				
		    			}
		    		}
		    		);
		  regulusWindow.specialised_Menu.add(regulusWindow.Ebl_Nuance_menuItem);
		   
	  }
	  public void checkMenuStatus()
	  {
		regulusWindow.updateCommandStatus();
		regulusWindow.availablemenus.check_available_menus();
		regulusWindow.unavailablecommands.check_unavailable_menus();
	  }
}
