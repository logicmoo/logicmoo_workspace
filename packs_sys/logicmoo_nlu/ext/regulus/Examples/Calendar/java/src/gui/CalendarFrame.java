package gui;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JToolBar;

@SuppressWarnings("serial")
class CalendarFrame extends JFrame implements ActionListener{
	
	private Calendar calendarRootComponent;
	
	// get language index for Japanese or English
	int japLangIdx, enLangIdx;

	
	private JMenuBar calendarMenuBar = null;

	private JMenu jMenuView = null;
	private JCheckBoxMenuItem viewNBest = null;
	private JCheckBoxMenuItem viewHistory = null;
	
	private javax.swing.JMenu jMenuLanguage = null;
	private javax.swing.ButtonGroup jButtonGroupLanguage = null ;
	private javax.swing.JRadioButtonMenuItem enLang = null;
	private javax.swing.JRadioButtonMenuItem japLang = null;
	
	private JMenu jMenuHelp = null;
	private JMenuItem jMenuItemHelpAbout = null;

	/**
	 * This method initializes 
	 * 
	 */
	public CalendarFrame() {
		super();
		initialize();
		japLangIdx = calendarRootComponent.getApplicationState().getLanguage().getLanguageIndex("Japanese");
		enLangIdx = calendarRootComponent.getApplicationState().getLanguage().getLanguageIndex("English");

	}
	
	/**
	 * This method initializes 
	 * 
	 */
	public CalendarFrame(Calendar rootComp, String frameTitle) {
		super(frameTitle);
		calendarRootComponent = rootComp;
		 
		initialize();
		japLangIdx = calendarRootComponent.getApplicationState().getLanguage().getLanguageIndex("Japanese");
		enLangIdx = calendarRootComponent.getApplicationState().getLanguage().getLanguageIndex("English");

	}

	/**
	 * This method initializes this
	 * 
	 */
	private void initialize() {

        this.setSize(new Dimension(524, 488));
		// Create the menu bar
        this.setTitle("Calendar Regulus Application");
        this.setJMenuBar(getCalendarMenuBar());			
	}

	public void actionPerformed( ActionEvent event )
	{
		System.out.println( event );
	}

	/**
	 * This method initializes calendarMenuBar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getCalendarMenuBar() {
		if (calendarMenuBar == null) {
			calendarMenuBar = new JMenuBar();
			
			calendarMenuBar.add(getJMenuView());
			calendarMenuBar.add(getJMenuLanguage());
			calendarMenuBar.add(getJMenuHelp());
		}
		return calendarMenuBar;
	}

	/**
	 * This method initializes jMenuView	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getJMenuView() {
		if (jMenuView == null) {
			jMenuView = new JMenu();
			jMenuView.setText("View");
			jMenuView.add(getViewHistory());
			jMenuView.add(getViewNBest());
			jMenuView.addSeparator();
		}
		return jMenuView;
	}
	
	
	/**
	 * This method initializes jMenuLanguage	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getJMenuLanguage() {
		if (jMenuLanguage == null) {
			jMenuLanguage = new JMenu();
			jMenuLanguage.setText("Language");
			
			enLang = getEnglishRadioButton();
			japLang = getJapaneseRadioButton();
			
			// jMenuLanguage.addSeparator();
			
			jButtonGroupLanguage = getJButtonGroupLanguage();
			jMenuLanguage.add(enLang);
			jMenuLanguage.add(japLang);
			
		}
		return jMenuLanguage;
	}

	/**
	 * This method initializes jMenuLanguage	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private javax.swing.ButtonGroup getJButtonGroupLanguage() {
		if ( jButtonGroupLanguage == null){
			jButtonGroupLanguage = new ButtonGroup();
			jButtonGroupLanguage.add(getEnglishRadioButton());
			jButtonGroupLanguage.add(getJapaneseRadioButton());
		};
		return jButtonGroupLanguage;
	}
	
	/**
	 * This method initializes communication in English   	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButtonMenuItem getEnglishRadioButton() {
		if (enLang == null) {
			enLang = new JRadioButtonMenuItem();
			enLang.setText("English");
			String language = calendarRootComponent.getApplicationState().getLanguage().getLanguageName();
			System.out.println("Language selected : " + language);
			
			if (language.contains("English")){				
				enLang.setSelected(true);
				System.out.println("English = true ");
			}
			else
				enLang.setSelected(false);
			enLang.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
			    	// TODO 
					calendarRootComponent.reconfigureToChangeLanguage(enLangIdx);

					calendarRootComponent.validateCalendarPanel();
			    	calendarRootComponent.repaintCalendarPanel();
				}
			});
		}
		return enLang;
	}
	
	/**
	 * This method initializes communication in Japanese   	
	 * 	
	 * @return javax.swing.JRadioButton	
	 */
	private JRadioButtonMenuItem getJapaneseRadioButton() {
		if (japLang == null) {
			japLang = new JRadioButtonMenuItem();
			japLang.setText("Japanese");
			// get old language name
			String language = calendarRootComponent.getApplicationState().getLanguage().getLanguageName();
			System.out.println("Language selected : " + language);
			if (language.contains("Japanese"))
				japLang.setSelected(true);
			else
				japLang.setSelected(false);
			japLang.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					calendarRootComponent.reconfigureToChangeLanguage(japLangIdx);
			    	calendarRootComponent.validateCalendarPanel();
			    	calendarRootComponent.repaintCalendarPanel();
				}
			});
		}
		return japLang;
	}
	
	/**
	 * This method initializes viewNBest	
	 * 	
	 * @return javax.swing.JCheckBoxMenuItem	
	 */
	private JCheckBoxMenuItem getViewNBest() {
		if (viewNBest == null) {
			viewNBest = new JCheckBoxMenuItem();
			viewNBest.setText("Raw Recognition Result");
			viewNBest.setMnemonic(KeyEvent.VK_N);
			viewNBest.setSelected(true);
			viewNBest.setVisible(true);
			viewNBest.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
			    	calendarRootComponent.setNBestShowing(viewNBest.isSelected());
			    	calendarRootComponent.validateCalendarPanel();
			    	calendarRootComponent.repaintCalendarPanel();
			    	
				}
			});
		}
		return viewNBest;
	}

	/**
	 * This method initializes viewHistory	
	 * 	
	 * @return javax.swing.JCheckBoxMenuItem	
	 */
	private JCheckBoxMenuItem getViewHistory() {
		if (viewHistory == null) {
			viewHistory = new JCheckBoxMenuItem();
			viewHistory.setMnemonic(KeyEvent.VK_H);
			viewHistory.setText("Show Dialogue History");
			viewHistory.setSelected(true);
			viewHistory.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
			    	calendarRootComponent.setHistoryShowing(viewHistory.isSelected());
			    	calendarRootComponent.validateCalendarPanel();
			    	calendarRootComponent.repaintCalendarPanel();
				}
			});
		}
		return viewHistory;
	}

	/**
	 * This method initializes jMenuHelp	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getJMenuHelp() {
		if (jMenuHelp == null) {
			jMenuHelp = new JMenu();
			jMenuHelp.setText("Help");
			jMenuHelp.setMnemonic(KeyEvent.VK_HELP);
			jMenuHelp.add(getJMenuItemHelpAbout());
			jMenuHelp.addSeparator();
		}
		return jMenuHelp;
	}

	/**
	 * This method initializes jMenuItemHelpAbout	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getJMenuItemHelpAbout() {
		if (jMenuItemHelpAbout == null) {
			jMenuItemHelpAbout = new JMenuItem();
			jMenuItemHelpAbout.setText("About Calendar Application ");
		}
		return jMenuItemHelpAbout;
	}

	public static void main(String args[]){
		// createSwingComponents();
		CalendarFrame calendarFrame = new CalendarFrame();	
		calendarFrame.setVisible(true);
	}

}  //  @jve:decl-index=0:visual-constraint="10,10"
