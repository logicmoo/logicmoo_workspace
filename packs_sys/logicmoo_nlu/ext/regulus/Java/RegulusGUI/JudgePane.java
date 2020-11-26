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

public class JudgePane extends JFrame
				implements ActionListener, VetoableChangeListener, InternalFrameListener, MouseListener{
	
	private RegulusGUI regulusWindow = null;
	private CorpusPane corpuspane    = null;
	private JInternalFrame judgePane = null;
	private JMenuBar bar = new JMenuBar();  // create menubar
	private JLabel counter = new JLabel(" Record Counter");
	private JTextArea counterArea = new JTextArea(1,3);
	private JLabel source = new JLabel("Source");
	private JLabel target = new JLabel("Target");
	private JLabel context = new JLabel("Context");
	private JLabel judgment = new JLabel("Judgment");
	private JMenuItem HelpMenu;
	private String translationString = "";
	private int translationInt = 0;
	private TranslationResult translationresult;
	public  JButton next = new JButton("Next Record");
	private JButton previous = new JButton("Previous Record");
	private JButton unjudged = new JButton("Next Unjudged");
	private JButton save = new JButton("Save File");
	private JButton first = new JButton("First Record");
	private JButton last = new JButton("Last Record");
	public  JButton undo = new JButton("    Undo    ");
	public  JButton undoprevious = new JButton("Undo Prev Unjudged");
	private GridBagLayout gblayout;
	private GridBagConstraints gbConstraints;
	private JPanel displayPanel = new JPanel();
    private JPanel inputPanel = new JPanel(new BorderLayout());
    private final String[] DisplayLabelText = {"Source","Target","Context","Judgment"};
    private JScrollPane[] DisplayScrollPanes = new JScrollPane[4];
    private JLabel percentage = new JLabel("Percentage");
    private JLabel corpus  = new JLabel("Corpus Output");
    private JLabel files = new JLabel("Corpus Files");
    private JLabel DisplayLabels[] = {source,target,context,judgment };
    private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
    private JTextArea[] DisplayTextPanes = new JTextArea[4];
    private int index = 0;
    private int temp = 0;
    private String SourceString = "";
    private String TargetString = "";
    private String ContextString = "";
    private String JudgementString = "";
    private String PreviousJudgementString = "";
    private String PreviousUnjudgedJudgementString = "";
    private int batchIndex = 0;
    private int savebatchIndex = 0;
    private boolean unjudged_record_found = false;
    private JRadioButton good, ok ,bad, donotknow;
    private String InputText = "";
    private String Corpus= "";
    private String TranslateCorpusString = "TRANSLATE_CORPUS";
    private String TranslateSpeechCorpusString = "TRANSLATE_SPEECH_CORPUS";
    private String TranslateSpeechCorpusAgainString = "TRANSLATE_SPEECH_CORPUS_AGAIN";
    private int length = 0;
    private String ReadKey = "";
    private String SpaceString = " ";
    private String JudgeCommandPart1 = "UPDATE_TRANSLATION_JUDGEMENTS ";
    private String JudgeCommandFull = "";
    private String JudgeCommandSpeechPart1 = "UPDATE_TRANSLATION_JUDGEMENTS_SPEECH ";
    private String JudgeCommandSpeechFull = "";
    private int LastRecordIndex = 0;
    private int showWhereIndex = 0;
    private JMenuItem quit_system_MenuItem;
    private boolean   remove_finished = false;
    private Component c = null;
    private String strName = "";
    private JInternalFrame[] frames = null;
    private int frameIndex = 0;
    private int holdFrameIndex = 0;
    private String ac = "";
    private String bc = "";
    private String dc = "";
    private JButton Translatebtn =     new JButton("Translator ");
    private JButton Judgebtn =     new JButton("Judge ");
    private JButton debugbtn =     new JButton("Debugging Trace");
 	private int judgeCount = 0;
    
	
//	 send name of internal frame 
	public JInternalFrame getInternalFrame() {
		  return judgePane;
	}
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	set the pointer to the CorpusPane window
	public void setCorpus(CorpusPane window) {
		  corpuspane = window;
	}
	
	public JudgePane() {
		  
	}
	public JudgePane(JudgeMenu judgemenu, RegulusGUI regulusgui , String CorpusString,JButton judgebtn, int jdgCounter) {
		
		 setJMenuBar( bar );  // set the menubar for the JInternalFrame
		 
		judgePane = new JInternalFrame("Judge Corpus Data",true,true,true,true);
		setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		Container c2 = judgePane.getContentPane();
		setCorpus(corpuspane);
		setRegulusGUI(regulusgui);
		Corpus = CorpusString;
		Judgebtn = judgebtn;
		judgeCount = jdgCounter;
		Judgebtn.addMouseListener(this);
		CreateScreen();
		
		
		JPanel inputPanel = new JPanel(new BorderLayout());
		  
		  judgePane.addVetoableChangeListener(this);
		  inputPanel.add(bar,BorderLayout.NORTH);
		  c2.add(inputPanel,BorderLayout.NORTH  );
		  judgePane.addInternalFrameListener(this);
		  c2.add(displayPanel);	
		  judgePane.pack();
		  if (translationInt == 0)
		  {
			  System.out.println("translationInt is zero "+translationInt);
			 JOptionPane.showMessageDialog(null, "No records in file"
						,"Message for Judging",JOptionPane.INFORMATION_MESSAGE);
		  }
		  else
		  {
		 
		  }
		 
	}
	
	
	public JudgePane(CorpusPane corpuspane, RegulusGUI regulusgui , String CorpusString,JButton judgebtn, int jdgCounter) {
		
		  setJMenuBar( bar );  // set the menubar for the JInternalFrame	
		   
		  judgePane = new JInternalFrame("Judge Corpus Data",true,true,true,true);
		  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		  Container c2 = judgePane.getContentPane();
		  setCorpus(corpuspane);
		  setRegulusGUI(regulusgui);
		  Corpus = CorpusString;
		  Judgebtn = judgebtn;
		  judgeCount = jdgCounter;
		  Judgebtn.addMouseListener(this);
		  CreateScreen();
		  
		  JPanel inputPanel = new JPanel(new BorderLayout());
		  judgePane.addInternalFrameListener(this);
		  judgePane.addVetoableChangeListener(this);
		  inputPanel.add(bar,BorderLayout.NORTH);
		  c2.add(inputPanel,BorderLayout.NORTH  );
		  c2.add(displayPanel);	
		  judgePane.pack();
		  if (translationInt == 0)
		  {
			  System.out.println("translationInt is zero "+translationInt);
			 JOptionPane.showMessageDialog(null, "No records in file"
						,"Message for Judging",JOptionPane.INFORMATION_MESSAGE);
		  }
		  else
		  {
		 
		  }
	}
	  public void mouseClicked(MouseEvent me)
	  {
		  SwingUtilities.isRightMouseButton(me);
	   
	    {
	       // it is right mouse click
	      
	    }
	  }
	  public void mousePressed(MouseEvent e)
	  {
		 // System.out.println("mousepressed");
		  JButton button = (JButton)e.getSource();
		  String cc = button.getActionCommand();
		  System.out.println("cc "+cc);
		  if (cc.startsWith("Debugging"))
		  {
			  bc = debugbtn.getActionCommand();
			  getAllFramesDebug();
		  }
		  else if (cc.startsWith("Translator"))
		  {
			  ac = Translatebtn.getActionCommand();
			  System.out.println("ac "+ac);
			  getAllFrames();
		  }
		  else if (cc.startsWith("Judge"))
		  {
			  dc = Judgebtn.getActionCommand();
			  System.out.println("dc "+dc);
			  getAllJudgeFrames();
		  }
	  	
	   
	  }
	  public void mouseReleased(MouseEvent e)
	  {
	  }
	  public void mouseEntered(MouseEvent e)
	  {
	  }
	  public void mouseExited(MouseEvent e)
	  {
	  }	 
	 public void internalFrameClosing(InternalFrameEvent e) {
		regulusWindow.judgeCounter--;
		remove_finished = false;
		lookForIcon();
			
	 }

	 public void internalFrameClosed(InternalFrameEvent e) {
			
	  }

	public void internalFrameOpened(InternalFrameEvent e) {
			
	 }

	public void internalFrameIconified(InternalFrameEvent e) {
		getIconFrame();
		remove_finished = false;
		lookForIcon();
			
	 }
	
	public void internalFrameDeiconified(InternalFrameEvent e) {
		getselectedFrame();
		Judgebtn =     new JButton(strName);
		regulusWindow.barPanel.add(Judgebtn);
		Judgebtn.addMouseListener(this);
		getselectedFrame();
		    	
	   }

	   public void internalFrameActivated(InternalFrameEvent e) {
		    	
		 }

		public void internalFrameDeactivated(InternalFrameEvent e) {
			
		 }
		    
		public void lookForIcon() {
			for (int i = 0; remove_finished == false ; i ++)  {
				c = regulusWindow. barPanel.getComponent(i);
				String strname = c.toString();
				//String strName = c.getName();
				 String b = strName;
				b = "Judge";
				// System.out.println("strName "+strName);
				// System.out.println("strname "+strname);
				 if (strname.indexOf(b) != -1) {
					 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
					 remove_finished = true;
				 }
				 else
				 {
						
					 
				 }
			 }
		}	 
		 public void getAllFrames(){
	    	 frames = regulusWindow.desktop.getAllFrames();
	    	 frameIndex = 0;
	         int countFrames = frames.length;
	         for (int i = 0; i < countFrames; i++) {
	          String strFrames = frames[i].toString();
 	         holdFrameIndex = i;
	         String b = ac;
	         System.out.println("b "+b);
			 if (strFrames.indexOf(b) != -1) {
  				try {
  					System.out.println("strFrames "+strFrames);
  					frames[i].setSelected(true);
  				
			  } catch (Exception e) {
	             // This should not happen
	             e.printStackTrace();
	         }
  			 
			}
	      }
		}
	    
		 public void getAllJudgeFrames(){
	    	 frames = regulusWindow.desktop.getAllFrames();
	    	 frameIndex = 0;
	         int countFrames = frames.length;
	         for (int i = 0; i < countFrames; i++) {
	          String strFrames = frames[i].toString();
 	         holdFrameIndex = i;
	         String b = dc;
	         System.out.println("b "+b);
			 if (strFrames.indexOf(b) != -1) {
  				try {
  					System.out.println("strFrames "+strFrames);
  					frames[i].setSelected(true);
  				
			  } catch (Exception e) {
	             // This should not happen
	             e.printStackTrace();
	         }
  			 
			}
	      }
		}
	    
	    public void getAllFramesDebug(){
	    	 frames = regulusWindow.desktop.getAllFrames();
	    	 frameIndex = 0;
	         int countFrames = frames.length;
	         for (int i = 0; i < countFrames; i++) {
	          String strFrames = frames[i].toString();
	         holdFrameIndex = i;
	         String b = bc;
			 if (strFrames.indexOf(b) != -1) {
 				try {
 					frames[i].setSelected(true);
 				
			  } catch (Exception e) {
	             // This should not happen
	             e.printStackTrace();
	         }
 			 
			}
	      }
		}
	 
	    	
	    public void getselectedFrame(){
	    	 frames = regulusWindow.desktop.getAllFrames();
	    	 frameIndex = 0;
	         int countFrames = frames.length;
	         for (int i = 0; i < countFrames; i++) {
	          String strFrames = frames[i].toString();
	         holdFrameIndex = i;
	         String b = "Judge";
			 if (strFrames.indexOf(b) != -1) {
				 
 				if (frames[i].isSelected())
 				{
 					strName = frames[i].getTitle();
 					//System.out.println("strName "+strName);
 				}
 			 
			}
	      }
	  }
	 
	    public void getIconFrame(){
 	    	 frames = regulusWindow.desktop.getAllFrames();
 	    	 frameIndex = 0;
 	         int countFrames = frames.length;
 	         for (int i = 0; i < countFrames; i++) {
  	          String strFrames = frames[i].toString();
   	         holdFrameIndex = i;
 	         String b = "Translator";
  			 if (strFrames.indexOf(b) != -1) {
  				 
    				if (frames[i].isIcon())
    				{
    					strName = frames[i].getTitle();
     				}
    			 
  			}
 	      }
 	  }	    	
	   
	        	 
	public void CreateScreen()
	{
		
		  
		  // set up the layout
		  gblayout = new GridBagLayout();
		  
		  // instansiate the gridbag constraints
		  gbConstraints = new GridBagConstraints();
		  gbConstraints.fill = GridBagConstraints.HORIZONTAL;
		  displayPanel.setLayout(gblayout);
	
		  
//			 save button
			 
		  save.setToolTipText("Save the Corpus judge file");
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = 0;
		  displayPanel.add(save,gbConstraints); 
		  
//			 Next button
		  next.setToolTipText("Show next record");
		  gbConstraints.weightx = 0.5;
		  gbConstraints.gridx = 1;
		  gbConstraints.gridy = 0;
		  displayPanel.add(next,gbConstraints);
		  
//			 Previous button
		  previous.setToolTipText("Show previous record");
		  gbConstraints.gridx = 2;
		  gbConstraints.gridy = 0;
		  displayPanel.add(previous,gbConstraints); 
		  
//			 next unjudged button
		 
		  unjudged.setToolTipText("Show next unjudged record");
		  gbConstraints.gridx = 3;
		  gbConstraints.gridy = 0;
		  displayPanel.add(unjudged,gbConstraints); 
		  

//			 first record button
			 
		  first.setToolTipText("Show first record in file");
		  gbConstraints.gridx = 4;
		  gbConstraints.gridy = 0;
		  displayPanel.add(first,gbConstraints); 
		  
//			 last record button
			 
		  last.setToolTipText("Show last record in file");
		  gbConstraints.gridx = 5;
		  gbConstraints.gridy = 0;
		  displayPanel.add(last,gbConstraints); 
		  
//			 undo previous record button
			 
		  undo.setToolTipText("Undo change in previous record");
		  gbConstraints.gridx = 6;
		  gbConstraints.gridy = 0;
		  displayPanel.add(undo,gbConstraints); 
		  
//			set counter label		  
		  
		  gbConstraints.gridx = 7;
		  gbConstraints.gridy = 0;
		  displayPanel.add(counter,gbConstraints); 
		  
//			set counter 	
		  
		  counterArea.setToolTipText("show how manu records have been read");
		  gbConstraints.gridx = 8;
		  gbConstraints.gridy = 0;
		  displayPanel.add(counterArea,gbConstraints); 
		  
		  
//			Add labels and textboxes
		  
		  createTestArea();
		  
//			Read the first record
		  
		  DecideWichCorpus();
		  DecideWichReadCorpus();
		  counterArea.setText("0");
//Now check if there were any corpus items to read
		  if (translationInt == 0)
		  {
			  System.out.println("translationInt is zero "+translationInt);
			 
		  }
		  else
		  {
		  GetTranslationItems();
		  WriteTranslationItems();
		  }
		  
// Check if next record button is pressed down 
		  
		  save.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
							DecideWichWriteCorpus();
				  		}
				  	}
				  );
		  
		  
		  // Check if next record button is pressed down 
		  
		  next.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
				  			ReadBatchTranslationResultsNextRecord();
				  			GetTranslationItems();
				  			String sVal = String.valueOf(batchIndex) ;
			  				counterArea.setText(sVal);
				  			WriteTranslationItems();
				  			PreviousJudgementString = JudgementString;
				  		}
				  	}
				  );
		  // Check if previous record button is pressed down 
		  
		  previous.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
				  			ReadBatchTranslationResultsPreviousRecord();
				  			GetTranslationItems();
				  			String sVal = String.valueOf(batchIndex) ;
			  				counterArea.setText(sVal);
				  			WriteTranslationItems();
				  		}
				  	}
				  );
		  
		  unjudged.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
				  			readTranslationItems();
			  				GetTranslationItems();
			  				String sVal = String.valueOf(showWhereIndex) ;
			  				counterArea.setText(sVal);
			  				WriteTranslationItems();
			  				PreviousUnjudgedJudgementString = JudgementString;
			  				unjudged_record_found = false;
					  			
				  		}
				  	}
				  );
		  first.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
//				 			Read the first record
							ShowBatchSpeechTranslationResultsFirstRecord();
			  				GetTranslationItems();
			  				counterArea.setText("0");
			  				WriteTranslationItems();
						  			
				  		}
				  	}
				  );
		  
		  last.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
							ShowBatchSpeechTranslationResultsLastRecord();
							//DecideWichCorpus();
							//DecideWichReadLastRecordCorpus();
			  				GetTranslationItems();
			  				String sVal = String.valueOf(translationInt) ;
			  				counterArea.setText(sVal);
			  				WriteTranslationItems();
						  			
				  		}
				  	}
				  );
		  
		  undo.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			good.setSelected(false);
				  			ok.setSelected(false);
							bad.setSelected(false);
							donotknow.setSelected(false);
							batchIndex = savebatchIndex;
							batchIndex = batchIndex - 1;
							ReadBatchTranslationResultsNextRecord();
							//ReadBatchTranslationResultsNextUnjudgedRecord();
							GetTranslationItems();
				  			JudgementString = PreviousUnjudgedJudgementString;
				  			System.out.println("JudgementString "+JudgementString);
				  			WriteTranslationItems();
				  		}
				  	}
				  );
		  
		 
		  good = new JRadioButton("Good",false);
		  bar.add(good);
		  ok = new JRadioButton("OK",false);
		  bar.add(ok);
		  bad = new JRadioButton("Bad",false);
		  bar.add(bad);
		  donotknow = new JRadioButton("Don't know",false);
		  bar.add(donotknow);
		  good.addActionListener(this);
		  ok.addActionListener(this);
		  bad.addActionListener(this);
		  donotknow.addActionListener(this);
		  
		  
		  // create Help  menu item and sub menu items
		  JMenu helpMenu = new JMenu( "Help" );
		  helpMenu.setMnemonic( 'H');
		  HelpMenu = new JMenuItem( "Display HelpText" );
		  HelpMenu.setToolTipText("Reading Help Text");
		  HelpMenu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				CreateAndLinkhelp();
		  				// put in name of code which reads help file
		  				
		  				regulusWindow.availablemenus.check_available_menus();
		  				regulusWindow.unavailablecommands.check_unavailable_menus();
		  			}
		  		}
		  		);
		  
		  helpMenu.add(HelpMenu);
		  bar.add(helpMenu);
		  
		  quit_system_MenuItem = new JMenuItem( "Exit" );
		  quit_system_MenuItem.setToolTipText("Exit System");
		  quit_system_MenuItem.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				System.exit(0);
		  			}
		  		}
		  		);
		  
		  helpMenu.add(quit_system_MenuItem);
		  bar.add(helpMenu);
	}
		
	

	
	 public void DecideWichWriteCorpus()
	  {
		  if(Corpus.lastIndexOf(TranslateSpeechCorpusAgainString) != -1) {
			  WriteUpdatedJudgeBatchFile();
		  }
		  else if (Corpus.lastIndexOf(TranslateSpeechCorpusString) != -1) {
			  WriteUpdatedJudgeBatchFile();
		  }
		  else if (Corpus.lastIndexOf(TranslateCorpusString) != -1) {
			  System.out.println("just before WriteUpdatedJudgeFile");
			  WriteUpdatedJudgeFile();
		  }
	  }
	
	 public void DecideWichCorpus()
	  {
		  if(Corpus.lastIndexOf(TranslateSpeechCorpusAgainString) != -1) {
			  TranslateSpeechCorpusAgainDefaultOrNot();
		  }
		  else if (Corpus.lastIndexOf(TranslateSpeechCorpusString) != -1) {
			  TranslateSpeechCorpusDefaultOrNot();
		  }
		  else if (Corpus.lastIndexOf(TranslateCorpusString) != -1) {
			  TranslateCorpusDefaultOrNot();
		  }
	  }
	 
	 public void DecideWichReadCorpus()
	  {
		  if(Corpus.lastIndexOf(TranslateSpeechCorpusAgainString) != -1) {
			  ReadBatchSpeechTranslationResultsFirstRecord(); 
		  }
		  else if (Corpus.lastIndexOf(TranslateSpeechCorpusString) != -1) {
			  ReadBatchSpeechTranslationResultsFirstRecord();
		  }
		  else if (Corpus.lastIndexOf(TranslateCorpusString) != -1) {
			  ReadBatchTranslationResultsFirstRecord();
		  }
	  }
	 
	 public void DecideWichReadLastRecordCorpus()
	  {
		  if(Corpus.lastIndexOf(TranslateSpeechCorpusAgainString) != -1) {
			  ReadBatchSpeechTranslationResultsLastRecord(); 
		  }
		  else if (Corpus.lastIndexOf(TranslateSpeechCorpusString) != -1) {
			  ReadBatchSpeechTranslationResultsLastRecord();
		  }
		  else if (Corpus.lastIndexOf(TranslateCorpusString) != -1) {
			  ReadBatchTranslationResultsLastRecord();
		  }
	  }
	public void TranslateCorpusDefaultOrNot()
	{
		length = Corpus.length();
		if (length > 16)
		{
			TranslateCorpusDecideKey();
			
		}
		else
		{
			ReadKey = "default";
			
		}
	}
	public void TranslateCorpusDecideKey()
	{
		if ( Corpus.substring(16,17).equals(SpaceString))
		{
			ReadKey = Corpus.substring(17,length);
		}
		else
		{
			ReadKey = Corpus.substring(16,length);	
		}
	}
	public void TranslateSpeechCorpusDefaultOrNot()
	{
		length = Corpus.length();
		if (length > 23)
		{
			TranslateSpeechCorpusDecideKey();
			
		}
		else
		{
			ReadKey = "default";
			
		}
	}
	
	public void TranslateSpeechCorpusDecideKey()
	{
		if ( Corpus.substring(23,24).equals(SpaceString))
		{
			ReadKey = Corpus.substring(24,length);
		}
		else
		{
			ReadKey = Corpus.substring(23,length);	
		}
	}
	public void TranslateSpeechCorpusAgainDefaultOrNot()
	{
		length = Corpus.length();
		if (length > 29)
		{
			TranslateSpeechCorpusAgainDecideKey();
		
		}
		else
		{
			ReadKey = "default";
			
		}
	}
	public void TranslateSpeechCorpusAgainDecideKey()
	{
		if ( Corpus.substring(29,30).equals(SpaceString))
		{
			ReadKey = Corpus.substring(30,length);
		}
		else
		{
			ReadKey = Corpus.substring(29,length);	
		}
	}
	public void decideDefaulOrNot()
	{
		if (ReadKey.equals("default"))
		{
			JudgeCommandFull = JudgeCommandPart1;
		}
		else
		{
			JudgeCommandFull = JudgeCommandPart1 + ReadKey;	
		}
	}
	
	public void decideDefaultBatchOrNot()
	{
		if (ReadKey.equals("default"))
		{
			JudgeCommandSpeechFull = JudgeCommandSpeechPart1;
		}
		else
		{
			JudgeCommandSpeechFull = JudgeCommandSpeechPart1 + ReadKey;	
		}
	}
	public void WriteUpdatedJudgeFile()
	{
		decideDefaulOrNot();	
		regulusWindow.putBatchTranslationResults(ReadKey);
		regulusWindow.handleCommand(JudgeCommandFull);
		
		if (regulusWindow.regulus_command_succeeded)
		{ 
			InputText = "Updating file for Translation Judgements succeeded";
			regulusWindow.txtBoxDisplayPositive(InputText);
		
		}
		else
		{
			String command = regulusWindow.getCommandErrorString();	
			InputText = command;
			regulusWindow.txtBoxDisplayPositive(InputText);
		}
			
	}
	public void WriteUpdatedJudgeBatchFile()
	{ 
		decideDefaultBatchOrNot();
		regulusWindow.putBatchSpeechTranslationResults(ReadKey);
		System.out.println("JudgeCommandSpeechFull in speech"+JudgeCommandSpeechFull);
		regulusWindow.handleCommand(JudgeCommandSpeechFull);
		
		if (regulusWindow.regulus_command_succeeded)
		{ 
			InputText = "Updating file for Translation Judgements succeeded";
			regulusWindow.txtBoxDisplayPositive(InputText);
		
		}
		else
		{
			String command = regulusWindow.getCommandErrorString();	
			InputText = command;
			regulusWindow.txtBoxDisplayPositive(InputText);
		}
			
	}
	public void actionPerformed(ActionEvent e)
	  {
		PreviousUnjudgedJudgementString = DisplayTextPanes[3].getText();
		System.out.println("PreviousUnjudgedJudgementString "+PreviousUnjudgedJudgementString);
		  if (e.getSource() == good)
		  {
			  DisplayTextPanes[3].setText(("good"));
			  DisplayTextPanes[3].setCaretPosition(0);
			  translationresult.setJudgement("good");
			  ok.setSelected(false);
			  bad.setSelected(false);
			  donotknow.setSelected(false);
			  savebatchIndex = batchIndex;
			 
			  System.out.println("savebatchIndex "+savebatchIndex);
		  }
		  else if (e.getSource() == ok)
		  {
			  DisplayTextPanes[3].setText(("ok"));
			  DisplayTextPanes[3].setCaretPosition(0); 
			  translationresult.setJudgement("ok");
			  good.setSelected(false);
			  bad.setSelected(false);
			  donotknow.setSelected(false);
			  savebatchIndex = batchIndex;
			 // PreviousUnjudgedJudgementString = "ok";
			  //System.out.println("savebatchIndex "+savebatchIndex);
		  }
		  else if (e.getSource() == bad)
		  {
			  DisplayTextPanes[3].setText(("bad"));
			  DisplayTextPanes[3].setCaretPosition(0);
			  translationresult.setJudgement("bad");
			  good.setSelected(false);
			  ok.setSelected(false);
			  donotknow.setSelected(false);
			  savebatchIndex = batchIndex;
			 // PreviousUnjudgedJudgementString = "bad";
			  //System.out.println("savebatchIndex "+savebatchIndex);
		  }
		  else if (e.getSource() == donotknow)
		  {
			  DisplayTextPanes[3].setText(("don't know"));
			  DisplayTextPanes[3].setCaretPosition(0);
			  translationresult.setJudgement("?");
			  good.setSelected(false);
			  ok.setSelected(false);
			  bad.setSelected(false);
			  savebatchIndex = batchIndex;
			 // PreviousUnjudgedJudgementString = "?";
			 // System.out.println("savebatchIndex "+savebatchIndex);
		  }
	  }
	public void readTranslationItems()
	{
		for (int i = 0 ; unjudged_record_found == false ; i++) 
		{
			findUnjudgedRecord();
			showWhereIndex = i;
		}
	}
	  
	public void findUnjudgedRecord()
	{
		ReadBatchTranslationResultsNextUnjudgedRecord();
		if (translationInt == 0)
		{
			System.out.println(" there are no unjudged records ");
		}
		else
		{
		CheckTranslationItems();
		}
		
	}
	public void CheckTranslationItems()
	{
		JudgementString = translationresult.getJudgement();
		SourceString    = translationresult.getSource();
		unjudged_record_found = false;
		if (JudgementString.equals("?"))
		{
			unjudged_record_found = true;
		}
		else
		{
			unjudged_record_found = false;
		}
	}
	 public void GetTranslationItems()
	 {
		SourceString    = translationresult.getSource();
		TargetString    = translationresult.getTarget();
		ContextString   = translationresult.getContext();
		JudgementString = translationresult.getJudgement();
	 }
	 public void WriteTranslationItems()
	  {
		   DisplayTextPanes[0].setText((SourceString));
		   DisplayTextPanes[0].setCaretPosition(0);
		   DisplayTextPanes[1].setText((TargetString));
		   DisplayTextPanes[1].setCaretPosition(0);
		   DisplayTextPanes[2].setText((ContextString));
		   DisplayTextPanes[2].setCaretPosition(0);
		   DisplayTextPanes[3].setText((JudgementString));
		   DisplayTextPanes[3].setCaretPosition(0);
	  }
	public void createTestArea()
	{
		  for (index = 0; index < 4; index++ ) 
		  {
			  createTest();
		  }
	}
	public void createTest()
	{
		  // add label
		  temp = 3 + index;
		  gbConstraints.fill = GridBagConstraints.VERTICAL;
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = temp;
		  gbConstraints.gridheight = 1;
		  gbConstraints.gridwidth = 1;
		  gbConstraints.weightx = 0;
		  gbConstraints.weighty = 0.7;
		  
		  displayPanel.add(DisplayLabels[index] ,gbConstraints);
		  

		  
		  // add textbox
		  DisplayTextPanes[index] = new JTextArea(2,70);
		  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
		  DisplayTextPanes[index].setEditable(false);
		  DisplayScrollPanes[index] = new JScrollPane(DisplayTextPanes[index]);
		  gbConstraints.fill = GridBagConstraints.BOTH;
		  gbConstraints.gridx = 1;
		  gbConstraints.gridy = temp;
		  gbConstraints.gridheight = 1;
		  gbConstraints.gridwidth = 10;
		  gbConstraints.weightx = 10;
		  gbConstraints.weighty = 0.7;
		  setAddArea();
	}  
		  public void setAddArea()
		  {
		    displayPanel.add(DisplayScrollPanes[index],gbConstraints); 
		  }	  
	  public void CreateAndLinkhelp()
	  {
		    HelpPane helpPane = new HelpPane(this, getRegulusGUI());
		  	helpPane.setRegulusGUI(getRegulusGUI());
		 //  display new internal window
		    JInternalFrame HelpPaneInternalFrame = helpPane.getInternalFrame();
		 //add the internal frame to the desktop
			regulusWindow.desktop.add(HelpPaneInternalFrame,JLayeredPane.DEFAULT_LAYER);
			HelpPaneInternalFrame.setVisible(true); 
	  }
	  public void ShowBatchSpeechTranslationResultsFirstRecord()
	   { 
		  translationresult = regulusWindow.batchTranslationResults[0];
		   batchIndex = 0;
	   }
	  public void ShowBatchSpeechTranslationResultsLastRecord()
	   { 
		   LastRecordIndex = translationInt -1;
		   translationresult = regulusWindow.batchTranslationResults[LastRecordIndex];
		   batchIndex = LastRecordIndex;
	   }
	   public void ReadBatchSpeechTranslationResultsFirstRecord()
	   { 
		   translationInt = regulusWindow.getBatchSpeechTranslationResults(ReadKey);
		   batchIndex = 0;
		   translationresult = regulusWindow.batchTranslationResults[0];
	   }
	   
	   public void ReadBatchSpeechTranslationResultsLastRecord()
	   { 
		   translationInt = regulusWindow.getBatchSpeechTranslationResults(ReadKey);
		   LastRecordIndex = translationInt -1;
		   translationresult = regulusWindow.batchTranslationResults[LastRecordIndex];
	   }
	   public void ReadBatchTranslationResultsFirstRecord()
	   {
		   batchIndex = 0;
		   translationInt = regulusWindow.getBatchTranslationResults(ReadKey);
		   translationresult = regulusWindow.batchTranslationResults[0];
	   }
	   public void ReadBatchTranslationResultsLastRecord()
	   {
		   translationInt = regulusWindow.getBatchTranslationResults(ReadKey);
		   LastRecordIndex = translationInt -1;
		   translationresult = regulusWindow.batchTranslationResults[LastRecordIndex];
	   }
	   public void ReadBatchTranslationResultsNextRecord()
	   { 
		   
		   if (batchIndex < translationInt )
		   {
		   batchIndex = batchIndex + 1;
		   translationresult = regulusWindow.batchTranslationResults[batchIndex];
		   }
		   else
		   {

 				JOptionPane.showMessageDialog(null, "No more records in file"
						,"Message for Judging",JOptionPane.INFORMATION_MESSAGE);
		   }
		 
	   }
	   
	 
	   
	   public void ReadBatchTranslationResultsPreviousRecord()
	   { 
		   if (batchIndex > 0 )
		   {
		   batchIndex = batchIndex - 1;
		   translationresult = regulusWindow.batchTranslationResults[batchIndex];
		   }
		   else
		   {
			   
 				JOptionPane.showMessageDialog(null, "You are at the first record in the file"
						,"Message for Judging",JOptionPane.INFORMATION_MESSAGE);
		   }
		 
	   }
	   public void ReadBatchTranslationResultsNextUnjudgedRecord()
	   { 
		  
		   if (batchIndex < translationInt)
		   {
		   batchIndex = batchIndex + 1;
		   translationresult = regulusWindow.batchTranslationResults[batchIndex];
		   }
		   else
		   {
			   unjudged_record_found = true;
 				JOptionPane.showMessageDialog(null, "No more unjudged records in file"
						,"Message for Judging",JOptionPane.INFORMATION_MESSAGE);
		   }
		 
	   }
	   
	 
	   public void vetoableChange(PropertyChangeEvent event)
	   throws PropertyVetoException {
	    
	   JInternalFrame frame2 = (JInternalFrame) event.getSource();
	   String name = event.getPropertyName();
	   Object value = event.getNewValue();
	   int count = 0;

//	    we only want to check attempts to close a frame

	   if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
	   	int result = JOptionPane.showConfirmDialog(judgePane,
	   			"      OK to close?",
	   			"Close Confirm Pane",
				JOptionPane.OK_CANCEL_OPTION);
	   	// if the user doesn't agree veto the close
	   	if (result == JOptionPane.CANCEL_OPTION)
	   		throw new PropertyVetoException("User cancelled close", event);
	   	else
	   	//count = aPerformed();
	   	//System.out.println("count "+count);
	   	setVisible(false);
	   	dispose();
	   	return;
	   }
	   }

}
