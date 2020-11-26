package JudgeGUI;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import javax.swing.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;
import org.w3c.dom.Document;
import org.w3c.dom.*;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException; 
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.DOMException;

public class JudgeGUI extends JFrame implements  VetoableChangeListener,ActionListener,KeyListener,MouseListener{

	private JDesktopPane desktop;
	private JInternalFrame frame1;
	private JudgeGUI judgeWindow = null;
	public  JInternalFrame[] allFrames = null;
	private TranslationTable translationtable = null;
	private JMenuBar bar = new JMenuBar();  // create menubar
	private JMenuItem HelpMenu;
	private JMenu judgeMenu;
	private JMenuItem JudgeMenu;
	private JTextField inputField = new JTextField(85);
	private JButton next = new JButton("Next Record");
	private JButton previous = new JButton("Last Edited Record");
	private JButton unjudged = new JButton("Next Unjudged");
	private JButton save = new JButton("Save File");
	private JButton first = new JButton("First Record");
	private JButton last = new JButton("Last Record");
	private JButton undo = new JButton("    Undo    ");
	private JButton undoprevious = new JButton("Undo Prev Unjudged");
	private JButton play = new JButton("Play File");
	private GridBagLayout gblayout;
	private GridBagConstraints gbConstraints;
	private JPanel displayPanel = new JPanel();
    private JPanel inputPanel = new JPanel(new BorderLayout());
    private int index = 0;
    private int temp = 0;
    private JLabel Wavfile = new JLabel("Wavfile");
    private JLabel DisplayLabels[] = {Wavfile };
    private JScrollPane[] DisplayScrollPanes = new JScrollPane[2];
    private JTextArea[] DisplayTextPanes = new JTextArea[2];
    private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
    private final String[] DisplayLabelText = {"Wavfile"};
    private String record = null;
    private String[] languageStrings = { "English", "Spanish" ,"English and Spanish" };
    private int xmlIndex = 0;
    private String fileString = ""; 
    private String translationString = "";
    private String sourceLanguageString = ""; 
    private String languageName = "";
    private String savelanguageName = "";
    private boolean person_found = false;
    private int languageCounter = 0;
    private boolean bothLanguages = false;
    private int totNumRecords = 0;
    private int lastRecordIndex = 0;
    private boolean next_person_found = false;
    private boolean previous_person_found = false;
    private int saveIndex = 0;
    private String PrevFile = "";
    private String PrevTranslation = "";
    private int stranslationInt = 0;
    private int sfileInt = 0;
    private String saveAreaFile[] = new String[100];
    private String saveAreaSentence[] = new String[100];
    private String contentsTable[] = new String[1000];
    private int recCount = 0;
    private int saveAreaIndex = 0;
    private String line = null; 
    private int checkIndex = 0;
    private int  startPos = 0;
    private int  endPos = 0;
    private String saveInputArea = "";
    private String editSentenceString = "";
    private boolean inputRecordFound = false;
    private boolean unjudgedRecordFound = false;
    private boolean unjudgedRecorCreated = false;
    private int saveCheckIndex = 0;
    private String saveFileNextUnjudged = "";
    private boolean languageNameIsCorrect = false;
    private String lastEditedFile = "";
    private String SentenceBeforeEdit = "";
    private String FileBeforeEdit = "";
    private String dirName = "";
    private String xmlFileNameIn = "/medslt-log.xml";
    private String xmlFileNameOut = "/medslt-logout.xml";
    private String fullPathName = "";
    private String editInputArea = "";
  
    
//  send name of internal frame 
	 public JInternalFrame getInternalFrame() {
	  return frame1;
	  }
	 // get pointer to Regulus window
	  public JudgeGUI getRegulusGUI() {
		  return judgeWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setJudgeGUI(JudgeGUI window) {
		  judgeWindow = window;
	  }
	  public JudgeGUI() {
		  
	  }
	  //public Frame3(RegulusGUI window)
	  public JudgeGUI(String directoryName) {
		  super("Transcription and Judging");
		  
		  dirName = directoryName;
			 
			   //Set up the GUI.
        desktop = new JDesktopPane(); //a specialized layered pane
        
        frame1 = new JInternalFrame("Transcription",true,true,true,true);
        
        setContentPane(desktop);
        
  //		 create menubar for internal frame
	    
	    setJMenuBar( bar );  // set the menubar for the JInternalFrame
	    
	    CreateScreen();
	    checkIfEmptyOutPutFile();
	    SystemTrayTest systemtraytest = new SystemTrayTest();
		Container c2 = frame1.getContentPane();
		
//		 add listener to confirm closing and listener to keyboard
		 inputField.addKeyListener(this);
		 inputField.addMouseListener(this);

		frame1.addVetoableChangeListener(this);
		inputPanel.add(inputField , BorderLayout.CENTER);
		inputPanel.add(bar,BorderLayout.NORTH);
		c2.add(inputPanel,BorderLayout.NORTH  );
		c2.add(displayPanel);	
		setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		frame1.pack();
		desktop.add(frame1);
		frame1.setVisible(true);
		setSize(1000,700);
		setVisible(true);
	}
	
	
	public void CreateScreen()
	{
		  // set up the layout
		  gblayout = new GridBagLayout();
		  
		  // instansiate the gridbag constraints
		  gbConstraints = new GridBagConstraints();
		  gbConstraints.fill = GridBagConstraints.HORIZONTAL;
		  displayPanel.setLayout(gblayout);
		  
		 
		  // create Help  menu item and sub menu items
		  JMenu helpMenu = new JMenu( "Help" );
		  helpMenu.setMnemonic( 'H');
		  HelpMenu = new JMenuItem( "Display HelpText" );
		  HelpMenu.setToolTipText("Reading Help Text");
		  HelpMenu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				//CreateAndLinkhelp();
		  			
		  			}
		  		}
		  		);
		  
		  helpMenu.add(HelpMenu);
		  bar.add(helpMenu);
		  
		  // create Menu option menu to go to judge menu
		  JMenu judgeMenu = new JMenu( "Judge" );
		  judgeMenu.setMnemonic( 'J');
		  JudgeMenu = new JMenuItem( "Do Judging" );
		  JudgeMenu.setToolTipText("Do Judging");
		  JudgeMenu.addActionListener(
		  		new ActionListener() {
		  			public void actionPerformed( ActionEvent e)
		  			{
		  				CreateAndLinkJudge();
		  			
		  			}
		  		}
		  		);
		  
		  judgeMenu.add(JudgeMenu);
		  bar.add(judgeMenu);
		  
		 
		  
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
		  
//			 play voice
			 
		  play.setToolTipText("play voice recording");
		  gbConstraints.gridx = 2;
		  gbConstraints.gridy = 0;
		  displayPanel.add(play,gbConstraints); 
		  
//			 Previous button
		  previous.setToolTipText("Show previous record");
		  gbConstraints.gridx = 3;
		  gbConstraints.gridy = 0;
		  displayPanel.add(previous,gbConstraints); 
		  
//			 next unjudged button
		 
		  unjudged.setToolTipText("Show next unjudged record");
		  gbConstraints.gridx = 4;
		  gbConstraints.gridy = 0;
		  displayPanel.add(unjudged,gbConstraints); 
		  

		  
		  
//			 first record button
			 
		  first.setToolTipText("Show first record in file");
		  gbConstraints.gridx = 5;
		  gbConstraints.gridy = 0;
		  displayPanel.add(first,gbConstraints); 
		  
//			 last record button
			 
		  last.setToolTipText("Show last record in file");
		  gbConstraints.gridx = 6;
		  gbConstraints.gridy = 0;
		  displayPanel.add(last,gbConstraints); 
		  
//			 undo previous record button
			 
		  undo.setToolTipText("Undo change in previous record");
		  gbConstraints.gridx = 7;
		  gbConstraints.gridy = 0;
		  displayPanel.add(undo,gbConstraints); 
		  

		  
		  // Create the Combobox, select the defauls item
		  JComboBox languageList = new JComboBox(languageStrings);
		  languageList.setSelectedIndex(0);
		  languageList.addActionListener(this);
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = 2;
		  displayPanel.add(languageList,gbConstraints);
		  
//			Add labels and textboxes
		  
		  createTestArea();
		  languageName = "English";
		  populateScreenAfterLanguageSelection();
		  
		  // press first record button
		  first.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			populateScreenAfterLanguageSelection();
						  			
				  		}
				  	}
				  );
		  
		  // press next button
		  next.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			populateScreenAfterNextSelection();
						  			
				  		}
				  	}
				  );
		  
		  // press last edited button
		  previous.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			inputField.setText( PrevTranslation);
				  			DisplayTextPanes[0].setText((PrevFile));
				  			DisplayTextPanes[0].setCaretPosition(0);
				  			saveIndex = saveIndex -2;
				  			
						  			
				  		}
				  	}
				  );
		  
		  // press next unjudged button
		  unjudged.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			readTextFile();
				  			FindInputRecord();
				  			findNextUnjudgedRecord();
				  		}
				  	}
				  );
		   
		  // press last button
		  last.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			populateScreenAfterLastSelection();
				  		}
				  	}
				  ); 
		  
//		 press save button
		  save.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			getContents();
				  			setContents();
				  		}
				  	}
				  );
		  
//			 press undo button
		  undo.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{  
				  			inputField.setText(SentenceBeforeEdit);
				  			DisplayTextPanes[0].setText(FileBeforeEdit );
				  			
			  			        
				  		}
				  	}
				  ); 
		  
//			 play file
		  play.addActionListener(
				  	new ActionListener() {
				  		public void actionPerformed( ActionEvent e)
				  		{ 
				  		String FileName = "";
				  		FileName = DisplayTextPanes[0].getText();
				  		   try {
						       String Command = "playwav \"" + FileName + "\"";
						       System.out.println("\nExecuting: \"" + Command + "\\n");
				  		       Runtime.getRuntime().exec(Command);
				  		   }
				  		  catch (IOException ex){
						      ex.printStackTrace();}
				  		  
			  			        
				  		}
				  	
				  	}
				  ); 
				}
	
	
	 public void createTestArea()
	  {
		 
		  for (index = 0; index < 1; index++ ) 
		  {
			  createTest();
			
		  }
	  }
	  public void createTest()
	  {
		 
		 // add label
		  temp  = 6+ index;
		  gbConstraints.fill = GridBagConstraints.VERTICAL;
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = temp;
		  gbConstraints.gridheight = 1;
		  gbConstraints.gridwidth = 1;
		  gbConstraints.weightx = 0;
		  gbConstraints.weighty = 0.7;
		  displayPanel.add(DisplayLabels[index] ,gbConstraints);
	
		  
		  // add textbox
	
		  DisplayTextPanes[index] = new JTextArea(2,80);
		  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
		  DisplayTextPanes[index].setEditable(true);
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
	  
	  public void CreateAndLinkJudge()
	  {
		  Frame2 frame2 = new Frame2(dirName);
		  
		  frame2.setJudgeGUI(this);
	  	
//			display new internal window
		  	JInternalFrame frame2InternalFrame = frame2.getInternalFrame();
		  	
//		 add the internal frame to the desktop	
		  	desktop.add(frame2InternalFrame);
		  	frame2InternalFrame.setVisible(true);
	  }
	  public void keyPressed( KeyEvent e)
	  {
		  if (e.getKeyCode() == e.VK_ENTER)
		  {
			  //System.out.println("Keypressed");
			  saveAreaFile[saveAreaIndex] = DisplayTextPanes[0].getText();
			  saveAreaSentence[saveAreaIndex] = inputField.getText();
			  lastEditedFile = DisplayTextPanes[0].getText();
			  saveAreaIndex++;
		  }
			
	  }
	  public void keyReleased( KeyEvent e)
	  {
		  // TODO method stub 
		 // System.out.println("key released"+e.getKeyCode());
	  }
	  public void keyTyped( KeyEvent e)
	  {
		  // TODO method stub 
		  //System.out.println("key typed"+e.getKeyCode());
	  }
	  
	  public void mouseClicked(MouseEvent e)
	  {
		 // System.out.println(" Mouse clicked");
		  SentenceBeforeEdit = inputField.getText();
		  FileBeforeEdit =  DisplayTextPanes[0].getText();
	  }
	  public void mousePressed(MouseEvent e)
	  {
		  int modifiers = e.getModifiers();
		  if ((modifiers & InputEvent.BUTTON1_MASK)== InputEvent.BUTTON1_MASK) {
			
		  }
		  if ((modifiers & InputEvent.BUTTON2_MASK)== InputEvent.BUTTON2_MASK) {
			  System.out.println("Middle button pressed");
		  }
		  if ((modifiers & InputEvent.BUTTON3_MASK)== InputEvent.BUTTON3_MASK) {
			  System.out.println("right button pressed");
			  //HandleRightClick();
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
	  public void writeTranscriptionItems()
	  {
		  DisplayTextPanes[0].setText((fileString));
		  DisplayTextPanes[0].setCaretPosition(0);
	  }
	  
	  // listen to the comboBox
	  public void actionPerformed(ActionEvent e) {
		JComboBox cb = (JComboBox)e.getSource();
		languageName = (String)cb.getSelectedItem();
		populateScreenAfterLanguageSelection();
	  }
	  public void populateScreenAfterLanguageSelection()
	  {
		  if (languageName.equals("English")){
			  bothLanguages = false;
			  savelanguageName = "American English";
			  processFirstRecord();
			 
		  }
		  else if (languageName.equals("Spanish")){
			  bothLanguages = false;
			  savelanguageName = "American Spanish";
			  processFirstRecord();
		  }
		  else if (languageName.equals("English and Spanish")){
			  bothLanguages = true;
			  processFirstRecord();
		  }
	  }
	 
	  public void processFirstRecord() {
		  languageCounter = 0;
		  person_found = false;
		  readXmlcheckLanguage();
		  checkUpdates();
		  checkEditRecord();
		  checkWhichSentence();
		  checkEditSentence();
		  writeTranscriptionItems();
	  }
	  
	  public void checkEditRecord()
		 {
		  editSentenceString = "";
			 for ( int i = 0 ; saveAreaFile[i]!= null ; i++ )
			 {
			 String b = saveAreaFile[i];
			 	if (fileString.indexOf(b) != -1) {
			 		editSentenceString = saveAreaSentence[i];
			 	}
			 }
		 }
	  public void populateScreenAfterNextSelection() {
		  if (languageName.equals("English")){
			  bothLanguages = false;
			  savelanguageName = "American English";
			  processNextRecord();
			 
		  }
		  else if (languageName.equals("Spanish")){
			  bothLanguages = false;
			  savelanguageName = "American Spanish";
			  processNextRecord();
		  }
		  else if (languageName.equals("English and Spanish")){
			  bothLanguages = true;
			  processNextRecord();
		  }
	  }
	
	  public void checkWhichSentence() {
		 
		  if (saveInputArea.equals(""))
		  {
			  inputField.setText(translationString);
		  }
		  else
		  {
			  checkSentence();
			  //inputField.setText(saveInputArea); 
			  //System.out.println("saveInputArea "+saveInputArea);
		  }
	  }
	  public void checkSentence() {
		  String b = ">";
		  int length = saveInputArea.length();
		  System.out.println("length "+length  );
		  if (saveInputArea.startsWith(b))
		  {
			  editInputArea = saveInputArea.substring(1,length);
			  inputField.setText(editInputArea ); 
		  }
		  else
		  {
			  inputField.setText(saveInputArea); 
		  }
	  }
	  public void checkEditSentence() {
		  if ( editSentenceString.equals(""))
		  	{
			  System.out.println(" No editing has happened");
		  	}
		  else
		  {
			  inputField.setText(editSentenceString); 
			  
		  }
	  }
	  public void processNextRecord() {
		  next_person_found = false;
		  saveIndex = saveIndex + 1;
		  languageCounter = 0;
		  translationString = "";
		  PrevTranslation = inputField.getText();
		  PrevFile = DisplayTextPanes[0].getText();
		  readXmlNextRecord();
		  checkUpdates();
		  checkEditRecord();
		  checkWhichSentence();
		  checkEditSentence();
		  //inputField.setText(translationString);
		  writeTranscriptionItems();
		
	  }
	  
	  public void populateScreenAfterLastSelection() {
		  if (languageName.equals("English")){
			  bothLanguages = false;
			  savelanguageName = "American English";
			  processLastRecord();
			 
		  }
		  else if (languageName.equals("Spanish")){
			  bothLanguages = false;
			  savelanguageName = "American Spanish";
			  processLastRecord();
		  }
		  else if (languageName.equals("English and Spanish")){
			  bothLanguages = true;
			  processLastRecord();
		  }
	  }
	  
	  public void processLastRecord() {
		  previous_person_found = false;
		  languageCounter = 0;
		  translationString = "";
		  readXmlLastRecord();
		  if (stranslationInt > sfileInt)
		  {
			  translationString = PrevTranslation;
		  }
		  
		  checkUpdates();
		  checkEditRecord();
		  checkWhichSentence();
		  checkEditSentence();
		 // inputField.setText(translationString);
		  writeTranscriptionItems();
	  }
	  
	  public void closeFrame(int count){
		  // Iterate ower the frames, closing them if they are open
		 		count = count - 1;
			  for (int j = 0; j < count; j++ ){
			  JInternalFrame f = allFrames[j];
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
		 
		JInternalFrame frame2 = (JInternalFrame) event.getSource();
		String name = event.getPropertyName();
		Object value = event.getNewValue();
		int count = 0;
		
		// we only want to check attempts to close a frame
		
		if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
			int result = JOptionPane.showInternalConfirmDialog(frame2,
					" OK to close?");
			// if the user doesn't agree veto the close
			if (result == JOptionPane.CANCEL_OPTION)
				throw new PropertyVetoException("User cancelled close", event);
			else
			
			return;
		}
		
	}

	public void checkIfEmptyOutPutFile(){
		readTextFile();
		checkIfZeroRecords();
	}
  	public void readXmlcheckLanguage() {
  		 try {
  			fullPathName = dirName + xmlFileNameIn;
  			
	            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
	            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
	           
	            Document doc = docBuilder.parse (new File(fullPathName));
	            // normalize text representation
	            doc.getDocumentElement ().normalize ();
	           // System.out.println ("Root element of the doc is " + 
	            
	            NodeList listOfPersons = doc.getElementsByTagName("interaction");
	            int totalPersons = listOfPersons.getLength();
	            totNumRecords = totalPersons;
	           // System.out.println("Total no of people : " + totalPersons);
	          
	            for(int s=0; person_found == false  ; s++){
	                Node firstPersonNode = listOfPersons.item(s);
	                if(firstPersonNode.getNodeType() == Node.ELEMENT_NODE){
	                	NodeList listOfChildren =  firstPersonNode.getChildNodes();
	                	for (int i = 0; i<listOfChildren.getLength(); i++)
	                	{
	                		
	                		Node node = listOfChildren.item(i);
	                		
	                		if (node.getNodeType()== Node.ELEMENT_NODE )
	                		{
	                			
	                			NodeList childNodeList = 
	              	        	node.getChildNodes();
 	                			String strNode = node.getNodeName().toString();
 	                			if (strNode.equals("sourceLanguage"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList thirdNameList = firstPersonElement.getElementsByTagName("sourceLanguage");
	                			    Element thirdNameElement = (Element)thirdNameList.item(0);
	                				NodeList textFmList = thirdNameElement.getChildNodes();
	                				sourceLanguageString  = ((Node)textFmList.item(0)).getNodeValue().trim();
	                				if ( bothLanguages ) {
	                					saveIndex = s;
	                					person_found = true;
	                				}
	                					
	                				if (savelanguageName.equals(sourceLanguageString) ) {
	                					languageCounter++;
	                				}
	                				if (savelanguageName.equals(sourceLanguageString)&& languageCounter > 1  ) {
	                					  saveIndex = s;
	                					  person_found = true;
	                				}
	                			}		
 	                			else if (strNode.equals("wavfile"))
	                			{
	                				
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList firstNameList = firstPersonElement.getElementsByTagName("wavfile");
	                			   
	                			    Element firstNameElement = (Element)firstNameList.item(0);
	                				NodeList textFNList = firstNameElement.getChildNodes();
	                				fileString = ((Node)textFNList.item(0)).getNodeValue().trim();

	                			}
	                			
	                       			if (strNode.equals("primaryRecognitionResult"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                				NodeList secondNameList = firstPersonElement.getElementsByTagName("primaryRecognitionResult");
	                				
	 		                		Element secondNameElement = (Element)secondNameList.item(0);
		                			NodeList textFlList = secondNameElement.getChildNodes();
		                			if (textFlList.item(0) instanceof Text )
		                			{
		                			 translationString  = ((Node)textFlList.item(0)).getNodeValue().trim();
		                			 //System.out.println("translationString "+translationString  );
		                			}
	                			  }
	                			
	                			}
	                			
	                		}
	                	}
	                	
	                }
	         
	           
	  	   }catch (SAXParseException err) {
	        System.out.println ("** Parsing error" + ", line " 
	             + err.getLineNumber () + ", uri " + err.getSystemId ());
	        System.out.println(" " + err.getMessage ());

	        }catch (SAXException e) {
	        Exception x = e.getException ();
	        ((x == null) ? e : x).printStackTrace ();

	        }catch (Throwable t) {
	        t.printStackTrace ();
	        }
	       // System.exit (0);
 
	  }
  	   
  	public void readXmlNextRecord() {
 		 try {
 			fullPathName = dirName + xmlFileNameIn;
  			
	            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
	            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
	           
	            Document doc = docBuilder.parse (new File(fullPathName));
	            // normalize text representation
	            doc.getDocumentElement ().normalize ();
	            
	            NodeList listOfPersons = doc.getElementsByTagName("interaction");
	            int totalPersons = listOfPersons.getLength();
	            totNumRecords = totalPersons;
          
	            for(int s=saveIndex; next_person_found == false  ; s++){
	            	
	                Node firstPersonNode = listOfPersons.item(s);
	                if(firstPersonNode.getNodeType() == Node.ELEMENT_NODE){
	                	NodeList listOfChildren =  firstPersonNode.getChildNodes();
	                	for (int i = 0; i<listOfChildren.getLength(); i++)
	                	{
	                		
	                		Node node = listOfChildren.item(i);
	                		
	                		if (node.getNodeType()== Node.ELEMENT_NODE )
	                		{
	                			
	                			NodeList childNodeList = 
	              	        	node.getChildNodes();
	                			String strNode = node.getNodeName().toString();
	                			
	                			if (s > listOfPersons.getLength()- 2)
	        	            	{
	        	            		next_person_found = true;
	        	            	}
	                			if (strNode.equals("sourceLanguage"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList thirdNameList = firstPersonElement.getElementsByTagName("sourceLanguage");
	                			    Element thirdNameElement = (Element)thirdNameList.item(0);
	                				NodeList textFmList = thirdNameElement.getChildNodes();
	                				sourceLanguageString  = ((Node)textFmList.item(0)).getNodeValue().trim();
	                				if ( bothLanguages && !translationString.equals("")) {
	                					saveIndex = s;
	                					next_person_found = true;
	                				}
	                					
	                				if (savelanguageName.equals(sourceLanguageString)&& !translationString.equals("") ) {
	                					languageCounter++;
	                				}
	                				if (savelanguageName.equals(sourceLanguageString)&& languageCounter > 1 
	                						&& !translationString.equals("")) {
	                					  saveIndex = s;
	                					 
	                					  next_person_found = true;
	                				}
	                			}		
	                			else if (strNode.equals("wavfile"))
	                			{
	                				
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList firstNameList = firstPersonElement.getElementsByTagName("wavfile");
	                			   
	                			    Element firstNameElement = (Element)firstNameList.item(0);
	                				NodeList textFNList = firstNameElement.getChildNodes();
	                				
	                				fileString = ((Node)textFNList.item(0)).getNodeValue().trim();
	                				//System.out.println("fileString "+fileString);
	                			}
	                			
	                       			if (strNode.equals("primaryRecognitionResult"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                				NodeList secondNameList = firstPersonElement.getElementsByTagName("primaryRecognitionResult");
	                				
	 		                		Element secondNameElement = (Element)secondNameList.item(0);
		                			NodeList textFlList = secondNameElement.getChildNodes();
		                			if (textFlList.item(0) instanceof Text )
		                			{
		                			
		                			 translationString  = ((Node)textFlList.item(0)).getNodeValue().trim();
		                				                			
		                			 }
	                			  }
	                			
	                			}
	                			
	                		}
	                	}
	                	
	                }
	         
	           
	  	   }catch (SAXParseException err) {
	        System.out.println ("** Parsing error" + ", line " 
	             + err.getLineNumber () + ", uri " + err.getSystemId ());
	        System.out.println(" " + err.getMessage ());

	        }catch (SAXException e) {
	        Exception x = e.getException ();
	        ((x == null) ? e : x).printStackTrace ();

	        }catch (Throwable t) {
	        t.printStackTrace ();
	        }
	       // System.exit (0);

	  }
 	   
	public void readXmlLastRecord() {
		 try {
			 	fullPathName = dirName + xmlFileNameIn;
	  			
	            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
	            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
	            Document doc = docBuilder.parse (new File(fullPathName));
	            // normalize text representation
	            doc.getDocumentElement ().normalize ();
	           // System.out.println ("Root element of the doc is " + 
	                // doc.getDocumentElement().getNodeName());
	            
	            NodeList listOfPersons = doc.getElementsByTagName("interaction");
	            int totalPersons = listOfPersons.getLength();
	            totNumRecords = totalPersons;
	           // System.out.println("Total no of people : " + totalPersons);
	          
	            for(int s=0; s<listOfPersons.getLength() ; s++){
	            
	           
	            //	for(int s=22; previous_person_found == false  ; s++){
	            	
	                Node firstPersonNode = listOfPersons.item(s);
	                if(firstPersonNode.getNodeType() == Node.ELEMENT_NODE){
	                	NodeList listOfChildren =  firstPersonNode.getChildNodes();
	                	for (int i = 0; i<listOfChildren.getLength(); i++)
	                	{
	                		
	                		Node node = listOfChildren.item(i);
	                		
	                		if (node.getNodeType()== Node.ELEMENT_NODE )
	                		{
	                			
	                			NodeList childNodeList = 
	              	        	node.getChildNodes();
	                			String strNode = node.getNodeName().toString();
	                       		
	                			if (strNode.equals("sourceLanguage"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList thirdNameList = firstPersonElement.getElementsByTagName("sourceLanguage");
	                			    Element thirdNameElement = (Element)thirdNameList.item(0);
	                				NodeList textFmList = thirdNameElement.getChildNodes();
	                				sourceLanguageString  = ((Node)textFmList.item(0)).getNodeValue().trim();
	                				if (savelanguageName.equals(sourceLanguageString)&& !translationString.equals("") ) {
	                					languageCounter++;
	                				}
	                				if (savelanguageName.equals(sourceLanguageString)&& languageCounter > 1 
	                						&& !translationString.equals("")) {
	                					  saveIndex = s;
	                					  previous_person_found = true;
	                					 
	                				}
	                			}		
	                			else if (strNode.equals("wavfile"))
	                			{
	                				
	                				Element firstPersonElement = (Element)firstPersonNode;
	                			    NodeList firstNameList = firstPersonElement.getElementsByTagName("wavfile");
	                			   
	                			    Element firstNameElement = (Element)firstNameList.item(0);
	                				NodeList textFNList = firstNameElement.getChildNodes();
	                				
	                				if (savelanguageName.equals(sourceLanguageString))
	                				{
	                					fileString = ((Node)textFNList.item(0)).getNodeValue().trim();
	                					sfileInt = s;
	                					
	                				}
	                			}
	                			
	                			else if (strNode.equals("primaryRecognitionResult"))
	                			{
	                				Element firstPersonElement = (Element)firstPersonNode;
	                				NodeList secondNameList = firstPersonElement.getElementsByTagName("primaryRecognitionResult");
	                				
	 		                		Element secondNameElement = (Element)secondNameList.item(0);
		                			NodeList textFlList = secondNameElement.getChildNodes();
		                			if (textFlList.item(0) instanceof Text && savelanguageName.equals(sourceLanguageString))
		                			{
		                			 PrevTranslation = translationString;
		                			 translationString  = ((Node)textFlList.item(0)).getNodeValue().trim();
		                			 stranslationInt = s;
		                				
		                			 }
	                			  }
	                			
	                			}
	                			
	                		}
	                	}
	                	
	                }
	         
	           
	  	   }catch (SAXParseException err) {
	        System.out.println ("** Parsing error" + ", line " 
	             + err.getLineNumber () + ", uri " + err.getSystemId ());
	        System.out.println(" " + err.getMessage ());

	        }catch (SAXException e) {
	        Exception x = e.getException ();
	        ((x == null) ? e : x).printStackTrace ();

	        }catch (Throwable t) {
	        t.printStackTrace ();
	        }
	       // System.exit (0);

	  }
	
	 
	 public void checkUpdates() {
		 saveInputArea = "";
		 readTextFile(); 
		 checkIfFile();
	 }
	 
	 
	
	 public void getContents() {
		 fullPathName = dirName + xmlFileNameOut;
		    //...checks on aFile are elided
		    StringBuffer contents = new StringBuffer();

		    //declared here only to make visible to finally clause
		    BufferedReader input = null;
		    try {
		      //use buffering, reading one line at a time
		      //FileReader always assumes default encoding is OK!
		      input = new BufferedReader( new FileReader( fullPathName) );
		      line = null; //not declared within while loop
		      recCount = 0;
		      /*
		      * readLine is a bit quirky :
		      * it returns the content of a line MINUS the newline.
		      * it returns null only for the END of the stream.
		      * it returns an empty String if two newlines appear in a row.
		      */
		      while (( line = input.readLine()) != null){
		    	  System.out.println("line "+line);
		    	  //System.out.println("contentsTable[recCount] "+contentsTable[recCount]);
		    	  contentsTable[recCount] = line;
		    	  compareRecord();
		    	  recCount++;
		       
		      }
		    }
		    catch (FileNotFoundException ex) {
		      ex.printStackTrace();
		    }
		    catch (IOException ex){
		      ex.printStackTrace();
		    }
		    finally {
		      try {
		        if (input!= null) {
		          //flush and close both "input" and its underlying FileReader
		          input.close();
		        }
		      }
		      catch (IOException ex) {
		        ex.printStackTrace();
		      }
		    }
		    
		  }
		 

		public void checkIfZeroRecords() {
			if (recCount == 0)
			{
				readInPutFile();
				setContents();
			}
		}
		
		 public void readInPutFile() {
				fullPathName = dirName + xmlFileNameIn;
			
			    //...checks on aFile are elided
			    StringBuffer contents = new StringBuffer();

			    //declared here only to make visible to finally clause
			    BufferedReader input = null;
			    try {
			      //use buffering, reading one line at a time
			      //FileReader always assumes default encoding is OK!
			      input = new BufferedReader( new FileReader(fullPathName ) );
			      line = null; //not declared within while loop
			      recCount = 0;
			      /*
			      * readLine is a bit quirky :
			      * it returns the content of a line MINUS the newline.
			      * it returns null only for the END of the stream.
			      * it returns an empty String if two newlines appear in a row.
			      */
			      while (( line = input.readLine()) != null){
			    	  contentsTable[recCount] = line;
			    	  recCount++;
			      }
			    }
			    catch (FileNotFoundException ex) {
			      ex.printStackTrace();
			    }
			    catch (IOException ex){
			      ex.printStackTrace();
			    }
			    finally {
			      try {
			        if (input!= null) {
			          //flush and close both "input" and its underlying FileReader
			          input.close();
			        }
			      }
			      catch (IOException ex) {
			        ex.printStackTrace();
			      }
			    }
			    
			  } 
		 public void readTextFile() {
				fullPathName = dirName + xmlFileNameOut;
			
			    //...checks on aFile are elided
			    StringBuffer contents = new StringBuffer();

			    //declared here only to make visible to finally clause
			    BufferedReader input = null;
			    try {
			      //use buffering, reading one line at a time
			      //FileReader always assumes default encoding is OK!
			      input = new BufferedReader( new FileReader(fullPathName ) );
			      line = null; //not declared within while loop
			      recCount = 0;
			      /*
			      * readLine is a bit quirky :
			      * it returns the content of a line MINUS the newline.
			      * it returns null only for the END of the stream.
			      * it returns an empty String if two newlines appear in a row.
			      */
			      while (( line = input.readLine()) != null){
			    	  contentsTable[recCount] = line;
			    	  recCount++;
			      }
			    }
			    catch (FileNotFoundException ex) {
			    	System.out.println("creating output file");
			      //ex.printStackTrace();
			    }
			    catch (IOException ex){
			      ex.printStackTrace();
			    }
			    finally {
			      try {
			        if (input!= null) {
			          //flush and close both "input" and its underlying FileReader
			          input.close();
			        }
			      }
			      catch (IOException ex) {
			        ex.printStackTrace();
			      }
			    }
			    
			  } 
	 public void compareRecord()
	 {
		 for ( int i = 0 ; saveAreaFile[i]!= null ; i++ )
		 {
		 String b = saveAreaFile[i];
		//System.out.println("b "+b );
		System.out.println("contentsTable[recCount] "+contentsTable[recCount]);
		 	if (contentsTable[recCount].indexOf(b) != -1) {
		 		recCount++;
		 		System.out.println("saveAreaSentence[i] "+saveAreaSentence[i]);
		 		contentsTable[recCount] = "<transcription>"+saveAreaSentence[i]+"</transcription>";
		 	}
		 }
	 }
	 public void checkIfFile() {
		 
		 String b = "C:";
		 for (  checkIndex = 0 ; contentsTable[checkIndex]!= null ; checkIndex++ ) {
			if (contentsTable[ checkIndex ].indexOf(b) != -1) {
				compareUpdateFileWithInputRecord();
			}
		 }
	 }
	
	 
		
	public void compareUpdateFileWithInputRecord() {
		 String b = fileString;
		 	if (contentsTable[checkIndex].indexOf(b) != -1) {
		 		int tempIndex = checkIndex + 1;
		 		 moveNewInputField();
		 	}
	 }
	
	 public void moveNewInputField() {
		 checkIndex++;
		 String b = "<transcription>";
		 if (contentsTable[checkIndex].indexOf(b) != -1) {
			 startPos = contentsTable[checkIndex].indexOf(b);
			 startPos = startPos + 14;
			 String c = "</transcription>";
			 endPos = contentsTable[checkIndex].indexOf(c);
			 saveInputArea = contentsTable[checkIndex].substring(startPos,endPos);
		 }
	 }
	 public void FindInputRecord() {
		 inputRecordFound = false;
		 String b =  fileString;
			 for (  checkIndex = 0 ; inputRecordFound == false ; checkIndex++ ) {
				if (contentsTable[ checkIndex ].indexOf(b) != -1) {
					//System.out.println("contentsTable[ checkIndex ] input "+contentsTable[ checkIndex ]);
					checkIndex = checkIndex + 1;
					saveCheckIndex = checkIndex;
					inputRecordFound = true;
				}
			 }
		 }
	 
	 public void findNextUnjudgedRecord() {
		 unjudgedRecordFound = false;
		 for (  checkIndex = saveCheckIndex ; unjudgedRecordFound == false ; checkIndex++ ) {
			 String b = "C:";
			 if (contentsTable[ checkIndex ].indexOf(b) != -1) {
				 saveFileNextUnjudged = contentsTable[ checkIndex ];
				 int tempIndex = checkIndex + 1;
				 String c = "</transcription>";
				 	if (contentsTable[tempIndex].indexOf(c) != -1) {
				 		}
				 		else
				 		{
				 			//System.out.println("contentsTable[checkIndex] next unjudged "+  contentsTable[checkIndex]);
				 			checkLanguge();
				 			if (languageNameIsCorrect)
				 			{
				 				saveCheckIndex = checkIndex;
				 				unjudgedRecordFound = true;
				 				createNextUnjudgedRecord();
				 			}
				 		}
				 	}
				 }
			}
		 
	 public void checkLanguge() {
		 languageNameIsCorrect = false;
		 if (languageName.equals("English")){
			 String b = "American English";
			 if (contentsTable[ checkIndex ].indexOf(b) != -1) {
				 languageNameIsCorrect = true; 
			 }
		 }
		 else if (languageName.equals("Spanish")){
			 String b = "American Spanish"; 
			 if (contentsTable[ checkIndex ].indexOf(b) != -1) {
				 languageNameIsCorrect = true; 
			 }
		 }
		 else if (languageName.equals("English and Spanish")){
			 String b = "American "; 
			 if (contentsTable[ checkIndex ].indexOf(b) != -1) {
				 languageNameIsCorrect = true; 
			 }
		 }
	 }
	 
	  public void createNextUnjudgedRecord() {
		  unjudgedRecorCreated = false;
		  for (  checkIndex = saveCheckIndex ; unjudgedRecorCreated == false ; checkIndex++ ) {
			 String e = "translationRequest";
			 if (contentsTable[checkIndex].indexOf(e) != -1) {
			 startPos = contentsTable[checkIndex].indexOf(e);
			 startPos = startPos + 19;
			 String d = "</translationRequest>";
			 endPos = contentsTable[checkIndex].indexOf(d);
			 translationString = contentsTable[checkIndex].substring(startPos,endPos);
			 inputField.setText(translationString);
			 fileString = saveFileNextUnjudged; 
			 writeTranscriptionItems();
			 unjudgedRecorCreated = true;
			 }
		}
	  }
	  public void setContents()
	  {
	  try {
		  fullPathName = dirName + xmlFileNameOut;
	        BufferedWriter out = new BufferedWriter(new FileWriter(fullPathName));
	        recCount = 0;
	        while (contentsTable[recCount]!= null){
	        out.write(contentsTable[recCount] + "\n");
	        
	        recCount++;
	        }
	        out.close();
	    } catch (IOException e) {
	    }
	  }
	  
	public static void main(String[] args) {
		if (args.length == 1) {
			String fileName = args[0];
			System.out.println("Arg = " + fileName + "\n"); 
			new JudgeGUI(fileName);
			//setvalueParse(parseval);
		}
		else {
			System.out.println("Usage: java JudgeGui.JudgeGui Dir>");
		}
	}
  }

	
	 
	



