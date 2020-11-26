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
import org.w3c.dom.Text;


public class Frame2  extends JFrame implements  VetoableChangeListener,ActionListener{
	
	private JInternalFrame frame2;
	private JudgeGUI judgeWindow = null;
	public  JInternalFrame[] allFrames = null;
	private JMenuBar bar = new JMenuBar();  // create menubar
	private JMenuItem HelpMenu;
	private JMenu transcriptMenu;
	private JMenu viewMenu;
	private JMenuItem TranscriptMenu;
	private JButton next = new JButton("Next Record");
	private JButton previous = new JButton("Previous Record");
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
    private JLabel lTranslationRequest = new JLabel("RecognitionResult");
    private JLabel lTranslationResult = new JLabel("TranslationResult");
    private JLabel lLastEnglish = new JLabel("Last English Question");
    private JLabel lLastSpanish = new JLabel("Last Spanish Sentence");
   // private JCheckBoxMenuItem[] DisplayCheckBoxes = new JCheckBoxMenuItem[2];
    private JLabel DisplayLabels[] = {lTranslationRequest,lTranslationResult, lLastEnglish, lLastSpanish};
    private JScrollPane[] DisplayScrollPanes = new JScrollPane[4];
    private JTextArea[] DisplayTextPanes = new JTextArea[4];
    private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
    private final String[] DisplayLabelText = {"TranscriptionResult","TranslationResult","Last English Question",
    		"Last Spanish Sentence"};
    private String[] languageStrings = { "English - Spanish", "Spanish - English" ,"English - English (backtranslation)",
    		"Spanish - Spanish (backtranslation)" };
    private String[] judgeStrings = { "good", "bad" ,"ok",};
    private String languageName = "";
    private boolean  bothLanguages = false;
    private String savelanguageNameSource = "";
    private String savelanguageNameTarget = "";
    private String dirName = "";
    private String xmlFileNameOut = "/medslt-logFinal.xml";
    private String xmlFileNameIn = "/medslt-logout.xml";
    private String fullPathName = "";
    private int totNumRecords = 0;
    private int saveIndex = 0;
    private boolean person_found = false;
    private String fileString = ""; 
     private String sourceLanguageString = ""; 
    private String savelanguageName = "";
    private int languageCounter = 0;
    private String primaryrecognition ="";
    private String translationresult = "";
    private String transcription = "";
    private JCheckBoxMenuItem[] DisplayCheckBoxes = new JCheckBoxMenuItem[4];
    private String line = null;
    private int recCount = 0;
    private String contentsTable[] = new String[1000];
    
    public JInternalFrame getInternalFrame() {
  	  return frame2;
  	  }
    
      // get pointer to Regulus window
	  public JudgeGUI getJudgeGUI() {
		  return judgeWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setJudgeGUI(JudgeGUI window) {
		  judgeWindow = window;
	  }
	public Frame2(JPanel deskTopBarPanel) {
		
	}
	public Frame2(String directoryName) {
		
		 frame2 = new JInternalFrame("Judging",true,true,true,true);
		 
		 //		 create menubar for internal frame
		 dirName = directoryName;
		 
		    setJMenuBar( bar );  // set the menubar for the JInternalFrame
		    
		    CreateScreen();
		    
			Container c2 = frame2.getContentPane();
			
			frame2.addVetoableChangeListener(this);
			
			inputPanel.add(bar,BorderLayout.NORTH);
			c2.add(inputPanel,BorderLayout.NORTH  );
			c2.add(displayPanel);
			setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
			frame2.pack();
			
	}
    
	public void CreateScreen()
	{
		  // set up the layout
		  gblayout = new GridBagLayout();
		  
		  // instansiate the gridbag constraints
		  gbConstraints = new GridBagConstraints();
		  gbConstraints.fill = GridBagConstraints.HORIZONTAL;
		  displayPanel.setLayout(gblayout);
		  
		  viewMenu = new JMenu("View");
		  viewMenu.setMnemonic('V');
		  bar.add(viewMenu);
		  
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
		  

		  
		  // Create the Combobox, select the defauls item
		  JComboBox languageList = new JComboBox(languageStrings);
		  languageList.setSelectedIndex(0);
		  languageList.addActionListener(this);
		  gbConstraints.gridx = 0;
		  gbConstraints.gridy = 1;
		  displayPanel.add(languageList,gbConstraints);
		  
		  // Create the Combobox, select the defauls item
		  JComboBox judgeList = new JComboBox(judgeStrings);
		  judgeList.setSelectedIndex(0);
		  judgeList.addActionListener(this);
		  gbConstraints.gridx = 1;
		  gbConstraints.gridy = 1;
		  displayPanel.add(judgeList,gbConstraints);
		  
//			Add labels and textboxes
		  
		  createTestArea();
		  
		  languageName = "Spanish - English";
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
	}
	
	 public void createTestArea()
	  {
		 
		  for (index = 0; index < 4; index++ ) 
		  {
			  createTest();
			  createCheckBoxes();
		  }
	  }
	 
	  public void createCheckBoxes()
	  {
	  DisplayCheckBoxes[index] = new JCheckBoxMenuItem(DisplayLabelText[index],true);
	  DisplayCheckBoxes[index].addItemListener(new AnswerCheckBoxHandler());
	  viewMenu.add(DisplayCheckBoxes[index]);
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
	  
	  // listen to the comboBox
	  public void actionPerformed(ActionEvent e) {
		JComboBox cb = (JComboBox)e.getSource();
		languageName = (String)cb.getSelectedItem();
	  }
	  
	  public void populateScreenAfterLanguageSelection()
	  {
		  if (languageName.equals("English - Spanish")){
			  savelanguageNameSource = "American English";
			  savelanguageNameTarget = "American Spanish";
			  DisplayCheckBoxes[2].setSelected(false); 
			  processFirstRecord();
			 
		  }
		  else if (languageName.equals("Spanish - English")){
			  savelanguageNameSource = "American Spanish";
			  savelanguageNameTarget = "American English";
			  DisplayCheckBoxes[3].setSelected(false); 
			  processFirstRecord();
		  }
		  else if (languageName.equals("English - English (backtranslation)")){
			  savelanguageNameSource = "American English";
			  DisplayCheckBoxes[2].setSelected(false); 
			  processFirstRecord();
		  }
		  else if (languageName.equals("Spanish - Spanish (backtranslation)")){
			  savelanguageNameSource = "American Spanish";
			  DisplayCheckBoxes[3].setSelected(false); 
			  processFirstRecord();
		  }
	  }
	  
	  public void populateScreenAfterNextSelection() {
		  if (languageName.equals("English - Spanish")){
			  savelanguageNameSource = "American English";
			  savelanguageNameTarget = "American Spanish";
			  DisplayCheckBoxes[2].setSelected(false); 
			 
			 
		  }
		  else if (languageName.equals("Spanish - English")){
			  savelanguageNameSource = "American Spanish";
			  savelanguageNameTarget = "American English";
			  DisplayCheckBoxes[3].setSelected(false); 
			 
		  }
		  else if (languageName.equals("English - English (backtranslation)")){
			  savelanguageNameSource = "American English";
			  DisplayCheckBoxes[2].setSelected(false); 
			 
		  }
		  else if (languageName.equals("Spanish - Spanish (backtranslation)")){
			  savelanguageNameSource = "American Spanish";
			  DisplayCheckBoxes[3].setSelected(false); 
			  
		  }
	  }
	  public void processFirstRecord() {
		  languageCounter = 0;
		  person_found = false;
		  transcription = "";
		  readXmlcheckLanguage();
		  checkIfTranscriptionExists();
		  //checkUpdates();
		  //checkEditRecord();
		  //checkWhichSentence();
		  //checkEditSentence();
		  writeTranscriptionItems();
	  }
	  public void checkIfTranscriptionExists() {
		 if (transcription.equals("")){
			
		 }
		 else
		 {
			 primaryrecognition = transcription; 
		 }
	  }
	  
	  public void processNextRecord() {
		  //readXmlNextRecord();
		 // checkUpdates();
		 // checkEditRecord();
		 // checkWhichSentence();
		 // checkEditSentence();
		  //inputField.setText(translationString);
		  writeTranscriptionItems();
		
	  }
	  public void writeTranscriptionItems()
	  {
		  DisplayTextPanes[0].setText((primaryrecognition));
		  DisplayTextPanes[0].setCaretPosition(0); 
		  DisplayTextPanes[1].setText((translationresult));
		  DisplayTextPanes[1].setCaretPosition(0);
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
	
	  public void setAddArea()
		{
		  displayPanel.add(DisplayScrollPanes[index],gbConstraints);
		}
	  
	  public void setCheckBoxes()
	  {
		  DisplayCheckBoxes[0].setSelected(true); 
		  DisplayCheckBoxes[1].setSelected(true);
		  DisplayCheckBoxes[2].setSelected(true); 
		  DisplayCheckBoxes[3].setSelected(true); 
	  
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
		                				System.out.println("savelanguageNameSource "+savelanguageNameSource );
		                				System.out.println("sourceLanguageString "+sourceLanguageString );
		                				if (savelanguageNameSource.equals(sourceLanguageString) ) {
		                					languageCounter++;
		                					person_found = true;
		                				}
		                			
		                			}		
	 	                			else if (strNode.equals("primaryRecognitionResult"))
		                			{
		                				
		                				Element firstPersonElement = (Element)firstPersonNode;
		                			    NodeList firstNameList = firstPersonElement.getElementsByTagName("primaryRecognitionResult");
		                			   
		                			    Element firstNameElement = (Element)firstNameList.item(0);
		                				NodeList textFNList = firstNameElement.getChildNodes();
		                				primaryrecognition  = ((Node)textFNList.item(0)).getNodeValue().trim();

		                			}
		                			
		                       			if (strNode.equals("translationResult"))
		                			{
		                				Element firstPersonElement = (Element)firstPersonNode;
		                				NodeList secondNameList = firstPersonElement.getElementsByTagName("translationResult");
		                				
		 		                		Element secondNameElement = (Element)secondNameList.item(0);
			                			NodeList textFlList = secondNameElement.getChildNodes();
			                			if (textFlList.item(0) instanceof Text )
			                			{
			                				translationresult  = ((Node)textFlList.item(0)).getNodeValue().trim();
			                			 System.out.println("translationresult "+translationresult  );
			                			}
		                			  }
		                       			if (strNode.equals("transcription"))
			                			{
			                				Element firstPersonElement = (Element)firstPersonNode;
			                				NodeList fourthNameList = firstPersonElement.getElementsByTagName("transcription");
			                				
			 		                		Element fourthNameElement = (Element)fourthNameList.item(0);
				                			NodeList textFxList = fourthNameElement.getChildNodes();
				                			if (textFxList.item(0) instanceof Text )
				                			{
				                				transcription  = ((Node)textFxList.item(0)).getNodeValue().trim();
				                			 System.out.println("transcription "+transcription  );
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
	 	public void readTextFile() {
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
	 	 class AnswerCheckBoxHandler implements ItemListener
		  {
			  public void itemStateChanged(ItemEvent event)
			  {
				  for (index = 0; index < 4; index++ ) {
					  if (event.getSource() == DisplayCheckBoxes[index]){
						  DisplayLabels[index].setVisible(DisplayCheckBoxes[index].isSelected());
						  DisplayScrollPanes[index].setVisible(DisplayCheckBoxes[index].isSelected());
					  }
				  }
					 
			  }
		  }
}
