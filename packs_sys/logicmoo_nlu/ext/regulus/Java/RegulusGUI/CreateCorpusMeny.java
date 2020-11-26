package RegulusGUI;
import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class CreateCorpusMeny extends JFrame
{
	private RegulusGUI regulusWindow = null;
	private Frame3 frame3 = null;
	private CreateFrame3 createframe3 = null;
	private CorpusPane corpusPane = null;
	//private CreateCorpusMeny createcorpusmeny = null;
	private String Corpus_table[] =  {"TRANSLATE_CORPUS" ,"TRANSLATE_CORPUS alternate",
			"TRANSLATE_SPEECH_CORPUSin_coverage","TRANSLATE_SPEECH_CORPUS_AGAIN",
			"TRANSLATE_SPEECH_CORPUS_AGAINin_coverage"}; 
	public  int Corpusind = 0;
	private String corpusId = "";
	private String corpusString = "";
	private String TranslateCorpusString = "TRANSLATE_CORPUS";
    private String TranslateSpeechCorpusString = "TRANSLATE_SPEECH_CORPUS";
    private String TranslateSpeechCorpusAgainString = "TRANSLATE_SPEECH_CORPUS_AGAIN";
    private int length = 0;
    private String SpaceString = " ";
    private String ReadKey = "";
    private String holdSentence = "";
    private String[] corpus_items = new String[10];
    private int corpusIndex = 0;
    private String CorpusMenuString = "";
    private String holdProgram = "";
    private String holdWhichProg = "";
    private String FromProgHold = "";
    private String CorpusString = "";
    

//	 set the pointer to the CreateFrame3 window
	  public void setFrame3(CreateFrame3 createwindow) {
		  createframe3 = createwindow;
	  }
	  
	
	  
public CreateCorpusMeny()
{

}


public CreateCorpusMeny(Frame3 frame3window,CreateFrame3 createframe,RegulusGUI regulus,String whichProgram)
  {
	frame3 = frame3window;
	regulusWindow = regulus;
	createframe3 = createframe;
	holdProgram = whichProgram;
	
  }
public void createCorpusMenuItemsTable()
{
 	 for (int i = 1 ; i < regulusWindow.num_Available ; i++ )
	 //for (int i = 0 ; i < 5 ; i++ )
	 {
 		holdSentence = regulusWindow.availableCommands[i]; 
		// frame3.holdSentence = Corpus_table[i];
		length = holdSentence.length();
		if (holdSentence.lastIndexOf(TranslateSpeechCorpusAgainString) != -1)
		 {
			TranslateSpeechCorpusAgainKeyOrNot();
		 }
		
		else if (holdSentence.lastIndexOf(TranslateSpeechCorpusString) != -1)
		 {
			TranslateSpeechCorpusKeyOrNot();
		 }
		
		else if (holdSentence.lastIndexOf(TranslateCorpusString) != -1)
		 {
			TranslateCorpusKeyOrNot();
		 }
	 }
}

public void TranslateSpeechCorpusAgainKeyOrNot()
{
	 if (length > 29){
		 TranslateSpeechCorpusAgainDecideKey();
	 }
	 else
	 {
		 Corpusind = Corpusind + 1;
		 corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS_AGAIN";
	 }
}

public void TranslateSpeechCorpusAgainDecideKey()
{
	if (holdSentence.substring(29,30).equals(SpaceString))
	{
		ReadKey = holdSentence.substring(30,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS_AGAIN "+ ReadKey;
	}
	else
	{
		ReadKey = holdSentence.substring(29,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS_AGAIN "+ ReadKey;
	}
}

public void TranslateSpeechCorpusKeyOrNot()
{
	 if (length > 23){
		 TranslateSpeechCorpusDecideKey();
	 }
	 else
	 {
		 Corpusind = Corpusind + 1;
		 corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS";
	 }
}
public void TranslateSpeechCorpusDecideKey()
{
	if (holdSentence.substring(23,24).equals(SpaceString))
	{
		ReadKey = holdSentence.substring(24,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS "+ ReadKey;
	}
	else
	{
		ReadKey = holdSentence.substring(23,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_SPEECH_CORPUS "+ ReadKey;
	}
}

public void TranslateCorpusKeyOrNot()
{
	 if (length > 16){
		 TranslateCorpusDecideKey();
	 }
	 else
	 {
		 //Corpusind = Corpusind + 1;
		 corpus_items[Corpusind] = "TRANSLATE_CORPUS";
	 }
}
public void TranslateCorpusDecideKey()
{
	if (holdSentence.substring(16,17).equals(SpaceString))
	{
		ReadKey = holdSentence.substring(17,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_CORPUS "+ ReadKey;
	}
	else
	{
		ReadKey = holdSentence.substring(16,length);
		Corpusind = Corpusind + 1;
		corpus_items[Corpusind] = "TRANSLATE_CORPUS "+ ReadKey;
	}
}

public void createCorpusMenu(String FromProg)
{
	FromProgHold = FromProg;
	Corpusind = Corpusind + 1;
	frame3.CorpusMenu.setEnabled(false);
for (corpusIndex = 0; corpusIndex < Corpusind; corpusIndex++ )
	{
	 showCorpusMenuItems();
	}
}

public void createCorpusMenuStrings(String FromProg)
{
	FromProgHold = FromProg;
	Corpusind = Corpusind + 1;
for (corpusIndex = 0; corpusIndex < Corpusind; corpusIndex++ )
	{
	 showCorpusMenuItemsCreate();
	}
}
public void showCorpusMenuItems()
{
	 frame3.corpus_menuitem[corpusIndex] = new  JMenuItem(corpus_items[corpusIndex]); 
	 frame3.corpus_menuitem[corpusIndex].addActionListener(
			 new ActionListener() {
				 public void actionPerformed( ActionEvent e)
		 			{
					 for (corpusIndex = 0; corpusIndex <  Corpusind ; corpusIndex++ ) 
		 				{
						
	 					if (e.getSource() == frame3.corpus_menuitem[corpusIndex])
		 					{ 
		 					CorpusMenuString = frame3.corpus_menuitem[corpusIndex].getText();
		 					DecideWichCorpus();
		 					frame3.SetCorpusId(corpusId);
		 					}
		 				}
					
		 			}
				 }
			 );
	
	 checkIfCorpusItemsOrNot();
	 frame3.CorpusMenu.add(frame3.corpus_menuitem[corpusIndex]);
}
public void checkIfCorpusItemsOrNot() {
	if (Corpusind == 1)
	{
		frame3.CorpusMenu.setEnabled(false);
	}
	else
	{
		frame3.CorpusMenu.setEnabled(true);
	}
}
public void showCorpusMenuItemsCreate()
{
	 createframe3.corpus_menuitem[corpusIndex] = new  JMenuItem(corpus_items[corpusIndex]); 
	 createframe3.corpus_menuitem[corpusIndex].addActionListener(
			 new ActionListener() {
				 public void actionPerformed( ActionEvent e)
		 			{
					 for (corpusIndex = 0; corpusIndex <  Corpusind ; corpusIndex++ ) 
		 				{
						
	 					if (e.getSource() == createframe3.corpus_menuitem[corpusIndex])
		 					{ 
		 					CorpusMenuString = createframe3.corpus_menuitem[corpusIndex].getText();
		 					DecideWichCorpus();
		 					createframe3.SetCorpusId(corpusId);
		 					}
		 				}
					
		 			}
				 }
			 );
	 createframe3.CorpusMenu.setEnabled(true);
	 createframe3.CorpusMenu.add(createframe3.corpus_menuitem[corpusIndex]);
}
public void DecideWichCorpus()
{
	regulusWindow.handleCommand("TRANSLATE_TRACE_OFF");
	  if(CorpusMenuString.lastIndexOf(TranslateSpeechCorpusAgainString) != -1) {
		 TranslateSpeechCorpusAgainDefaultOrNot();
		 System.out.println("String Corpus contains TranslateSpeechCorpusAgainString");
	  }
	  else if (CorpusMenuString.lastIndexOf(TranslateSpeechCorpusString) != -1) {
		 TranslateSpeechCorpusDefaultOrNot();
		 System.out.println("String Corpus contains TranslateSpeechCorpusString");
	  }
	  else if (CorpusMenuString.lastIndexOf(TranslateCorpusString) != -1) {
		 TranslateCorpusDefaultOrNot();
		 System.out.println("String Corpus contains TranslateCorpusString");
			 
	  }
}

public String TranslateCorpusDefaultOrNot()
{
	length =CorpusMenuString.length();
	if (length > 16)
	{
		 translate_corpus_arg_menu_handling();
		 return CorpusString;
	}
	else
	{
		 translate_corpus_menu_handling();
		 CorpusString = "TRANSLATE_CORPUS";
		 return CorpusString;
	}
}

public String TranslateSpeechCorpusDefaultOrNot()
{
	length =CorpusMenuString.length();
	if (length > 23)
	{
		 translate_speech_corpus_arg_menu_handling();
		  return CorpusString;
	}
	else
	{
		 translate_speech_corpus_menu_handling();
		 CorpusString = "TRANSLATE_SPEECH_CORPUS";
		 return CorpusString;
	
	}
}

public String TranslateSpeechCorpusAgainDefaultOrNot()
{
	length =CorpusMenuString.length();
	if (length > 29)
	{
		 translate_speech_corpus_again_arg_menu_handling();
		  return CorpusString;
	}
	else
	{
		 translate_speech_corpus_Again_menu_handling();
		 CorpusString = "TRANSLATE_SPEECH_CORPUS_AGAIN";
		 return CorpusString;
	}
}

public void translate_corpus_menu_handling()
{
	corpusString = "TRANSLATE_CORPUS";
	createCorpusBackGroundJob(corpusString);
}
public void translate_corpus_arg_menu_handling()
{
	corpusString = CorpusMenuString;
	createCorpusBackGroundJob(corpusString);
}	

public void translate_speech_corpus_menu_handling()
{	
	corpusString = CorpusMenuString;
	createCorpusBackGroundJobForSpeech(corpusString);
}

public void translate_speech_corpus_arg_menu_handling()
{
	
	 corpusString = CorpusMenuString;
	 createCorpusBackGroundJobForSpeech(corpusString);
	 
}	
public void translate_speech_corpus_Again_menu_handling()
{
	corpusString = "TRANSLATE_SPEECH_CORPUS_AGAIN";
	createCorpusBackGroundJob(corpusString);
}
public void translate_speech_corpus_again_arg_menu_handling()
{
	 corpusString = CorpusMenuString;
	 createCorpusBackGroundJob(corpusString);
	 
}
public void createCorpusBackGroundJob(String strCorpus)
{ 
	String CorpusIdent = strCorpus;
	System.out.println("CorpusIdent "+CorpusIdent);
	regulusWindow.is_this_list_of_command = false;
	new corpusInBackGround(frame3, regulusWindow,CorpusIdent).start();
	
	new ReadOneCorpusFile(this,frame3, regulusWindow,CorpusIdent,CorpusMenuString).start();
}
public void createCorpusBackGroundJobForSpeech(String strCorpus)
{ 
	String CorpusIdent = strCorpus;
	regulusWindow.is_this_list_of_command = false;
	new corpusInBackGround(frame3, regulusWindow,CorpusIdent).start();
	
	new ReadTwoCorpusFiles(this,frame3, regulusWindow,CorpusIdent).start();
	
	}
public void CreateAndLinkcorpusPane(String StrCorpus)
{
	    CorpusPane corpusPane = new CorpusPane(frame3, regulusWindow, StrCorpus);
	  	corpusPane.setRegulusGUI(regulusWindow );
	  //  display new internal window
	    JInternalFrame CorpusPaneInternalFrame = corpusPane.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(CorpusPaneInternalFrame);
		CorpusPaneInternalFrame.setVisible(true); 
}


}
