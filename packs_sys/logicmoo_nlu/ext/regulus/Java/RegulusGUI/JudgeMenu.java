package RegulusGUI;
import se.sics.prologbeans.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

public class JudgeMenu extends JFrame

{
	private RegulusGUI regulusWindow = null;
	private Frame3 frame3 = null;
	private CreateFrame3 createframe3 = null;
	private JudgeMenu judgemenu = null;
	private int Judgeind = 0;
	private boolean record_matched = false;
	private boolean record_is_translation = false; 
	private int length = 0;
	private String UpdateTranslationJudgementsString = "UPDATE_TRANSLATION_JUDGEMENTS";
	private String UpdateTranslationJudgementsSpeechString = "UPDATE_TRANSLATION_JUDGEMENTS_SPEECH";
	private String ReadKey = "";
	private String SpaceString = " ";
	private String HoldStringCorpus = "";
	private String JudgeCorpusString = "JUDGE_CORPUS";
	private String JudgeCorpusSpeechString = "JUDGE_SPEECH_CORPUS";
	private String holdSentence = "";
	private String[] judge_items =  new String[10];
	private int judgeIndex = 0;
	private  String JudgeMenuString = "";
	private JButton buttonThree;

	
//	 set the pointer to the CreateFrame3 window
	  public void setFrame3(CreateFrame3 createwindow) {
		  createframe3 = createwindow;
	  }
	public JudgeMenu()
	{

	}
	
	public JudgeMenu(Frame3 frame3window,CreateFrame3 createframe,RegulusGUI regulus)
	  {
		frame3 = frame3window;
		regulusWindow = regulus;
		createframe3 = createframe;
	  }
	
	
	public void createJudgeMenuItemsTable()
	{
		 for (int i = 1 ; i < regulusWindow.num_Available + 1 ; i++ )
		 {
			holdSentence = regulusWindow.availableCommands[i];
			length = holdSentence.length();
		 if (holdSentence.lastIndexOf(UpdateTranslationJudgementsSpeechString) != -1) {
			 TranslateJudgeSpeechDefaultOrNot();		
		 	}
			
		else if (holdSentence.lastIndexOf(UpdateTranslationJudgementsString) != -1){
			TranslateJudgeDefaultOrNot();
		 	}
		 }

	 }

	public void TranslateJudgeDefaultOrNot()
	{
		if (length > 29)
		{
			TranslateJudgeDecideKey();
			
		}
		else
		{
			//frame3.judgeIndex = frame3.judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_CORPUS";
		}
	}
	public void TranslateJudgeDecideKey()
	{
		if ( holdSentence.substring(29,30).equals(SpaceString))
		{
			ReadKey = holdSentence.substring(30,length);
			judgeIndex = judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_CORPUS " + ReadKey;
		}
		else
		{
			ReadKey = holdSentence.substring(29,length);
			judgeIndex = judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_CORPUS " + ReadKey;
		}
	}
	public void TranslateJudgeSpeechDefaultOrNot()
	{
		if (length > 36)
		{
			TranslateJudgeSpeechDecideKey();
			
		}
		else
		{
			judgeIndex = judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_SPEECH_CORPUS";
			
		}
	}
	public void TranslateJudgeSpeechDecideKey()
	{
		if ( holdSentence.substring(36,37).equals(SpaceString))
		{
			ReadKey = holdSentence.substring(37,length);
			judgeIndex = judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_SPEECH_CORPUS " + ReadKey;
		}
		else
		{
			ReadKey = holdSentence.substring(36,length);
			judgeIndex = judgeIndex + 1;
			judge_items[judgeIndex] = "JUDGE_SPEECH_CORPUS " + ReadKey;
		}
	}
public void createJudgeMenu(String FromProg)
{
Judgeind = judgeIndex; 	
Judgeind = Judgeind + 1;
for (judgeIndex = 0; judgeIndex < Judgeind; judgeIndex++ )
	{
	 showJudgeMenuItems();
	}
}
public void createJudgeMenuString(String FromProg)
{
Judgeind = judgeIndex; 	
Judgeind = Judgeind + 1;
for (judgeIndex = 0; judgeIndex < Judgeind; judgeIndex++ )
	{
	 showJudgeMenuItemsCreate();
	}
}
public void showJudgeMenuItems()
{
	 frame3.judge_menuitem[judgeIndex] = new  JMenuItem(judge_items[judgeIndex]); 
	 frame3.judge_menuitem[judgeIndex].addActionListener(
			 new ActionListener() {
				 public void actionPerformed( ActionEvent e)
		 			{
					 for (judgeIndex = 0; judgeIndex <  Judgeind ; judgeIndex++ ) 
		 				{
						
	 					if (e.getSource() == frame3.judge_menuitem[judgeIndex])
		 					{ 
		 					JudgeMenuString = frame3.judge_menuitem[judgeIndex].getText();
			 				 CheckWhichJudgeMenu();
		 					}
		 				}
					
		 			}
				 }
			 );
	 checkIfJudgeItemsOrNot();
	 frame3.JudgeMenus.add(frame3.judge_menuitem[judgeIndex]);
}

public void checkIfJudgeItemsOrNot() {
	if (Judgeind == 1)
	{
		frame3.JudgeMenus.setEnabled(false);
	}
	else
	{
		frame3.JudgeMenus.setEnabled(true);
	}
}
public void showJudgeMenuItemsCreate()
{
	 createframe3.judge_menuitem[judgeIndex] = new  JMenuItem(judge_items[judgeIndex]); 
	 createframe3.judge_menuitem[judgeIndex].addActionListener(
			 new ActionListener() {
				 public void actionPerformed( ActionEvent e)
		 			{
					 for (judgeIndex = 0; judgeIndex <  Judgeind ; judgeIndex++ ) 
		 				{
						
	 					if (e.getSource() == createframe3.judge_menuitem[judgeIndex])
		 					{ 
		 					JudgeMenuString = createframe3.judge_menuitem[judgeIndex].getText();
			 				 CheckWhichJudgeMenu();
		 					}
		 				}
					
		 			}
				 }
			 );
	 createframe3.JudgeMenus.add(createframe3.judge_menuitem[judgeIndex]);
}

public void CheckWhichJudgeMenu()
{
	 if (JudgeMenuString.lastIndexOf(JudgeCorpusString) != -1)
	 {
		 judge_corpus_menu_handling();
	 }
	 if (JudgeMenuString.lastIndexOf(JudgeCorpusSpeechString) != -1)
	 {
		 judge_corpus_speech_menu_handling();
	 }
}


public void judge_corpus_menu_handling()
{
	KeyCorpusOrNot();
	CreateAndLinkjudgePane();
}
public void judge_corpus_speech_menu_handling()
{
	KeyCorpusSpeechOrNot();
	CreateAndLinkjudgePane(); 
}	

public void KeyCorpusOrNot()
{
	length = JudgeMenuString.length();
	if (length > 29)
	{
		DecideCorpusKey();
	}
	else
	{
		HoldStringCorpus = "TRANSLATE_CORPUS";
	}
}
public void DecideCorpusKey()
{
	if ( JudgeMenuString.substring(29,30).equals(SpaceString))
	{
		ReadKey = JudgeMenuString.substring(30,length);
		HoldStringCorpus = "TRANSLATE_CORPUS " + ReadKey;
	}
	else
	{
		ReadKey = JudgeMenuString.substring(29,length);
		HoldStringCorpus = "TRANSLATE_CORPUS " + ReadKey;
	}
}
public void KeyCorpusSpeechOrNot()
{
	length = JudgeMenuString.length();
	if (length > 36)
	{
		DecideCorpusSpeechKey();
	}
	else
	{
		HoldStringCorpus = "TRANSLATE_SPEECH_CORPUS";
	}
}
public void DecideCorpusSpeechKey()
{
	if ( JudgeMenuString.substring(29,30).equals(SpaceString))
	{
		ReadKey = JudgeMenuString.substring(30,length);
		HoldStringCorpus = "TRANSLATE_SPEECH_CORPUS " + ReadKey;
	}
	else
	{
		ReadKey = JudgeMenuString.substring(29,length);
		HoldStringCorpus = "TRANSLATE_SPEECH_CORPUS " + ReadKey;
	}
}
public void CreateAndLinkjudgePane()
{
	regulusWindow.judgeCounter++; 
	buttonThree = new JButton("Judge " +regulusWindow.judgeCounter);
	regulusWindow.barPanel.add( buttonThree);
	
	JudgePane judgePane = new JudgePane(judgemenu, regulusWindow,HoldStringCorpus,buttonThree,regulusWindow.judgeCounter );
	judgePane.setRegulusGUI(regulusWindow );
	  //  display new internal window
	JInternalFrame JudgePaneInternalFrame = judgePane.getInternalFrame();
	  //add the internal frame to the desktop
	regulusWindow.desktop.add(JudgePaneInternalFrame);
	JudgePaneInternalFrame.setVisible(true); 
}

}

