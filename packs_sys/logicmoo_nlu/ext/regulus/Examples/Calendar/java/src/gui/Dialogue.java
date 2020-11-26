package gui;

import java.util.Observable;
import java.util.Vector;

import net.sf.regulus.NBestRegResult;

/*
 This class models the values that the application displays. 
 The CalendarPanel class depends on Dialogue (i.e. CalendarPanel is an observer of dialogue).  
 Other interested classes can register for notification when Dialogue changes.
*/
public class Dialogue extends Observable {
	static final int ID_REC_TEXT = 1;
	static final int ID_ANSWER = 2;
	static final int ID_HELP = 3;
	static final int Id_RAW_REC = 4;
	
	private String recognizedText;
	private String answer;
	private Vector<String> help;
	private NBestRegResult recResult;

	public Dialogue() {
		super();
	}
	
	public Dialogue(String recognizedText, String answer, Vector<String> help) {
		super();
		this.recognizedText = recognizedText;
		this.answer = answer;
		this.help = help;		
	}
	

	public String getRecognizedText() {
		return recognizedText;
	}

	public void setRecognizedText(String recognizedText) {
		this.recognizedText = recognizedText;
		setChanged();
		notifyObservers(new Integer(ID_REC_TEXT));
	}

	public String getAnswer() {
		return answer;
	}

	public void setAnswer(String answer) {
		if (answer.startsWith("tts")) {
			 answer = answer.replaceFirst("tts.", "");
			 answer = answer.replace(")", "");
		}
		this.answer = answer;
		setChanged();
		notifyObservers(new Integer(ID_ANSWER));
	}

	public Vector<String> getHelp() {
		return help;
	}

	public void setHelp(Vector<String> help) {
		this.help = help;
		setChanged();
		notifyObservers(new Integer(ID_HELP));
	}

	public NBestRegResult getRawRecResult() {
		return recResult;
	}

	public void setRawRecResult(NBestRegResult recResult) {
		this.recResult = recResult;
		setChanged();
		notifyObservers(new Integer(Id_RAW_REC));
	}
		
}
