package gui.transact;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.sf.regulus.NBestRegResult;
import net.sf.regulus.RegResult;

/**
 * Utility for parsing dialogue server responses.
 * @author GEORGESC
 *
 */
public class DialServResponseParser {

	static class Response {
		String selected;
		String action;
		String paraphrase;
		public String getSelected() {
			return selected;
		}
		public String getAction() {
			return action;
		}
		public String getParaphrase() {
			return paraphrase;
		}
		private void setSelected(String selected) {
			this.selected = selected;
		}
		private void setAction(String action) {
			this.action = action;
		}
		private void setParaphrase(String paraphrase) {
			this.paraphrase = paraphrase;
		}

		
		
	}

	static class DialServResponse{
		String textResponse ;
		String playResponse ;		
	}

	private final static Logger logger = Logger.getLogger("DialServResponseParser");

	/**
	 * Given the dialogue server reply, get the text response.
	 * @return
	 */
	static String getGeneralResponse(String dialogueServerReply){
		String txResponse = dialogueServerReply;
		try{
			if (txResponse.indexOf("unknown_request") > -1){
				txResponse = "+text Sorry\\, unknown request.";
				return txResponse;
			}
			int idxAction = dialogueServerReply.indexOf("action");
			int idx2 = dialogueServerReply.indexOf("paraphrase");
			if ((idxAction !=-1) && (idx2 != -1))
				txResponse = txResponse.substring(idxAction , idx2-1);
			if (txResponse.contains("no_action"))
				return "+text No action";
	
	
			int indexWav = txResponse.indexOf("wavfile_tts(");
			if (indexWav !=-1){
				txResponse = txResponse.substring(indexWav, txResponse.length());
				return "+" + txResponse;
			}
	
			int indexTTS = txResponse.indexOf("tts(");
			if (indexTTS != -1){
				txResponse = txResponse.replace(")", "");
				txResponse = txResponse.replace("]", "");
				txResponse = txResponse.replace(".", "");
				if (txResponse.indexOf("'") == 0)
					txResponse = txResponse.substring(1, txResponse.length());
				if (txResponse.lastIndexOf("'") == txResponse.length()-1)
					txResponse = txResponse.substring(0, txResponse.length()-1);					
				txResponse = txResponse.replace("\\'", "'");
				txResponse = txResponse.replace(",", "\\,");
				return "+" + txResponse;
			}
		}
		catch(java.lang.StringIndexOutOfBoundsException e){
			System.out.println("Index out of bounds inside  getTextResponse");
		}
		return dialogueServerReply;
	}

	
	/**
	 * Given the dialogue server reply, get the selected interpretation
	 * @param dialogueServerReply
	 * @return
	 */
	static String getSelectedInterpret(String dialogueServerReply){
	
		int idx1 = dialogueServerReply.indexOf("selected");
		int idx2 = dialogueServerReply.indexOf("action");
		String selectedInterpretation = "";
		try{
			if ((idx1 !=-1) && (idx2 != -1)){
				selectedInterpretation = dialogueServerReply.substring(idx1 + 9, idx2-2);
				if (selectedInterpretation.indexOf("'") == 0){
					selectedInterpretation = selectedInterpretation.substring(1, selectedInterpretation.length());
				}
				if (selectedInterpretation.lastIndexOf("'") == selectedInterpretation.length() ){
					selectedInterpretation = selectedInterpretation.substring(0, selectedInterpretation.length()-1);
				}
			}
			else {
				if (dialogueServerReply.indexOf("unknown_request") > -1);
				// selectedInterpretation = "";			
				else
					if (dialogueServerReply.indexOf("Sorry, something went wrong") > -1);
				// selectedInterpretation = "";				
					else
						// tts('Sorry, something went wrong').
						selectedInterpretation = dialogueServerReply;
			}
		}catch(java.lang.StringIndexOutOfBoundsException e){
			System.out.println("Index out of bounds inside getSelectedFromDialogServerReply ");
		}
		return selectedInterpretation;		
	}

	/**
	 * @param dialogueServerReply
	 * @return
	 */
	static DialServResponseParser.DialServResponse transformResponse(String dialogueServerReply, Logger logger) {
		String textResponse = DialServResponseParser.getGeneralResponse(dialogueServerReply);
		String playResponse = "";
		if (textResponse.startsWith("+wavfile_tts")) {
			int idxComma = textResponse.indexOf(",");
			try{
				//if (idxComma!= -1){
				playResponse = "+wavfile_tts" + textResponse.substring(idxComma+1, textResponse.length()-1);
				textResponse = textResponse.substring("+wavfile_tts(".length(), idxComma-1);
				textResponse = textResponse.replaceFirst("'", "");
			}
			catch(java.lang.StringIndexOutOfBoundsException e){
				logger.log(Level.SEVERE, " ERR when substring for play response !! ");
			}

		}
		textResponse = textResponse.replace("\\", "");	
		DialServResponseParser.DialServResponse dsr = new DialServResponse();
		dsr.playResponse = playResponse;
		dsr.textResponse = textResponse;	
		return dsr;
	}
	
	public static String getActionProcessNbestList(NBestRegResult nbest){
		String result = "action(process_nbest_list([";
		Iterator<RegResult> i = nbest.getNBestResults().iterator();
		boolean first_hyp = true;
		while (i.hasNext()){
			if (!first_hyp)
				result += ",";		
			else
				first_hyp = false;
			RegResult r = i.next();
			result += "nbest_hyp(" + r.getConfidence() +",'";
			result += r.getRecognition() + "')";			
		}
		result += "]))."; 
		return result;
		
	}
	
	/**
	 * @param dialogueServerReply
	 * @return
	 * Input example : [selected=were there any meetings the last week,action=tts(no),
	 * paraphrase=[were there meetings between Tue Jul 21 2009 and 15:35 on Tue Jul 28 2009,?paraphrase]]
	 */
	static DialServResponseParser.Response parseResponse(String dialogueServerReply) {
		String[] rList = dialogueServerReply.split("=");
		String[] sel = rList[1].split(",action");
		String[] act = rList[2].split(",paraphrase");
		String[] par =  rList[3].split(",..paraphrase");
		
		/*
		textResponse = textResponse.replace("\\", "");
		*/	
		DialServResponseParser.Response dsr = new Response();
		dsr.setSelected(sel[0].substring(0, sel[0].lastIndexOf("'")).replaceFirst("'", "")); // eliminate fisrt and last "'" character
		dsr.setAction(act[0]);
		dsr.setParaphrase(par[0].replaceFirst(".", ""));
		//dsr.setParaphrase(rList[3].replaceFirst(".", "").replace("]].", ""));
		
		/*
		dsr.playResponse = playResponse;
		dsr.textResponse = textResponse;
		*/	
		return dsr;
	}

}