
package net.sf.regulus;

import java.util.ArrayList;
/**
 * 
 * This object encapsulates information about recognition.
 * Contains the list of NBest phrases (N recognition strings) received from regserver. 
 */
public class RegResultNBest extends RegResult{
	
	/** contains the list of NBest phrases (N recognition strings) 
	 * received from regserver */
	NBestBean[] nbest;
	
	/** contains the command to be sent to the dialogue server. 
	 * This corresponds to the regserver response*/
	private String interpret4DialogueServer;
	
	/*
	 * Creates a RegResultNBest object. Override the RegResult constructor, which 
	 * extracts recognition and interpretation strings out of the regulus server reply.
	 */
	public RegResultNBest(String speechServerReply){
		// super();
		super.recognition = speechServerReply; 
		try{
			if(speechServerReply.startsWith("recognition_succeeded")) {
				// test if there is only one result 
				int firstRecResultIndex = speechServerReply.indexOf("rec_result");
				int secondRecResultIndex = speechServerReply.indexOf("rec_result", firstRecResultIndex+1);
				
				if (secondRecResultIndex == -1){
					int firstCommaIndex = speechServerReply.indexOf(",");
					int secondCommaIndex = speechServerReply.indexOf(",", firstCommaIndex+1);
					
					String tmp = speechServerReply.substring(firstCommaIndex+1, secondCommaIndex );					
					tmp = tmp.trim();
					this.recognition = tmp;
					
					String interpretation = speechServerReply.substring(secondCommaIndex+1, speechServerReply.length()-1);
					interpretation = interpretation.trim().replaceAll("\\) ]$", "");					
					
					init(ACTION_SUCCEEDED, 0, tmp, interpretation);
				}
				else{
					String result = formatNBestResponse(speechServerReply, "action(process_nbest_list([");
					result = result + "]))";	
					interpret4DialogueServer = result;
				}
			}
			else{
				init(ACTION_ERROR, 0, "", "");
				// System.out.println("ERR: problem when getting substr of recognition_succeeded... " );
			}
		}
		catch(Exception e){
			init(ACTION_ERROR, 0, "", "");
		}
	}
	
	/**
	 * 
	 * @param status
	 */
	public RegResultNBest(int status){
		super(status);
	}
	
	
	/**
	 * Recursive procedure
	 * @param regResponse the response received from regserver. 
	 * This should be a string of the form: "rec_result(32, 'where were starlander', [ 'value' = [ [ 'whq', [ 'apply', [ 'lambda', 'x', [ [ 'verb_type', 'pp' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'subj', [ [ 'spec', 'name' ], [ 'head', 'starlander' ] ] ], [ 'subcat_pp', [ 'x' ] ] ] ], [ [ 'loc', 'where' ] ] ] ] ]]) ])."
	 * @return  
	 */
	static private String formatNBestResponse(String speechServerReply, String res){
		int i1 = speechServerReply.indexOf("rec_result");
		int i2 = speechServerReply.indexOf("rec_result", i1+1);
	
		if (i1 == -1) {
			return "";	
		}
		else{	
			String substr1;
			if (i2 > -1)
				substr1 = speechServerReply.substring(i1, i2-1);
			else
				substr1 = speechServerReply;
		
			int indexFirstPar = substr1.indexOf('(');
			int indexFirstComma = substr1.indexOf(',');
			int indexSecondComma = substr1.indexOf(',', indexFirstComma+1);
			if ((indexFirstPar > -1) && (indexFirstComma > -1) && (indexSecondComma > -1) ) {
				Integer score = new Integer(substr1.substring(indexFirstPar+1, indexFirstComma));
				String phrase = substr1.substring(indexFirstComma+3, indexSecondComma-1);
			
				res = res + "nbest_hyp(";
				res = res + score;
				res = res + ",";
				res = res + "'" + phrase + "'";
				res = res + ")";
			}
			if (i2 > -1){
				String substr2 = speechServerReply.substring(i2, speechServerReply.length());
				res = res + ",";
				String res_total = formatNBestResponse(substr2, res);
				return res_total;
			}
			return res;
		}	
	}
	
	/**
	 * Non-Recursive procedure
	 * @param regResponse the response received from regserver. 
	 * This should be a string of the form: "rec_result(32, 'where were starlander', [ 'value' = [ [ 'whq', [ 'apply', [ 'lambda', 'x', [ [ 'verb_type', 'pp' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'subj', [ [ 'spec', 'name' ], [ 'head', 'starlander' ] ] ], [ 'subcat_pp', [ 'x' ] ] ] ], [ [ 'loc', 'where' ] ] ] ] ]]) ])."
	 * @return  
	 * @author GEORGESC
	 */
	public static java.util.ArrayList<NBestBean> formatNBestRegResponse(String speechServerReply){
		java.util.ArrayList<NBestBean> result = new ArrayList<NBestBean>();
		
		int i1 = speechServerReply.indexOf("rec_result");
		int i2 = speechServerReply.indexOf("rec_result", i1+1);
	
		while ((i1 != -1) && (i2 != -1)) {
			String substr = speechServerReply.substring(i1, i2-1);
		
			int indexFirstPar = substr.indexOf('(');
			int indexFirstComma = substr.indexOf(',');
			int indexSecondComma = substr.indexOf(',', indexFirstComma+1);
			if ((indexFirstPar > -1) && (indexFirstComma > -1) && (indexSecondComma > -1) ) {
				String score = substr.substring(indexFirstPar+1, indexFirstComma);
				String phrase = substr.substring(indexFirstComma+3, indexSecondComma-1);
				// 	nbest_hyp(51,were there any),nbest_hyp(42,for thursday),
				String logicForm = substr.substring(indexSecondComma, substr.length());
				logicForm = logicForm.replace("'", "");
				logicForm = logicForm.replace("),", "");
				logicForm = logicForm.replace(", [ value = [ ", "");
				logicForm = logicForm.replace(" ] ]]", " ]");
				
				NBestBean nbest = new NBestBean(score, phrase, logicForm);
				result.add(nbest);
						
				i1 = i2;
				i2 = speechServerReply.indexOf("rec_result", i2+1);
			} // end if
		}// end while
		if (i1 > -1){
			String substr = speechServerReply.substring(i1, speechServerReply.length()-1);
			int indexFirstPar = substr.indexOf('(');
			int indexFirstComma = substr.indexOf(',');
			int indexSecondComma = substr.indexOf(',', indexFirstComma+1);
		
			String score = substr.substring(indexFirstPar+1, indexFirstComma);
			String phrase = substr.substring(indexFirstComma+3, indexSecondComma-1);
			String logicForm = substr.substring(indexSecondComma, substr.length());
			logicForm = logicForm.replace("'", "");
			logicForm = logicForm.replace(")", "");
			logicForm = logicForm.replace(".", "");
			logicForm = logicForm.replace(", [ value = [ ", "");
			logicForm = logicForm.replace(" ] ]]", "");

			NBestBean nbest = new NBestBean(score, phrase, logicForm);
			result.add(nbest);
		}

		
		return result;
	}

	/**
	 * Non-Recursive procedure
	 * @param regResponse the response received from regserver. 
	 * This should be a string of the form: "rec_result(32, 'where were starlander', [ 'value' = [ [ 'whq', [ 'apply', [ 'lambda', 'x', [ [ 'verb_type', 'pp' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'subj', [ [ 'spec', 'name' ], [ 'head', 'starlander' ] ] ], [ 'subcat_pp', [ 'x' ] ] ] ], [ [ 'loc', 'where' ] ] ] ] ]]) ])."
	 * @return  
	 */
	public static String formatNBestRegResponseToStr(String speechServerReply){
		String result = "";
		
		int i1 = speechServerReply.indexOf("rec_result");
		int i2 = speechServerReply.indexOf("rec_result", i1+1);
	
		while ((i1 != -1) && (i2 != -1)) {
			String substr = speechServerReply.substring(i1, i2-1);
		
			int indexFirstPar = substr.indexOf('(');
			int indexFirstComma = substr.indexOf(',');
			int indexSecondComma = substr.indexOf(',', indexFirstComma+1);
			if ((indexFirstPar > -1) && (indexFirstComma > -1) && (indexSecondComma > -1) ) {
				String score = substr.substring(indexFirstPar+1, indexFirstComma);
				String phrase = substr.substring(indexFirstComma+3, indexSecondComma-1);
				// 	nbest_hyp(51,were there any),nbest_hyp(42,for thursday),
				String logicForm = substr.substring(indexSecondComma, substr.length());
				logicForm = logicForm.replace("'", "");
				logicForm = logicForm.replace("),", "");
				logicForm = logicForm.replace(", [ value = [ [", "");
				logicForm = logicForm.replace(" ] ] ]", " ]");
				
				result = result + " \n " + score + " \n " + phrase + " \n " + logicForm;
								
				i1 = i2;
				i2 = speechServerReply.indexOf("rec_result", i2+1);

			}
		}
		
		return result;
	}
	public String getInterpret4DialogueServer(){
		return interpret4DialogueServer;
	}
	
	/**
	 * Method getInterpret4TranslationServer added in order to support nbest for the MedSLT gui. 
	 * The method returns the message to be sent to the translation server.
	 *@param String str - a string containg the response from the recognition server.  
	 */	
	public java.util.ArrayList<NBestBean> getInterpret4TranslationServer(String str){
		java.util.ArrayList<NBestBean> nbestList =  formatNBestRegResponse(str);
/*		
		java.util.Iterator it = nbestList.iterator();
		String interpret4TranslationServer = "action(translate_nbest([";
		boolean firstHyp = true;
		while(it.hasNext()){
			NBestBean nbest = (NBestBean)it.next();
			if (firstHyp){
				interpret4TranslationServer = interpret4TranslationServer + "hyp('";
				firstHyp = false;
			}
			else{
				interpret4TranslationServer = interpret4TranslationServer + ", hyp('";
			}
			
			interpret4TranslationServer = interpret4TranslationServer + nbest.getRec();
			interpret4TranslationServer = interpret4TranslationServer + "',";
			interpret4TranslationServer = interpret4TranslationServer + nbest.getConfidence();
			interpret4TranslationServer = interpret4TranslationServer + ", [";
			interpret4TranslationServer = interpret4TranslationServer + nbest.getValue();
			interpret4TranslationServer = interpret4TranslationServer + "])";
		}
		
		interpret4TranslationServer = interpret4TranslationServer + "])).";
		
		return interpret4TranslationServer;
*/
		return nbestList;
	}
	
	public static void main(String[] args){
/*		String str = "recognition_succeeded([rec_result(44, 'were there any meetings my weeks', " +
				"[ 'value' = [ [ 'ynq', [ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], " +
				"[ 'verb', 'there_is' ], [ 'subj', [ [ 'spec', 'any' ], [ 'head', 'meeting' ], " +
				"[ 'duration', [ [ 'spec', 'null' ], [ 'possessive', [ [ [ 'spec', 'pro' ], " +
				"[ 'head', 'i' ] ] ] ], [ 'head', 'week' ] ] ] ] ] ] ] ]]), " +
				"rec_result(44, 'were there any meetings last week', [ 'value' = [ [ 'ynq', " +
				"[ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], [ 'verb', " +
				"'there_is' ], [ 'subj', [ [ 'spec', 'any' ], [ 'head', 'meeting' ], " +
				"[ 'duration', [ [ 'spec', 'last' ], [ 'head', 'week' ] ] ] ] ] ] ] ]]), " +
				"rec_result(39, 'were there any meetings her weeks', [ 'value' = [ [ 'ynq', [ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], [ 'verb', 'there_is' ], [ 'subj', [ [ 'spec', 'any' ], [ 'head', 'meeting' ], [ 'duration', [ [ 'spec', 'null' ], [ 'possessive', [ [ [ 'spec', 'pro' ], [ 'head', 'she' ] ] ] ], [ 'head', 'week' ] ] ] ] ] ] ] ]]), rec_result(39, 'was there any meeting my days', [ 'value' = [ [ 'ynq', [ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], [ 'verb', 'there_is' ], [ 'subj', [ [ 'spec', 'any' ], [ 'head', 'meeting' ], [ 'duration', [ [ 'spec', 'null' ], [ 'possessive', [ [ [ 'spec', 'pro' ], [ 'head', 'i' ] ] ] ], [ 'head', 'day' ] ] ] ] ] ] ] ]]), rec_result(39, 'were there ten in my weeks', [ 'value' = [ [ 'ynq', [ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], [ 'verb', 'there_is' ], [ 'subj', [ [ 'spec', 10 ], [ 'head', 'null' ] ] ], [ 'in_time', [ [ 'spec', 'null' ], [ 'possessive', [ [ [ 'spec', 'pro' ], [ 'head', 'i' ] ] ] ], [ 'head', 'week' ] ] ] ] ] ]]), rec_result(39, 'were there ten in my days', [ 'value' = [ [ 'ynq', [ [ 'verb_type', 'existential' ], [ 'tense', 'past' ], [ 'verb', 'there_is' ], [ 'subj', [ [ 'spec', 10 ], [ 'head', 'null' ] ] ], [ 'in_time', [ [ 'spec', 'null' ], [ 'possessive', [ [ [ 'spec', 'pro' ], [ 'head', 'i' ] ] ] ], [ 'head', 'day' ] ] ] ] ] ]]) ]).";
	*/	
		//String str = "recognition_succeeded([rec_result(53, 'were there pains', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'secondary_symptom', 'pain' ] ]]), rec_result(42, 'where is the pain', [ 'value' = [ [ 'utterance_type', 'whq' ], [ 'loc', 'where' ], [ 'voice', 'active' ], [ 'secondary_symptom', 'pain' ], [ 'tense', 'present' ], [ 'verb', 'be' ] ]]), rec_result(42, 'was it pain', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'pronoun', 'it' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'secondary_symptom', 'pain' ] ]]), rec_result(33, 'was there the same', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'spec', 'the_same' ] ]]), rec_result(33, 'was there tea', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'cause', 'tea' ] ]]), rec_result(33, 'was it deep', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'pronoun', 'it' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'adj', 'deep' ] ]]) ]).";
		String str = "recognition_succeeded([rec_result(53, 'were there pains', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'secondary_symptom', 'pain' ] ]]) ]).";
		
		ArrayList<NBestBean> test = RegResultNBest.formatNBestRegResponse(str);
		// ("help('who was at the last meeting\nwhere was the last meeting\nwhen was the last meeting in geneva\nwhen was the last meeting in england\nwhen was the last meeting').");
		java.util.Iterator it = test.iterator();
		while(it.hasNext()){
			NBestBean nbest = (NBestBean)it.next();
			System.out.println("Confidence = " + nbest.getConfidence());
			System.out.println("Recognition = " + nbest.getRec());
			System.out.println("LF = " + nbest.getValue());
		}
		
// testing the getInterpret4TranslationServer method
/*		String interpret4TranslationServer = getInterpret4TranslationServer(str);
		
		System.out.println();
		System.out.println(" str = " + str );
		System.out.println();
		System.out.println(" Interpret4TranslationServer = " + interpret4TranslationServer );*/
	}
}
