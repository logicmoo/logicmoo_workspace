package net.sf.regulus;

import java.util.ArrayList;

/**
 * 
 * This object encapsulates information about nbest recognition.
 *
 */
public class NBestRegResult {

	/** contains a list of the nbest recognition results */
	private ArrayList<RegResult> regResults = null;
	
	/*
	 * Creates a NBestRegResult object. 
	 * The constructor extracts confidence, recognition and interpretation strings out of the regulus server reply.
	 * The interpretationObject is created on demand for each result
	 */
	public NBestRegResult(String speechServerReply){
		
		regResults = new ArrayList<RegResult>(); 
		
		if(speechServerReply.startsWith("recognition_succeeded")) {
			parseSpeechServerReply(speechServerReply);
		}
		else {
			RegClient.logger.info("Invalid SpeechServer reply format: '" + speechServerReply + "'");
		}
						
		return;
	}
	
	/**
	 * 
	 * parse the content of the regulus server reply.
	 * fill a list with RegResult containing the i-best information.
	 * 
	 */
	public void parseSpeechServerReply(final String speechServerReply){
		
		try{
			String[] nbestList = speechServerReply.split("rec_result");
			int confidence = 0;
			String recognition = "";
			String interpretation = "";
			
			for (int i = 1; i < nbestList.length; i++)
			{
				int indexFirstPar = nbestList[i].indexOf('(');
				int indexFirstComma = nbestList[i].indexOf(',');
				int indexFirstQuote = nbestList[i].indexOf('\'');
				int indexSecondQuote = nbestList[i].indexOf('\'', indexFirstQuote+1);
				int indexFirstPat = nbestList[i].indexOf("[ 'value' =");
				int indexLastPat = nbestList[i].lastIndexOf("]])");
				
				confidence = Integer.parseInt(nbestList[i].substring(indexFirstPar+1, indexFirstComma));
				recognition = nbestList[i].substring(indexFirstQuote+1, indexSecondQuote);
				interpretation = nbestList[i].substring(indexFirstPat, indexLastPat+2);
					
				RegResult nbestResult = new RegResult(confidence, recognition, interpretation);
				regResults.add(nbestResult);
			}
		}
		catch(Exception e){
			RegClient.logger.info("Could not parse SpeechServer reply format: '" + speechServerReply + "'");		
		}
		
		return;
	}
	
	/**
	 * @return an array with the nbest results.
	 */
	public ArrayList<RegResult> getNBestResults()
	{
		return regResults;
	}
	
	/**
	 * print the contents of the nbest list.
	 */
	public void printResults(){
		java.util.Iterator it = regResults.iterator();
		
		while(it.hasNext()){
			RegResult result = (RegResult)it.next();			
			result.printResult();
			System.out.println("\n");
		}
		
		return;
	}
	
	/**
	 * 
	 * @return A Regulus representation of the revognition result. 
	 */
	public String toRegulusString(){
	
		java.util.Iterator it = regResults.iterator();
		StringBuffer str = new StringBuffer();
		
		if (it.hasNext()){
			
			str.append("recognition_succeeded([");
			
			while(it.hasNext()){
				
				RegResult result = (RegResult)it.next();			
					
				str.append("rec_result(");	
				str.append(result.getConfidence());
				str.append(", '");
				str.append(result.getRecognition());
				str.append("', ");
				str.append(result.getInterpretation());
	
				if (it.hasNext()){
					str.append("), ");
				}
				else{
					str.append(") ]).");		
				}
			}
		}
		else{
			str.append("recognition_failed(some_dumb_reason).");
		}		
		
		return str.toString();
	}
	
	public static void main(String[] args){
				
		String str = "recognition_succeeded([rec_result(53, 'were there pains', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'secondary_symptom', 'pain' ] ]]), rec_result(42, 'where is the pain', [ 'value' = [ [ 'utterance_type', 'whq' ], [ 'loc', 'where' ], [ 'voice', 'active' ], [ 'secondary_symptom', 'pain' ], [ 'tense', 'present' ], [ 'verb', 'be' ] ]]), rec_result(42, 'was it pain', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'pronoun', 'it' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'secondary_symptom', 'pain' ] ]]), rec_result(33, 'was there the same', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'spec', 'the_same' ] ]]), rec_result(33, 'was there tea', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'cause', 'tea' ] ]]), rec_result(33, 'was it deep', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'pronoun', 'it' ], [ 'tense', 'past' ], [ 'verb', 'be' ], [ 'adj', 'deep' ] ]]) ]).";
		//String str = "recognition_succeeded([rec_result(53, 'were there pains', [ 'value' = [ [ 'utterance_type', 'ynq' ], [ 'voice', 'active' ], [ 'tense', 'past' ], [ 'existential', 'there_is' ], [ 'secondary_symptom', 'pain' ] ]]) ]).";
				
		NBestRegResult test = new NBestRegResult(str);
		
		test.printResults();
		System.out.println(test.toRegulusString());
		
		return;
	}				
}
