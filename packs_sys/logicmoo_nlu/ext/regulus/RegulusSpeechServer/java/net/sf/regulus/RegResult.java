
package net.sf.regulus;

/**
 * 
 * This object encapsulates information about recognition.
 *
 */
public class RegResult {
    // TODO: update final results with more meaningfull returns, e.g. no_speech_timeout, audio_error, etc.
    /** The action that returned this object has been succesfull */
	public static final int ACTION_SUCCEEDED = 0;
	/** The request has been aborted */
	public static final int ACTION_ABORTED = 1;
	/** An error occured while executing the request */
	public static final int ACTION_ERROR = 2;
	
	private int status = 0;
	private int confidence = 0;
	
	/** contains interpretation string as received from regserver */
	private String interpretation = null;
	/** contains recognition string as received from regserver */
	protected String recognition = null;
	/** contains object representation of interpretation.
	 * Key-, value-pairs form hashtables, list of values is returned in arrays */
	private Object interpretationObject = null;
	
	/*
	 * Creates a RegResult object. 
	 */
	public RegResult(){}

	/*
	 * Creates a RegResult object. 
	 * The constructor extracts recognition and interpretation strings out of the regulus server reply.
	 * The interpretationObject is created on demand.
	 */
	public RegResult(String speechServerReply){
		int status;
		int conf;
		String recognition;
		String interpretation;
		
		try{
			
			if(speechServerReply.startsWith("recognition_")) {
				//
				// recognition_succeeded(59,'recognition text',[ 'value' = [ [nl results] ]).
				//
				int firstParen = speechServerReply.indexOf('(');
				int firstComma = speechServerReply.indexOf(',');
				int secondComma = speechServerReply.indexOf(',', firstComma+1);
				
				if((firstParen == -1) || (firstComma == -1) || (secondComma == -1)){
					status = ACTION_ERROR;
					init(status, 0, "", "");			
				}
				else{
					String tmp = speechServerReply.substring(firstParen+1, firstComma);
					tmp = tmp.replaceFirst("\\(", "");
					conf = Integer.parseInt(tmp);
	
					recognition = speechServerReply.substring(firstComma+1, secondComma);
					recognition = recognition.trim().replaceAll(",$", "");
					recognition = recognition.trim().replaceAll("\'$", "");
					recognition = recognition.replaceAll("^\'", "");
					
					if(recognition.equals("<Interpretation>")){
						recognition = "";
					}
													 
					interpretation = speechServerReply.substring(secondComma+1, speechServerReply.length()-1);
					interpretation = interpretation.trim().replaceAll("\\)$", "");
					
					init(ACTION_SUCCEEDED, conf, recognition, interpretation);
				}
			}
			else{
				init(ACTION_ERROR, 0, "", "");
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
	public RegResult(int status){
		init(status, 0, null, null);
	}
	
	/**
	 * 
	 * @param confidence
	 * @param recognition
	 * @param interpretation
	 */
	public RegResult(int confidence, String recognition, String interpretation){
		init(ACTION_SUCCEEDED, confidence, recognition, interpretation);
	}
	
	/**
	 * 
	 * @param confidence
	 * @param recognition
	 */
	public RegResult(int confidence, String recognition){
		init(ACTION_SUCCEEDED, confidence, recognition, "");
	}  
	
	/**
	 * 
	 * @param status
	 * @param confidence
	 * @param recognition
	 * @param interpretation
	 */
	protected void init(int status, int confidence, String recognition, String interpretation){
		this.status = status;
		this.confidence = confidence;
		this.recognition = recognition;
		this.interpretation = interpretation;
	}
	/**
	 * @return Confidence of the recognition result.<br>Note that this field might not be set, e.g. when the object contains only an interpretation result.  
	 */
	public int getConfidence() {
		return confidence;
	}

	/**
	 * @return The Natural Language (NL) interpretation of the grammar.
	 */
	public String getInterpretation() {
		return interpretation;
	}

	/**
	 * @return The recognition result.
	 */
	public String getRecognition() {
		return recognition;
	}

	/**
	 * @param conf - The confidence of the recognition result.
	 */
	public void setConfidence(int conf) {
		confidence = conf;
	}

	/**
	 * @param interpretation
	 */
	public void setInterpretation(String interpretation) {
		this.interpretation = interpretation;
	}

	/**
	 * @param recognition
	 */
	public void setRecognition(String recognition) {
		this.recognition = recognition;
	}

	/**
	 * @return Contains the status of the result.<br>Can be one of ACTION_SUCCEEDED, ACTION_ABORTED, ACTION_ERROR
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * @param status - Status is of ACTION_SUCCEEDED, ACTION_ABORTED, ACTION_ERROR
	 */
	public void setStatus(int status) {
		this.status = status;
	}
	
	/**
	 * Convenience function to check if the result contains usefull infrmation
	 * @return true if requested recognition/interpretation action was succesfull
	 */
	public boolean isActionSuccesfull(){
	    if(ACTION_SUCCEEDED == status){
	        return true;
	    }
	    else{
	        return false;
	    }
	}
	
	public Object getInterpretationObject(){
	    if(interpretationObject == null){
	        try{
	            Interpretation i = new Interpretation(interpretation);
	            interpretationObject = i.getInterpretation(); 
	        }
	        catch(Exception e){
	            e.printStackTrace();
	            interpretationObject = null;
	        }
	    }
	    
	    return interpretationObject;
	}
	
	/**
	 * 
	 * @return A Regulus representation of the revognition result. 
	 */
	public String toRegulusString(){
		StringBuffer result = new StringBuffer();
		
		if(ACTION_SUCCEEDED == status){
			result.append("recognition_succeeded(");
			result.append(confidence);
			result.append(", '");
			result.append(recognition);
			result.append("', '");
			result.append(interpretation);
			result.append("').");
		}
		else{
			result.append("recognition_failed(some_dumb_reason).");
		}
		
				
		return result.toString();
	}
	
	/**
	 * 
	 * print the info of the regresult. 
	 */
	public void printResult(){
				
		System.out.println("Confidence = " + getConfidence());
		System.out.println("Recognition = " + getRecognition());
		System.out.println("Interpretation = " + getInterpretation());
						
		return;
	}

}
