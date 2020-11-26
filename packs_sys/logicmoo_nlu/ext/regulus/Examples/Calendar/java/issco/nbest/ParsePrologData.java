package issco.nbest;

public class ParsePrologData {

	/**
	 * 
	 * @param nbestCurrent
	 * @return
	 */
	static String getWavFile(String nbestCurrent){
		int idxWav = nbestCurrent.indexOf("wavfile");
		if (idxWav < 0 )
			return "";
  		int idxWavStart = nbestCurrent.indexOf("'", idxWav);
  		int idxWavEnd = nbestCurrent.indexOf("'", idxWavStart + 1);
  		String wavFile = nbestCurrent.substring(idxWavStart+1, idxWavEnd);				    			  		
  		return wavFile;
	}

	/**
	 * 
	 * @param nbestCurrent
	 * @return
	 */
	static String getCorrectWords(String nbestCurrent){
  		int idxCorrectWords = nbestCurrent.indexOf("correct_words");
  		if (idxCorrectWords < 0)
  			return "";
		int idxCWStart = nbestCurrent.indexOf("[", idxCorrectWords);
  		int idxCWEnd = nbestCurrent.indexOf("]", idxCWStart + 1);
  		String correctWords = nbestCurrent.substring(idxCWStart+1, idxCWEnd);	    			  		
  		return correctWords;
	}

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getFirstFeature(String recCurrent, String featureName){
			int idxStart = recCurrent.indexOf("=", recCurrent.indexOf(featureName));
			int idxEnd = recCurrent.indexOf(",", idxStart + 1);
			String value = recCurrent.substring(idxStart+1, idxEnd);	    			  						    			  		
			return value;
	}
	

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getRecWords(String recCurrent){
		int idxRecWords = recCurrent.indexOf("recognised_words");
		if (idxRecWords < 0)
			return "";
			int idxRecWordsStart = recCurrent.indexOf("[", idxRecWords);
			int idxRecWordsEnd = recCurrent.indexOf("]", idxRecWordsStart + 1);
			String recWords = recCurrent.substring(idxRecWordsStart+1, idxRecWordsEnd);	    			  						    			  		
			return recWords;
	}

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getConfidence(String recCurrent){
		int idxConf = recCurrent.indexOf("confidence");
		if (idxConf < 0 )
			return "";
			int idxConfStart = recCurrent.indexOf("=", idxConf);
			int idxConfEnd = recCurrent.indexOf(",", idxConfStart + 1);
			String confValue = recCurrent.substring(idxConfStart+1, idxConfEnd);	    			  						    			  		
			return confValue;
	}

	
	/**
	 * 
	 * @param recCurrent
	 * @param featName - feature name in prolog format 
	 * (e.g. "definite_meeting_and_meeting_referent")
	 * @return
	 */
	static String getFeatureValue(String recCurrent, String featName){
		int idx = recCurrent.indexOf(featName);
		if (idx < 0 )
			return "";
		
		int idxStart = recCurrent.indexOf("=", idx);
		int idxEnd = recCurrent.indexOf(",", idxStart + 1);
		String value = recCurrent.substring(idxStart+1, idxEnd);	
		//System.out.println(value);
		return value;
	}

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getResponse(String recCurrent){
		int idxResponseVal = recCurrent.indexOf("response=");
		if (idxResponseVal < 0)
			return "";
			int idxResponseStart = recCurrent.indexOf("=", idxResponseVal);
			int idxResponseEnd = recCurrent.indexOf(",", idxResponseStart + 1);
			String response = recCurrent.substring(idxResponseStart+1, idxResponseEnd);	    			  						    			  		
			return response;
	}


	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getResponseType(String recCurrent){
		int idxResponseVal = recCurrent.indexOf("response_type=");
		if (idxResponseVal < 0)
			return "";
			int idxResponseStart = recCurrent.indexOf("=", idxResponseVal);
			int idxResponseEnd = recCurrent.indexOf(",", idxResponseStart + 1);
			String response = recCurrent.substring(idxResponseStart+1, idxResponseEnd);	    			  						    			  		
			return response;
	}

	// lf_context_available

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static String getLFContextAvailable(String recCurrent){
		int idxResponseVal = recCurrent.indexOf("lf_context_available=");
		if (idxResponseVal < 0)
			return "";
			int idxResponseStart = recCurrent.indexOf("=", idxResponseVal);
			int idxResponseEnd = recCurrent.indexOf(",", idxResponseStart + 1);
			String response = recCurrent.substring(idxResponseStart+1, idxResponseEnd);	    			  						    			  		
			return response;
	}

	/**
	 * Get the feature's value. The value should be encountered by the "=" and "]" characters   
	 * @param recCurrent
	 * @param featureName
	 * @return
	 */
	static String getLastFeature(String recCurrent, String featureName){
		int idxFeatVal = recCurrent.indexOf(featureName);
		if (idxFeatVal < 0)
			return "";
			int idxFeatStart = recCurrent.indexOf("=", idxFeatVal);
			int idxFeatEnd = recCurrent.indexOf("]", idxFeatStart + 1);
			String response = recCurrent.substring(idxFeatStart+1, idxFeatEnd);	    			  						    			  		
			return response;
	}

	/**
	 * Get the feature's value. The value should be encountered by the "=" and "," characters   
	 * @param recCurrent
	 * @param featureName
	 * @return
	 */
	static String getFeature(String recCurrent, String featureName){
		int idxFeatVal = recCurrent.indexOf(featureName);
		if (idxFeatVal < 0)
			return "";
			int idxFeatStart = recCurrent.indexOf("=", idxFeatVal);
			int idxFeatEnd = recCurrent.indexOf(",", idxFeatStart + 1);
			String response = recCurrent.substring(idxFeatStart+1, idxFeatEnd);	    			  						    			  		
			return response;
	}

	/**
	 * 
	 * @param recCurrent
	 * @return
	 */
	static java.util.Vector<String> getReferentAvailable(String recCurrent){
		int idxFeatVal = recCurrent.indexOf("referent_available");
		if (idxFeatVal < 0)
			return null;
		java.util.Vector<String> refList = new java.util.Vector<String>();
		
		// Check how many referents is possible to have?
		// Do a while to see:
		while (idxFeatVal >= 0){
			int idxFeatStart = recCurrent.indexOf("=", idxFeatVal);
			int idxFeatEnd = recCurrent.indexOf(",", idxFeatStart + 1);
			String referent = recCurrent.substring(idxFeatStart+1, idxFeatEnd);
			refList.add(referent);
			
			recCurrent = recCurrent.substring(idxFeatEnd, recCurrent.length());
			idxFeatVal = recCurrent.indexOf("referent_available");
		}
		
			return refList;
	}

	/**
	 * 
	 * @param currentStr
	 * @param featureName
	 * @return
	 */
	static String getFeatBetweenSquarePar(String currentStr, String featureName){
  		int idxFeat = currentStr.indexOf(featureName);
  		if (idxFeat < 0)
  			return "";
		int idxFeatStart = currentStr.indexOf("[", idxFeat);
  		int idxFeatEnd = currentStr.indexOf("]", idxFeatStart + 1);
  		String featValue = currentStr.substring(idxFeatStart+1, idxFeatEnd);	    			  		
  		return featValue;
	}				


}
