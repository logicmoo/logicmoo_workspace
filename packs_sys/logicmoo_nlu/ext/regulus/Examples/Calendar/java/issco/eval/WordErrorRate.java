package issco.eval;

/**
 * Class useful for computing Word Error Rate for two utterances. 
 * 
 * <p> Copyright (C) 2008 Maria Georgescul, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA </p>
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. <br/>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <br>
 *
 *@author Maria GEORGESCUL, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 */
public class WordErrorRate implements ErrorRate {
	String wordDeliminator = " ";
	 String[] ref = new String[0];
	 String[] hyp = new String[0];
	
	 java.util.Vector<String> refList ;
	 java.util.Vector<String> hypList ;
	 
	int deletions;
	int insertions;
	int substitutions;
	int noRightWords;
	

	/**
	 * @deprecated Use WordErrorRate(String refStr, String hypStr, String wordDeliminator) instead
	 *
	 */
	public WordErrorRate(){
	}
	
	
	/**
	 * 
	 * @param refStr - reference utterance, where each two words are separated by a string specified by wordDeliminator
	 * @param hypStr - hypothetised utterance, where each two words are separated by a a string specified by wordDeliminator.
	 * @param wordDeliminator - A string or a character (e.g. a space, a comma, etc.) used to delimit text into words. 
	 */
	public WordErrorRate(String refStr, String hypStr, String wordDeliminator){
		this.wordDeliminator = wordDeliminator;
		refStr = refStr.toLowerCase();
		hypStr = hypStr.toLowerCase();
		
		ref = refStr.split(wordDeliminator);
		hyp = hypStr.split(wordDeliminator);
		
		// NB: can't use here Arrays.asList since it returns a fixed-size list backed by the specified array -> errs when the resulting list is modified ! 
		 refList = new java.util.Vector<String>();
		 for (int i = 0; i< ref.length; i++){
			ref[i].replace("-", "");
			ref[i].replace("\t", "");
			ref[i].replace("%n", "");
			ref[i].replace(",", "");
			ref[i].replace("-", "");
			ref[i].replace(" ", "");
			
			if (ref[i].length() > 1){
				char lastChar = ref[i].charAt(ref[i].length() - 1);
				if ( ! java.lang.Character.isJavaIdentifierPart( lastChar )){
					ref[i] = ref[i].substring(0, ref[i].length() - 1);
				}
			}
			
			refList.add(ref[i]);
		 }
		 hypList = new java.util.Vector<String>();
		 for (int i = 0; i< hyp.length; i++){
			 hyp[i].replace("-", "");
			 hyp[i].replace("\t", "");
			 hyp[i].replace("\n", "");
			 hyp[i].replace(",", "");
			 hyp[i].replace("-", "");
			 hyp[i].replace(" ", "");
			 if (hyp[i].length() > 1){
				 char lastChar_hyp = hyp[i].charAt(hyp[i].length() - 1);
				 if ( ! java.lang.Character.isJavaIdentifierPart(lastChar_hyp )){
					 // hyp[i].replace( hyp[i].subSequence(hyp[i].length() - 1, hyp[i].length() ), "");
					 hyp[i] = hyp[i].substring(0, hyp[i].length() - 1);
				 }
			 }

			 hypList.add(hyp[i]);
		 }

		 java.util.Collections.sort(refList);
		 java.util.Collections.sort(hypList);		 
	}
	
	/**
	 * Computes the numerator for word error rate. <br/> 
	 * The algorithm requires only O(n^2) iterations, where n=max{ref.length, hyp.length}.
	 * <br/>
	 * NB: This method will delete from this.refList and this.hypList those words that are common.
	 *  
	 * @return the total number of word deletions, insertions and substitutions required 
	 * in order to obtain the "reference" (ref) data starting from "hypothetised" (hyp) data.
	 *    
	 */
	public float computeNumerator() throws IndexOutOfBoundsException, UnsupportedOperationException  {		
			this.noRightWords = 0;
			if (refList.size() > hypList.size()){
				java.util.ListIterator<String> itRef = this.refList.listIterator();				
				while (itRef.hasNext()){
					String refWord = itRef.next();
					
					java.util.ListIterator<String> itHyp = this.hypList.listIterator();
					boolean refIsEmpty = false;
					while (itHyp.hasNext() && (! refIsEmpty) ){
						String hypWord = itHyp.next();
						boolean correct1 = refWord.equals(hypWord);
						if (correct1){
							itRef.remove();
							itHyp.remove();
							if (itRef.hasNext())
								refWord = itRef.next();
							else
								refIsEmpty = true;
							this.noRightWords++;							
						}			
					}
				}// end while
			}
			else{
				java.util.ListIterator<String> itHyp = this.hypList.listIterator();
				while (itHyp.hasNext()){
					String hypWord = itHyp.next();
					
					java.util.ListIterator<String> itRef = this.refList.listIterator();
					boolean hypIsEmpty = false;
					while (itRef.hasNext() && !hypIsEmpty ){
						String refWord = itRef.next();
						
						boolean correct_b = refWord.equals(hypWord);
						if (correct_b){
							itRef.remove();
							itHyp.remove();
							if (itHyp.hasNext())
								hypWord = itHyp.next();
							else
								hypIsEmpty = true;
							this.noRightWords++;
						}			
					}
				}				
			}
		
		int hypLen = hypList.size() ;
		int refLen = refList.size() ;
		if (hypList.toString().equals("[]"))
			hypLen = 0;
		if (refList.toString().equals("[]"))
			refLen = 0;
		deletions = ( hypLen > refLen)? (hypLen -  refLen) : 0 ;
		insertions = (hypLen < refLen)? ( refLen - hypLen) : 0 ;
		substitutions = (deletions > 0)? refLen : 0;
		if (insertions > 0){
			substitutions = hypLen;
		}
		if ((hypLen == refLen) && (hypLen > 0) )
			substitutions = hypLen;
		// substitutions = ( (hypLen == refLen) && (hypLen == 1) ) ? 1 : 0;
		return (deletions + insertions + substitutions);
	}

	/**
	 * Computes word error rate denominator, i.e. the number of words contained by the reference utterance. 
	 *@return the length of the reference utterance.   
	 */
	public float computeDenominator() {
			return ref.length;
	}
	
	/*
	 * Gets the number of substitutions required in order to obtain ref from hyp.
	 */
	public int getDeletions() {
		return deletions;
	}

	/*
	 * Gets the correct number of words
	 * (i.e. the number of words that are present in both the reference utterance and the hypothetised utterance).
	 */
	public int getNoRightWords() {
		return noRightWords;
	}
	
	/*
	 * Gets the number of insertions required in order to obtain ref from hyp.
	 */
	public int getInsertions() {
		return insertions;
	}
	
	public int getSubstitutions() {
		return substitutions;
	}
	
	public String[] getHyp() {
		return hyp;
	}

	public String[] getRef() {
		return ref;
	}

	public java.util.Vector<String> getRefList() {
		return refList;
	}

	public java.util.Vector<String> getHypList() {
		return hypList;
	}
	
	public void setHyp(String hypStr) {
		 hyp = hypStr.split(wordDeliminator);

	}

	public void setRef(String refStr) {
		 ref = refStr.split(wordDeliminator);
	}

	/**
	 * WordErrorRate's main can be called with two arguments of type String and containing: 
	 * <br/> 1) the reference utterance;  
	 * <br/> 2) the hypothetised utterance. 
	 * @param args
	 */
public static void main(String[] args){
	 System.out.println("This is an implementation of Word Error Rate. \n" +
	 		"WordErrorRate's main can be called with two arguments of type String " +
	 		"and containing: \n" +
	 		" 1) the reference utterance ;  \n 2) the hypothetised utterance. ");
	 if ((args.length < 2)) {
		 System.out.println("Reference or hypothetised utterance was not specified !!! "); 
		 System.exit(1);
	 }
	 if ((args[0] == null ) || (args[1] == null )) {
		 System.out.println("Reference or hypothetised utterance was not specified !!! ");
		 System.exit(1);
	 }
		
	 WordErrorRate wer = new WordErrorRate(args[0], args[1], " ");
	 
	float wer_numerator = wer.computeNumerator();
	if (wer.computeDenominator() == 0 ){
		 System.out.println("\n WARNING: There are no words inside the reference utterance!!!");
	 }
	else{
		System.out.println();
		System.out.format("WER numerator = %f %n" , wer_numerator);
		System.out.format("Insertions = %d %n" , wer.getInsertions());
		System.out.format("Deletions = %d %n" , wer.getDeletions());
		System.out.format("Substitutions = %d %n" , wer.getSubstitutions());
		System.out.print(String.format("WER denominator = %f %n" , wer.computeDenominator()));
		float wer_aux = (float)wer_numerator / (float)wer.computeDenominator();
		System.out.println("Word Error Rate  = " + wer_aux + " ");
		
		System.out.println("Ref  = " + wer.getRefList().toString() + " ");
		System.out.println("Hyp  = " + wer.getHypList().toString() + " ");
		
		System.out.println("Ref str len = " + wer.getRefList().toString().length() + " ");
		System.out.println("Ref len = " + wer.getRefList().size() + " ");
		System.out.println("Hyp len = " + wer.getHypList().size() + " ");
		
	}
}

}
