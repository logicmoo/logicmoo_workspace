package issco.eval;


/**
 * TODO Define semantic error rate and implement the entire class
 * Class useful for computing Semantic Error Rate. 
 * @author Maria GEORGESCUL, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 * 
 */
public class SemanticErrorRate implements ErrorRate {
	String[] ref;
	String[] hyp;

	/**
	 * 
	 *
	 */
	public SemanticErrorRate(){
	}
	
	/**
	 * 
	 * @param refStr - reference utterance, where each two words are separated by a string specified by wordDeliminator
	 * @param hypStr - hypothetised utterance, where each two words are separated by a a string specified by wordDeliminator.
	 * @param wordDeliminator - A string or a character (e.g. a space, a comma, etc.) used to delimit text into words. 
	 */
	public SemanticErrorRate(String refStr, String hypStr, String wordDeliminator){
		ref = refStr.split(wordDeliminator);
		hyp = hypStr.split(wordDeliminator);
	}
	
	/**
	 * Computes word error rate denominator, i.e. the number of words contained by the reference utterance. 
	 *@return the length of the reference utterance.   
	 */
	public float computeDenominator() {
			return ref.length;
	}

	/**
	 * Computes the numerator for semantic error rate. 
	 */
	public float computeNumerator() {
		// TODO implementation of the method
		return 0;
	}

	public String[] getHyp() {
		return hyp;
	}

	public String[] getRef() {
		return ref;
	}

	public void setHyp(String hypStr) {
		 hyp = hypStr.split(" ");

	}

	public void setRef(String refStr) {
		 ref = refStr.split(" ");
	}

	/**
	 * SemanticErrorRate's main can be called with two arguments of type String and containing: 
	 * <br/> 1) the reference utterancel;  
	 * <br/> 2) the hypothetised utterance. 
	 * @param args
	 */
	public static void main(String[] args){
		
	 System.out.println("This is an implementation of Semantic Error Rate. " +
	 		"\nWordErrorRate's main can be called with two arguments of type String " +
	 		"and containing \n 1) " +
	 		"the reference utterance  \n 2) the hypothetised utterance. ");
	 if ((args.length < 2)) {
		 System.out.println("Reference or hypothetised utterance was not specified !!! "); 
		 		// " \nPlease provide two arguments of type String: \n1) the first argument should contain the reference utterance  \n2) the second argument should contain the hypothetised utterance.");
		 System.exit(1);
	 }
	 if ((args[0] == null ) || (args[1] == null )) {
		 System.out.println("Reference or hypothetised utterance was not specified !!! ");
		 // System.out.println("Please provide two arguments of type String: \n1) the first argument should contain the reference utterance; \n2) the second argument should contain the hypothetised utterance. ");
		 System.exit(1);
	 }

	 SemanticErrorRate ser = new SemanticErrorRate(args[0], args[1], " ");

	float ser_numerator = ser.computeNumerator();
	if (ser.computeDenominator() == 0 ){
		 System.out.println("\n WARNING: There are no words inside the reference utterance!!!");
	 }
	else{
		System.out.println();
		System.out.println("WER numerator   = " + ser_numerator + " ");
		 System.out.println("WER denominator = " + ser.computeDenominator()  + " ");
		float wer_aux = (float)ser_numerator / (float)ser.computeDenominator();
		System.out.println("Semantic Error Rate  = " + wer_aux + " ");
	}
}

}
