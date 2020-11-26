package issco.eval;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.logging.Logger;

/**
 * Class useful for computing the word Precision, word Recall and Word Error Rate for a couple of utterances 
 * given in two annotation files.
 * @author Maria GEORGESCUL, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 */

public class ERs4TwoAnnotFiles {

	static private Logger logger = Logger.getLogger(ERs4TwoAnnotFiles.class.getName());

	/**
	 * @param inFileName
	 * @return
	 */
	static BufferedReader getBufferedReader(String inFileName) throws IOException{
		try{
			java.io.FileReader fis = new java.io.FileReader(inFileName);
			BufferedReader br = new BufferedReader(fis);
			return br;
		}
		catch(java.io.FileNotFoundException ex){
			// Logger logger = Logger.getLogger(ERs4TwoAnnotFiles.class.getName());
			logger.severe("File not found : " + inFileName);
			// throw IOException;	
			return null;
		}		
	}
	
	/**
	 * 
	 * @param inFileName
	 * @return
	 */
	static java.io.BufferedWriter getBufferedWriter(String fileName) throws java.io.IOException{		 
		try{
			java.io.Writer fis = new java.io.FileWriter(fileName);
			java.io.BufferedWriter br = new java.io.BufferedWriter(fis);
			return br;
		}
		catch(java.io.FileNotFoundException ex){
			logger.severe("File not found : " + fileName);
			// System.exit(1);
			return null;
		}	
		catch(java.io.IOException ex){
			logger.severe(fileName + " file exists but is a directory rather than a regular file, or" +
					" does not exist and cannot be created, or" +
					" cannot be opened for any other reason. " );
			// System.exit(1);
			return null;
		}	
	}
	

	/**
	 * Precision, Recall and Word Error Rate computed for two annotation files. 
	 * Calling the method for reference and hypothesis files 
	 * ignores the rest of the lines when one of the files reaches the end
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException{
		BufferedReader refBR = null;
		BufferedReader hypBR = null;
		BufferedWriter errOutBR = null;
		try{
			if(args.length != 3){
				System.out.println("Please provide three file names corresponding to reference, hypothesis and output file.");
				return;
			}
			refBR = getBufferedReader(args[0]);
			hypBR = getBufferedReader(args[1]);
			errOutBR = getBufferedWriter(args[2]);
		
			if ((refBR == null) || (hypBR == null) || (errOutBR == null)){
				return;
			}
			String refLine;
			String hypLine;
			float WER=0;
			int no_utt = 0;
			
			errOutBR.write("Reference Sentence \t Hypothetised sentence \t " +
					"Words in Reference Sentence (alfabetical order) \t Words in Hypothetised sentence \t " +
					"Words that are in ref but not in hyp \t Words that are in hyp but not in ref \t " +
					"Number of words that are both in ref and hyp " +
					"\t Precision \t Percentage Precision " +
					"\t Recall \t Percentage Recall " +
					"\t WER \t Insertions \t Deletions \t Substitutions \n");
			while (((refLine = refBR.readLine()) != null) && ((hypLine = hypBR.readLine()) != null)){
					errOutBR.write(refLine);
					errOutBR.write("\t");
					errOutBR.write(hypLine);
					errOutBR.write("\t");
	      	
					WordErrorRate wer = new WordErrorRate(refLine, hypLine, " ");
					java.util.Vector<String> refListInitial = wer.getRefList();
					java.util.Vector<String> hypListInitial = wer.getHypList();
					errOutBR.write(refListInitial.toString());
					errOutBR.write("\t");
					errOutBR.write(hypListInitial.toString());
					errOutBR.write("\t");
      	   
					float wer_num = (float)wer.computeNumerator();           
					float current_WER = wer_num / (float)wer.computeDenominator() ;
					int correct =  wer.getNoRightWords() ;           
					int hyp_len = wer.getHyp().length;
					int ref_len = wer.getRef().length;           
             
					// float precision = correct / (float)hyp_len;
					// float recall = correct / (float)wer.getRef().length;
					errOutBR.write(wer.getRefList().toString());           
					errOutBR.write("\t");
					errOutBR.write(wer.getHypList().toString());
					errOutBR.write("\t");
					errOutBR.write(correct + "\t " + correct + "/" + hyp_len + 
        		   					"\t " + 100 * ((float) correct / (float)hyp_len) + "%"+
        		   					"\t " + correct + "/" + ref_len  + 
        		   					"\t " + 100 * ( (float)correct / (float)ref_len )  + "%"+
        		   					"\t" + current_WER + "\t" + wer.getInsertions() + "\t" + wer.getDeletions() + 
        		   					"\t" + wer.getSubstitutions() + "\n");
					WER += current_WER;
					no_utt++;
			}
			logger.info("Precision and Recall has been computed. Please see the results in " + args[2] + " file.");        
			errOutBR.flush();
		}
		catch(IOException e){
			logger.severe("IO exception occurs when writing to " + args[2]);		
		}
		finally{
			// logger.info(" Close files that are already open.");
			if (refBR != null){	
				refBR.close();
			}
			if (hypBR != null){	
				hypBR.close();
			}		
			if (errOutBR != null){	
				errOutBR.close();
			}
		}
	}
	
}
