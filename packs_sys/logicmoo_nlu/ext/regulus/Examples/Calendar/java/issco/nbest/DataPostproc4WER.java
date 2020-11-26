package issco.nbest;

import issco.eval.WordErrorRate;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.jdom.*;

/** Class useful for data post-processing required for computing Word Error Rate (WER)
 *  after nbest re-ranking. 
 * Re-ranking can be made either by naive baselines algorithms or 
 * by machine learning techniques).
 *@author GEORGESCUL Maria, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 *@see DataPostProc4ER 
 */
public class DataPostproc4WER extends DataPostProc4ER {

	/**
	 * Gets Word Error Rate (WER) for re-ranking predicted by the machine learning technique.
	 * @param xmlFileName the XML file name containing the reference data 
	 * (i.e. the human utterance transcription). 
	 * @param svmPredFileName - file name of the hypothetised data (i.e. the re-estimated recognition ) 
	 * @return Word Error Rate
	 * @throws Exception
	 */
	
	public float[] getER4Predicted(String xmlFileName, String svmPredFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			BufferedReader br = getBufferedReader(svmPredFileName);
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to compute Word Error Rate (WER), 
				// get the correct transcription
				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();
				// System.out.println("Correct transcript " + transcriptionElem.getValue());
					
				// In the xml tree: find hyp_trancription, i.e. the corresponding transcription 
				// (of the 1-best predicted hypothesis) 		
				java.util.List recList = nbestElem.getChildren("recognition");
			
//				 Find the hypothesis that is classified by svms as min in rank:
				float bestRank = 100000; 
				int bestRankIdx = -1;
				// Take from reranking predictions file: the index of the 1-best 
				for (int k = 1; k < recList.size(); k++){
					String thisLine = br.readLine();
					if (thisLine == null)
						throw new Exception("End of file in predictions file before reading the entire xml file!");
					float predictedRank = new Float(thisLine).floatValue();
					if (bestRank > predictedRank){
						bestRankIdx = k;
						bestRank = predictedRank;
					}
				}
				// bestRankIdx = 1; // just for checking what's the WER for always choosing first hyp as the best
				
				// System.out.println("best rank = " + bestRankIdx);			
				//if (recList.size() < bestRankIdx)
					// System.out.println("Less than " + bestRankIdx + " recognitions !!! ");
				
				Element recElem = (Element) recList.get(bestRankIdx);

				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();

				WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
				werArray[i] = (float)wer.computeNumerator() / (float) wer.computeDenominator() ; 			
			}
			
			String thisLine;
			if (( thisLine = br.readLine()) != null ){
				System.out.println("There are still some predictions while the xml file reached endOfFile!!! ");
				System.out.println(thisLine);
			}
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}
	
	
	/**
	 * Takes randomly one of the 6-best hypothesis as the best one.
	 * Then, simply computes the word error rate (WER).  
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public float[] getER4RandomHyp(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;
			
			java.util.Random rand = new java.util.Random();
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();
				// System.out.println("Correct transcript " + transcriptionElem.getValue());
					
				// In the xml tree: find hyp_trancription, i.e. the corresponding transcription 
				// (of the 1-best predicted hypothesis) 		
				java.util.List recList = nbestElem.getChildren("recognition");
			
				int rankIdx = rand.nextInt(recList.size()-1) + 1 ; 
				Element recElem = (Element) recList.get(rankIdx);

				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();

				WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
				werArray[i] = (float)wer.computeNumerator() / (float) wer.computeDenominator() ; 			

			}
			
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Takes the first in rank hypothesis.
	 * Then, simply computes word error rate (WER).  
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public float[] getER4Rank1Hyp(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;
					
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();
				// System.out.println("Correct transcript " + transcriptionElem.getValue());
					
				// In the xml tree: find hyp_trancription, i.e. the corresponding transcription 
				// (of the 1-best predicted hypothesis) 		
				java.util.List recList = nbestElem.getChildren("recognition");
			
				int rankIdx = 1 ; 
				Element recElem = (Element) recList.get(rankIdx);

				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();

				WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
				werArray[i] = (float)wer.computeNumerator() / (float) wer.computeDenominator() ; 			

			}
			
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Gets the SER for linear re-ranking. Two main steps are performed:
	 *  * 1) Performs re-ranking using a linear function, 
	 * which takes into account ..., i.e. 
	 * newRank = ...
	 * <br>
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the semantic error rate (SER) as defined in the "semantically_correct" tag 
	 * 
	 */
	public float[] getER4LinearReranking(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;
			int minNewRankID = 1;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 

				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();

				java.util.List recList = nbestElem.getChildren("recognition");
			
				///////////////////////////////////////////
				int minNewRank = 100;		
				for (int j = 1; j < recList.size(); j++ ){
					Element recElem = (Element) recList.get(j);
					int rank = new Integer(recElem.getChild("rank").getValue()).intValue();
					int no_dialogue_move = new Integer(recElem.getChild("no_dialogue_move").getValue()).intValue();
					int underconstrained_query = new Integer(recElem.getChild("underconstrained_query").getValue()).intValue();
					int non_indefinite_existential = new Integer(recElem.getChild("non_indefinite_existential").getValue()).intValue();
					int non_show_imperative = new Integer(recElem.getChild("non_show_imperative").getValue()).intValue();
					int indefinite_meeting_and_meeting_referent = new Integer(recElem.getChild("indefinite_meeting_and_meeting_referent").getValue()).intValue();
				
					/*				 
					% 1) Place in the N-best list - lower number is better
					feature_weight(rank, -1).
					% 2) Strongly penalise hypotheses that produce no dialogue move
					feature_weight(no_dialogue_move, -50).
					% 3) Penalise queries with no contentful constraints
					feature_weight(underconstrained_query, -10).
					% 4) Penalise existentials which aren't indefinite, e.g. "is there the meeting next week"
					feature_weight(non_indefinite_existential, -10).
					% 5) Strongly penalise imperatives where the main verb isn't "show" or something similar
					feature_weight(non_show_imperative, -50).
					% 6) Disprefer combination of indefinite mention of meeting + available meeting referent
					feature_weight(indefinite_meeting_and_meeting_referent, -2).
*/
					int newRank = rank + 50 * no_dialogue_move
						+ 10* underconstrained_query + 10 * non_indefinite_existential
						+ 50 * non_show_imperative + 2 * indefinite_meeting_and_meeting_referent;
					if (newRank < minNewRank){
						minNewRank = newRank;
						minNewRankID = j;
					}
				}
				
				Element recElem = (Element) recList.get(minNewRankID);
				
				//////////////////////////////////////////	
				// System.out.println("best rank = " + minNewRankID);			
				if (recList.size() < minNewRankID )
					 System.out.println("Less than " + minNewRankID + " recognitions !!! ");
				
				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();
				
				WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
				werArray[i] = (float)wer.computeNumerator() / (float) wer.computeDenominator() ; 			
			}
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		catch (NullPointerException nullE) {nullE.printStackTrace();}
		return null;
	}

	
	
	/**
	 * 1) Performs re-ranking using a linear function, 
	 * which takes into account only three features.
	 * <br>
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the word error rate (WER).  
	 * deprecated Use instead getWER4LinearReranking method which uses more than 3 features.
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 * 
	 */
	public float[] getER4LinearReranking3Feat(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;			
			int minNewRankID = 1;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();
				// System.out.println("Correct transcript " + transcriptionElem.getValue());
					
				// In the xml tree: find hyp_transcription, 
				// i.e. the transcription corresponding to the 1-rank predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");
									
//				int newRank =  
				// rand.nextInt(recList.size()-1) + 1 ; // just for checking what's the WER for always choosing first hyp as the best
				//System.out.println("Random rank = " + rankIdx);
				int minNewRank = 100;		
				for (int j = 1; j < recList.size(); j++ ){
					Element recElem = (Element) recList.get(j);
					int rank = new Integer(recElem.getChild("rank").getValue()).intValue();
					int uq = new Integer(recElem.getChild("underconstrained_query").getValue()).intValue();
					int it = new Integer(recElem.getChild("inconsistent_tense").getValue()).intValue();
					
					int newRank = rank + 10* uq + 10* it;
					if (newRank < minNewRank){
						minNewRank = newRank;
						minNewRankID = j;
					}
				}
				
				Element recElem = (Element) recList.get(minNewRankID);
				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();
				
				WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
				werArray[i] = (float)wer.computeNumerator() / (float) wer.computeDenominator() ; 			

			}
			
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Compute the best word error rate that can be achieved starting from n-best hypothesis.  
	 * @param xmlFileName
	 * @return a list of error rates corresponding to each utterance 
	 * @throws Exception
	 * 
	 */
	public float[] getBestERCanBeAchieved(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] werArray = new float[nbestList.size()];
			int noUtt = 0;
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
				Element transcriptionElem = nbestElem.getChild("correct_words");				
				String refTranscription =  transcriptionElem.getValue();
					
				java.util.List recList = nbestElem.getChildren("recognition");
			
				float bestWER = 10000;
				// (start with k=1 since we skip first value in recList which corresponds to the correct transcription)
				for (int k = 1; k < recList.size(); k++){
					Element recElem = (Element) recList.get(k);

					Element recWordsElem = recElem.getChild("recognised_words");
					String recWords =  recWordsElem.getValue();
					WordErrorRate wer = new WordErrorRate(refTranscription, recWords, wordDeliminator);
					float wer_numerator = wer.computeNumerator(); 
					if (bestWER > wer_numerator){
						bestWER = wer_numerator;
					}
				}
				werArray[i] = bestWER / (float) refTranscription.split(this.wordDeliminator).length ; 			
			}
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}
	
	/**
		 * Public main 
		 * @param args
		 */
		public static void main(String[] args){
			DataPostproc4WER thisC = new DataPostproc4WER();

			String mainDir = "D:/svm_light/calendar_data/";
			String xmlDirName = mainDir + "xmlInFiles/";
		
			String errOutFN_lin = mainDir + "linearKernel/wer_out.txt";
			String svmPredDirName_lin = mainDir + "linearKernel/svmTestPredictions/";
			thisC.computeER4linearKernel(errOutFN_lin, xmlDirName, svmPredDirName_lin);

			String werOutFN = mainDir + "/rbfKernel/wer_out_300_1500_100.txt";
			String svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
			thisC.computeER4rbfKernel(werOutFN, xmlDirName, svmPredDirName_rbf, 300, 1500, 100);
			werOutFN = mainDir + "/rbfKernel/wer_out_130_280_10.txt";
			svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
			thisC.computeER4rbfKernel(werOutFN, xmlDirName, svmPredDirName_rbf, 130, 280, 10);
			werOutFN = mainDir + "/rbfKernel/wer_out_-256_256_2.txt";
			svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
			thisC.computeER4rbfKernel(werOutFN, xmlDirName, svmPredDirName_rbf, -256, 256, 2);

				
			String errOutFN_pol = mainDir + "polinomialKernel/wer_out.txt";
			String svmPredDirName_pol = mainDir + "polinomialKernel/svmTestPredictions/";
			thisC.computeER4polynomialKernel(errOutFN_pol, xmlDirName, svmPredDirName_pol);
		
			String errOutFN_rand = mainDir + "wer_randBaseline.txt";
			thisC.computeER4RandomBaseline(errOutFN_rand, xmlDirName);

			String errOutFN_linearReranking = mainDir + "wer_linearRerankingBaseline.txt";
			thisC.computeER4LinearReranking(errOutFN_linearReranking, xmlDirName);

			String errOutFN_bestToAchieve = mainDir + "wer_bestCanBeAchieved.txt";
			thisC.computeBestERCanBeAchieved(errOutFN_bestToAchieve, xmlDirName);
			
/*
			String errOutFN_linRerank = mainDir + "err_linear_reranking.txt";
			thisC.computeER4LinearReranking3Feat(errOutFN_linRerank, xmlDirName);
			*/
		}

	 
}
