package issco.nbest;

import issco.eval.WordErrorRate;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.jdom.*;

/** Class useful for data post-processing in order to compute semantic error rate once 
 * the nbest re-ranking has been done.
 * Re-ranking can be made either by naive baselines algorithms or 
 * by machine learning techniques).
 * 
 * @author GEORGESCUL Maria, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 * @see DataPostProc4ER 
 */
public class DataPostproc4SER extends DataPostProc4ER{

	/**
	 * Compute the best semantic error rate that can be achieved starting from n-best hypothesis.  
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
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE SEMANTIC ERROR RATE (SER)
				
				java.util.List recList = nbestElem.getChildren("recognition");
			
				float bestSER = 10000;
				// (start with k=1 since we skip first value in recList which corresponds to the correct transcription)
				for (int k = 1; k < recList.size(); k++){
					Element recElem = (Element) recList.get(k);

					Element semCorrectElem = recElem.getChild("semantically_correct");
					String semCorrect =  semCorrectElem.getValue();
					int ser;
					if (semCorrect.equalsIgnoreCase("good"))
						ser = 0;
					else 
						ser = 1;

					if (bestSER > ser){
						bestSER = ser;
					}
				} // end for k 
				serArray[i] = bestSER  ; 			
			}
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	
	/**
	 * Gets the SER for re-ranking predicted by the machine learning technique.
	 * @param xmlFileName the XML file name containing the reference data 
	 * (i.e. the human utterance transcription). 
	 * @param svmPredFileName - file name of the hypothetised data (i.e. the re-estimated recognition ) 
	 * @return Semantic Error Rate
	 * @throws Exception
	 */
	public float[] getER4Predicted(String xmlFileName, String svmPredFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			BufferedReader br = getBufferedReader(svmPredFileName);
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
				
				// In the xml tree: find utterance features for the 1-best predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");
			
//				 Find the hypothesis that is classified by re-ranking procedure as minimal in rank:
				float bestRank = 100000; 
				int bestRankIdx = -1;
				// Take from predictions file: the index of the 1-best 
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
				// bestRankIdx = 1; // just for checking what's the SER for always choosing first hyp as the best
				// System.out.println("best rank = " + bestRankIdx);			
				if (recList.size() < bestRankIdx )
					 System.out.println("Less than " + bestRankIdx + " recognitions !!! ");
				
				Element recElem = (Element) recList.get(bestRankIdx);

				Element semCorrectElem = recElem.getChild("semantically_correct");
				String semCorrect =  semCorrectElem.getValue();
				if (semCorrect.equalsIgnoreCase("good"))
					serArray[i] = 0;
				else 
					serArray[i] = 1;
			}
			
			String thisLine;
			if (( thisLine = br.readLine()) != null ){
				System.out.println("There are still some " +
						"predictions while the xml file reached endOfFile!!! ");
				System.out.println(thisLine);
			}
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		catch (NullPointerException nullE) {nullE.printStackTrace();}
		return null;
	}

	/**
	 * Gets the SER for a naive algorithm which does random re-ranking.
	 * @param xmlFileName
	 * @param svmPredFileName
	 * @return
	 * @throws Exception
	 */
	public float[] getER4RandomHyp(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;
			java.util.Random rand = new java.util.Random();
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
				java.util.List recList = nbestElem.getChildren("recognition");
			
				int bestRankIdx = rand.nextInt(recList.size()-1) + 1 ;
				
				// System.out.println("best rank = " + bestRankIdx);			
				if (recList.size() < bestRankIdx )
					 System.out.println("Less than " + bestRankIdx + " recognitions !!! ");
				
				Element recElem = (Element) recList.get(bestRankIdx);

				Element semCorrectElem = recElem.getChild("semantically_correct");
				String semCorrect =  semCorrectElem.getValue();
				if (semCorrect.equalsIgnoreCase("good"))
					serArray[i] = 0;
				else 
					serArray[i] = 1;
			}
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		catch (NullPointerException nullE) {nullE.printStackTrace();}
		return null;
	}

	/**
	 * Gets the SER for a naive algorithm which does random re-ranking.
	 * @param xmlFileName
	 * @param svmPredFileName
	 * @return
	 * @throws Exception
	 */
	public float[] getER4Rank1Hyp(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
				java.util.List recList = nbestElem.getChildren("recognition");
			
				int bestRankIdx = 1 ;
				
				System.out.println("best rank = " + bestRankIdx);			
				if (recList.size() < bestRankIdx )
					 System.out.println("Less than " + bestRankIdx + " recognitions !!! ");
				
				Element recElem = (Element) recList.get(bestRankIdx);

				Element semCorrectElem = recElem.getChild("semantically_correct");
				String semCorrect =  semCorrectElem.getValue();
				if (semCorrect.equalsIgnoreCase("good"))
					serArray[i] = 0;
				else 
					serArray[i] = 1;
			}
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		catch (NullPointerException nullE) {nullE.printStackTrace();}
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
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;
			int minNewRankID = 1;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
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
				
				Element semCorrectElem = recElem.getChild("semantically_correct");
				String semCorrect =  semCorrectElem.getValue();
				if (semCorrect.equalsIgnoreCase("good"))
					serArray[i] = 0;
				else 
					serArray[i] = 1;
			}
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		catch (NullPointerException nullE) {nullE.printStackTrace();}
		return null;
	}


	/**
	 * 1) Performs re-ranking using a linear function, 
	 * which takes into account only three features:
	 * newRank = 1 * oldRank + 10 * underconstrained_query + 10 * incosistent_tense;
	 * <br>
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the semantic error rate (SER) as follows:
	 * -- a recognition is considered as semantically correct if the dialogue move 
	 * representation it produces is both a) non-null and 
	 * b) the same as the one that would have been produced from a perfect recognition result.
	 * -- if dialogue move is not the same: then counts the number of deletions/insertions required in order to obtain the perfect dialogue move.
	 *   @deprecated Use instead the linear re-ranking with more than 3 feat
	 *   and SER manually defined (available in xml file) 
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public float[] getER4LinearReranking3Feat(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			float[] serArray = new float[nbestList.size()];
			int noUtt = 0;			
			int minNewRankID = 1;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE SEMANTIC ERROR RATE (ser), 
				// get the dialogue_move feature value for the correct transcription 
				// dialogue_move
				Element dmElem = nbestElem.getChild("dialogue_move");	
				String refDM = "";
				if ( dmElem != null)
					if (!dmElem.getValue().equalsIgnoreCase(""))
						refDM =  dmElem.getValue();
					
				// In the xml tree: find hyp_transcription, 
				// i.e. the transcription corresponding to the 1-rank predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");
				
				// PERFORM LINEAR RE-RANKING
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
				Element dm4recElem = recElem.getChild("dialogue_move");
				String dm4rec = "";
				if (dm4recElem != null) 
					if (!dm4recElem.getValue().equalsIgnoreCase(""))
						dm4rec =  dm4recElem.getValue();

				WordErrorRate wer = new WordErrorRate(refDM, dm4rec, this.wordDeliminator);
				serArray[i] = wer.computeNumerator() ;
			}
			
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Computes semantic error rate (SER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using the following linear function:
	 * rank' = rank + 10 * underconstrained_query + 10 * incosnsistent_tense.
	 * The effect is to pick the first (i.e. smallest in rank) hypothesis 
	 * which doesn't fail on one of underconstrained_query or incosnsistent_tense.    
	 *@deprecated
	 */
	public void computeER4LinearRerankingBaseline3Feat(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] wer = getER4LinearReranking(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< wer.length; i++){
					averageSER += wer[i]; 
				}
				averageSER = averageSER / (float) wer.length;
				fiveFoldCV_sumWER += averageSER;
				errOutBR.write("Linear reranking, for fold no  = " + k + " , the average WER = " + averageSER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("Linear reranking, for fold no  = " + k + " , the average WER = " + averageSER*100 + "%");
			}
				
				//System.out.println("Linear reranking, Average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Linear reranking, Average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	
	/**
	 * Public main 
	 * @param args
	 */
	public static void main(String[] args){
		DataPostproc4SER thisC = new DataPostproc4SER();
		
		String mainDir = "D:/svm_light/calendar_data/";
		String xmlDirName = mainDir + "xmlInFiles/";
		/*
		String errOutFN_linKernel = mainDir + "linearKernel/ser_out.txt";
		String svmPredDirName_lin = mainDir + "linearKernel/svmTestPredictions/";
		thisC.computeER4linearKernel(errOutFN_linKernel, xmlDirName, svmPredDirName_lin);
	
		String serOutFN = mainDir + "/rbfKernel/ser_out_300_1500_100.txt";
		String svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
		thisC.computeER4rbfKernel(serOutFN, xmlDirName, svmPredDirName_rbf, 300, 1500, 100);
		serOutFN = mainDir + "/rbfKernel/ser_out_130_280_10.txt";
		svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
		thisC.computeER4rbfKernel(serOutFN, xmlDirName, svmPredDirName_rbf, 130, 280, 10);
		serOutFN = mainDir + "/rbfKernel/ser_out_-256_256_2.txt";
		svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
		thisC.computeER4rbfKernel(serOutFN, xmlDirName, svmPredDirName_rbf, -256, 256, 2);

		
		String errOutFN_pol = mainDir + "polinomialKernel/ser_out.txt";
		String svmPredDirName_pol = mainDir + "polinomialKernel/svmTestPredictions/";
		thisC.computeER4polynomialKernel(errOutFN_pol, xmlDirName, svmPredDirName_pol);
		
		String serOutFN_rand = mainDir + "ser_randBaseline.txt";
		thisC.computeER4RandomBaseline(serOutFN_rand, xmlDirName);

		serOutFN_rand = mainDir + "ser_rank1Baseline.txt";
		thisC.computeER4Rank1Baseline(serOutFN_rand, xmlDirName);

		String errOutFN_lin = mainDir + "ser_linearRerankingBaseline.txt";
		thisC.computeER4LinearReranking(errOutFN_lin, xmlDirName);
		*/

		String errOutFN_bestToAchieve = mainDir + "ser_bestCanBeAchieved.txt";
		thisC.computeBestERCanBeAchieved(errOutFN_bestToAchieve, xmlDirName);

		/*		
		String SER_OutFN_linRerank = mainDir + "sem_err_linear_reranking.txt";
		computeSER4LinearRerankingBaseline(SER_OutFN_linRerank, xmlDirName);
		 */		
	}
	
}
