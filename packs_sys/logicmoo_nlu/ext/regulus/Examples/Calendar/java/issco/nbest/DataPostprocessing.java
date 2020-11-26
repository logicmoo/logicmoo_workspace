package issco.nbest;

import issco.eval.WordErrorRate;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.jdom.*;

/** Class useful for data post-processing after nbest re-ranking. 
 * Re-ranking can be made either by naive baselines algorithms or 
 * by machine learning techniques).
 * @deprecated use DataPostproc4WER and DataPostproc4SER instead
 * @author GEORGESCUL
 * */
public class DataPostprocessing {
	private static String xmlFileName = "";
	private static final String wordDeliminator = ",";

		/**
	 * Gets Word Error Rate (WER) for re-ranking predicted by the machine learning technique.
	 * @param xmlFileName the XML file name containing the reference data 
	 * (i.e. the human utterance transcription). 
	 * @param svmPredFileName - file name of the hypothetised data (i.e. the re-estimated recognition ) 
	 * @return Word Error Rate
	 * @throws Exception
	 */
	
	public static float[] getWER4Predicted(String xmlFileName, String svmPredFileName) throws Exception{		
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
			
				// In order to COMPUTE WORD ERROR RATE (wer), 
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
				bestRankIdx = 1; // just for checking what's the WER for always choosing first hyp as the best
				
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
	 * Gets the SER for re-ranking predicted by the machine learning technique.
	 * @param xmlFileName the XML file name containing the reference data 
	 * (i.e. the human utterance transcription). 
	 * @param svmPredFileName - file name of the hypothetised data (i.e. the re-estimated recognition ) 
	 * @return Semantic Error Rate
	 * @throws Exception
	 */
	public static float[] getSER4Predicted(String xmlFileName, String svmPredFileName) throws Exception{		
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
	 * Gets the SER for a naive alg which does random re-ranking.
	 * @param xmlFileName
	 * @param svmPredFileName
	 * @return
	 * @throws Exception
	 */
	public static float[] getSER4RandomHyp(String xmlFileName) throws Exception{		
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
	public static float[] getSER4LinearReranking(String xmlFileName) throws Exception{		
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
	 * @param xmlFileName the xmlFileName to set
	 */
	public static void setXmlFileName(String xmlFileName) {
		DataPostprocessing.xmlFileName = xmlFileName;
	}


	/**
	 * @return the xmlFileName
	 */
	public static String getXmlFileName() {
		return xmlFileName;
	}


	/**
	 * Computes semantic error rate (SER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using the following linear function:
	 * rank' = ...
	 *
	 */
	public static void computeSER4LinearReranking(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] wer = getSER4LinearReranking(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< wer.length; i++){
					averageSER += wer[i]; 
				}
				averageSER = averageSER / (float) wer.length;
				fiveFoldCV_sumWER += averageSER;
				System.out.println("Linear reranking, for fold no  = " + k + " , the average SER = " + averageSER*100 + "%");
				errOutBR.write("Linear reranking, for fold no  = " + k + " , the average SER = " + averageSER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("Linear reranking, for fold no  = " + k + " , the average WER = " + averageSER*100 + "%");
			}
				
				//System.out.println("Linear reranking, Average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				System.out.println("Linear reranking, Average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Linear reranking, Average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}


	

	/**
	 * Takes randomly one of the 6-best hypothesis as the best one.
	 * Then, simply computes the word error rate (WER).  
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public static float[] getWER4RandomHyp(String xmlFileName) throws Exception{		
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
			
				int rankIdx = rand.nextInt(recList.size()-1) + 1 ; // just for checking what's the WER for always choosing first hyp as the best
				//System.out.println("Random rank = " + rankIdx);						
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
	 * 1) Performs re-ranking using a linear function, 
	 * which takes into account only three features.
	 * <br>
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the word error rate (WER).  
	 * @deprecated 
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 * 
	 */
	public static float[] getWER4LinearReranking(String xmlFileName) throws Exception{		
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
	 * 
	 * @param xmlFileName
	 * @return
	 * @throws Exception
	 */
	public static float[] getBestWERCanBeAchieved(String xmlFileName, String svmPredFileName) throws Exception{		
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
				werArray[i] = bestWER / (float) refTranscription.split(wordDeliminator).length ; 			
			}
			return werArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}
	
	/**
	 * 
	 * @param inFileName
	 * @return
	 */
	static BufferedReader getBufferedReader(String inFileName){
		java.io.FileInputStream fis;
		BufferedReader br;
		try{
			fis = new java.io.FileInputStream(inFileName);
			br = new BufferedReader(new InputStreamReader(fis));
			return br;
		}
		catch(java.io.FileNotFoundException ex){
			return null;
		}		
	}
	
	/**
	 * 
	 * @param inFileName
	 * @return
	 */
	static java.io.BufferedWriter getBufferedWriter(String inFileName){
		java.io.FileOutputStream fis;
		java.io.BufferedWriter br;
		try{
			fis = new java.io.FileOutputStream(inFileName);
			br = new java.io.BufferedWriter(new OutputStreamWriter(fis));
			return br;
		}
		catch(java.io.FileNotFoundException ex){
			return null;
		}		
	}
	
	/**
	 * Public main 
	 * @param args
	 */
	public static void main(String[] args){
		//String mainDir = "D:/svm_light/nbest_allFeatures/";
		String mainDir = "C:/maria/calendar_data/";
		//String mainDir = "D:/svm_light/nbest_3Features_rank3vals/";
		String xmlDirName = mainDir + "xmlInFiles/";
		/*
		String errOutFN_lin = mainDir + "linearKernel/err_out.txt";
		String svmPredDirName_lin = mainDir + "linearKernel/svmTestPredictions/";
		// computSER4linearKernel(String errOutFN, String xmlDirName, String svmPredDirName)
		computeSER4linearKernel(errOutFN_lin, xmlDirName, svmPredDirName_lin);
	
		
		String errOutFN_rbf = mainDir + "/rbfKernel/ser_out.txt";
		String svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
		computeSER4rbfKernel(errOutFN_rbf, xmlDirName, svmPredDirName_rbf);
	
		String errOutFN_pol = mainDir + "polinomialKernel/ser_out.txt";
		String svmPredDirName_pol = mainDir + "polinomialKernel/svmTestPredictions/";
		computeSER4polynomialKernel(errOutFN_pol, xmlDirName, svmPredDirName_pol);
		
		String errOutFN_rand = mainDir + "ser_randBaseline.txt";
		computeSER4RandomBaseline(errOutFN_rand, xmlDirName);

		String errOutFN_lin = mainDir + "ser_linBaseline.txt";
		computeSER4LinearReranking(errOutFN_lin, xmlDirName);
		// computeSemPrecision4LinearRerankingBaseline(SER_OutFN_linRerank, xmlDirName);
		*/
		
		/*
		String errOutFN_lin = mainDir + "linearKernel/wer_out.txt";
		String svmPredDirName_lin = mainDir + "linearKernel/svmTestPredictions/";
		computeWER4linearKernel(errOutFN_lin, xmlDirName, svmPredDirName_lin);
*/		
		String werOutFN = mainDir + "/rbfKernel/wer_out.txt";
		String svmPredDirName_rbf = mainDir + "rbfKernel/svmTestPredictions/";
		computeWER4rbfKernel(werOutFN, xmlDirName, svmPredDirName_rbf);
	/*	
		String errOutFN_pol = mainDir + "polinomialKernel/err_out.txt";
		String svmPredDirName_pol = mainDir + "polinomialKernel/svmTestPredictions/";
		computeWER4polynomialKernel(errOutFN_pol, xmlDirName, svmPredDirName_pol);
		
		String errOutFN_rand = mainDir + "err_randBaseline.txt";
		computeWER4RandomBaseline(errOutFN_rand, xmlDirName);

		String errOutFN_linRerank = mainDir + "err_linear_reranking.txt";
		computeWER4LinearRerankingBaseline(errOutFN_linRerank, xmlDirName);
		
		String SER_OutFN_linRerank = mainDir + "sem_err_linear_reranking.txt";
		computeSER4LinearRerankingBaseline(SER_OutFN_linRerank, xmlDirName);
*/
		/*
		String SER_OutFN_linRerank = mainDir + "sem_err_linear_reranking.txt";
		computeSemPrecision4LinearRerankingBaseline(SER_OutFN_linRerank, xmlDirName);
		
		String SER_OutFN_1best = mainDir + "sem_err_1best.txt";
		computeSemPrecision4NoRerankingBaseline(SER_OutFN_1best, xmlDirName);

		*/
		
		/* 
		 // Just checking whether the error rate is much different 
		 // when no 5-fold division is done (a small difference can occur because approcximations when computing the average)		
	try{
		String xmlFileNameAll = "D:/work/MEDSLTrelated/n-best/features_from_Manny/march2008/nbest.xml";
		boolean[] ser = getSemPrecisionNoReranking(xmlFileNameAll);
		
		float averageSER = 0;
		for (int i = 0; i< ser.length; i++){
			if (ser[i])
				averageSER += 1; 
		}
		averageSER = averageSER / (float) ser.length;
		System.out.println("1-best semantic precision on whole data (no 5-fold divided data) and no re-ranking = " + (averageSER*100) + "%");
	}
	catch(Exception e){};
		*/

		/*
		 
		 //RANDOM RERANKING 
		String SER_OutFN_randRerank = mainDir + "sem_err_random_reranking.txt";
		computeSemPrecision4RandomRerankingBaseline(SER_OutFN_randRerank, xmlDirName);
*/
		

		
	}
	/**
	 * Private static method for computing the error rate of the SVM predictions.
	 * The predictions were obtained using 5-fold SVM training with RBF kernel and varying the gamma parameter.
	 * The radial basis function: exp(-gamma ||a-b||^2).
	 *
	 */
	
	static void computeSER4rbfKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			// for %%n in (-256 -128 -64 -32 -16 -8 -4 -2 -1 1 2 4 8 16 32 64 128 256 ) do svm_classify nbest_adhoc2/nbest_test_fold%%j.txt nbest_adhoc2/fold%%i_model2_t2_g%%n.txt nbest_adhoc2/predictions_%%i_%%j_t2_g%%n.txt > nbest_adhoc2/out_classify_%%i_%%j_t2_g%%n.txt
	// for (int n = -256; n<257;){
			for (int n = 300; n<1600; n+=100){		
				float fiveFoldCV_sumSER = 0;
				int k;
				for (k = 1; k<6;k++){
					String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t2_g" + n + ".txt";
					// check whether this prediction file exists
					File f = new File(svmPredFileName);
					if (f.exists()){
						float[] wer = getSER4Predicted(xmlFileName, svmPredFileName);
						if (wer == null){}
						else{
							float averageSER = 0;
							for (int i = 0; i< wer.length; i++){
								averageSER += wer[i]; 
							}	
							averageSER = averageSER / (float) wer.length;
							fiveFoldCV_sumSER += averageSER;
						//System.out.println("For fold no  = " + k + "; gamma = " + n + " , the SER = " + 100*averageWER + "%");
						//errOutBR.write("For gamma = " + n + " and fold no  = " + k + " , the SER = " + 100*averageWER + "%");
						//errOutBR.write("\n");
						}
					}
					else {
						k = 1000;
					}
				}
				
				if (k >= 1000){
					System.out.println("For gamma = " + n + " the reranking is not available ");
					errOutBR.write("For gamma = " + n + " the reranking is not available ");
					errOutBR.write("\n");	
				}
				else{
					System.out.println("For gamma = " + n + " , the average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
					errOutBR.write("For gamma = " + n + " , the average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
					errOutBR.write("\n");
				}
			/*	
				if (n == -1)
					n = 1;
				else{
					if (n < 0)
						n /= 2;
					if (n > 0)
						n *=2;
				}
		*/		
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
	}
	
	/**
	 * Private static method for computing the error rate of the SVM predictions.
	 * The predictions were obtained using 5-fold SVM training with RBF kernel and varying the gamma parameter.
	 * The radial basis function: exp(-gamma ||a-b||^2).
	 *
	 */
	
	private static void computeWER4rbfKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			// for %%n in (-256 -128 -64 -32 -16 -8 -4 -2 -1 1 2 4 8 16 32 64 128 256 ) do svm_classify nbest_adhoc2/nbest_test_fold%%j.txt nbest_adhoc2/fold%%i_model2_t2_g%%n.txt nbest_adhoc2/predictions_%%i_%%j_t2_g%%n.txt > nbest_adhoc2/out_classify_%%i_%%j_t2_g%%n.txt
			for (int n = 300; n<1600; n+=100){
			// for (int n = -256; n<257;){
				float fiveFoldCV_sumWER = 0;
				for (int k = 1; k<6;k++){
					String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t2_g" + n + ".txt";

					// check whether this prediction file exists
					File f = new File(svmPredFileName);
					if (f.exists()){
						float[] wer = getWER4Predicted(xmlFileName, svmPredFileName);
			
						float averageWER = 0;
						for (int i = 0; i< wer.length; i++){
							averageWER += wer[i]; 
						}
						averageWER = averageWER / (float) wer.length;
						fiveFoldCV_sumWER += averageWER;
							//System.out.println("For fold no  = " + k + "; gamma = " + n + " , the WER = " + 100*averageWER + "%");
							errOutBR.write("For gamma = " + n + " and fold no  = " + k + " , the WER = " + 100*averageWER + "%");
							errOutBR.write("\n");
					}
					}
				
				//System.out.println("For gamma = " + n + " , the average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("For gamma = " + n + " , the average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("\n");
	/*
				if (n == -1)
					n = 1;
				else{
					if (n < 0)
						n /= 2;
					if (n > 0)
						n *=2;
				}
				*/
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
	}
	
	/**
 	 * Private static method for computing 
 	 * semantic error rate of the SVM re-ranking predictions.
 	 * The predictions were obtained using 5-fold SVM training with linear kernel.
	 *
	 */
	static void computeSER4linearKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumSER = 0;
			for (int k = 1; k<6;k++){				
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				String svmPredFileName = svmPredDirName + "predictions_" + k + "_t0.txt"; 

					float[] ser = getSER4Predicted(xmlFileName, svmPredFileName);
			
					float averageSER = 0;
					for (int i = 0; i< ser.length; i++){
						averageSER += ser[i]; 
					}
					averageSER = averageSER / (float) ser.length;
					fiveFoldCV_sumSER += averageSER;
					errOutBR.write("For fold no  = " + k + " , the average SER = " + averageSER *100 + "%");
					errOutBR.write("\n");
					System.out.println("For fold no  = " + k + " , the average SER = " + averageSER*100 + "%");
			}
				
				System.out.println(" Average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
				errOutBR.write("Average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};
		
	}

	/**
 	 * Private static method for computing the error rate of the SVM re-ranking predictions.
 	 * The predictions were obtained using 5-fold SVM training with linear kernel.
	 *
	 */
	static void computeWER4linearKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){				
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				String svmPredFileName = svmPredDirName + "predictions_" + k + "_t0.txt"; 

				float[] wer = getWER4Predicted(xmlFileName, svmPredFileName);
			
				float averageWER = 0;
				for (int i = 0; i< wer.length; i++){
					averageWER += wer[i]; 
				}
				averageWER = averageWER / (float) wer.length;
				fiveFoldCV_sumWER += averageWER;
				errOutBR.write("For fold no  = " + k + " , the average WER = " + averageWER *100 + "%");
				errOutBR.write("\n");
				System.out.println("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
			}
				
				//System.out.println(" Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};
		
	}
	/**
	 * Computes word error rate (WER) for a baseline naive algorithm,
	 * which takes randomly one of the 6-best hypothesis as the best one.  
	 *
	 */
	static void computeSER4RandomBaseline(String errOutFN, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumSER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] ser = getSER4RandomHyp(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< ser.length; i++){
					averageSER += ser[i]; 
				}
				averageSER = averageSER / (float) ser.length;
				fiveFoldCV_sumSER += averageSER;
				errOutBR.write("For fold no  = " + k + " , the average WER = " + averageSER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
			}
				
				//System.out.println(" Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}
	
	/**
	 * Computes word error rate (WER) for a baseline naive algorithm,
	 * which takes randomly one of the 6-best hypothesis as the best one.  
	 *
	 */
	static void computeWER4RandomBaseline(String errOutFN, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] wer = getWER4RandomHyp(xmlFileName);
			
				float averageWER = 0;
				for (int i = 0; i< wer.length; i++){
					averageWER += wer[i]; 
				}
				averageWER = averageWER / (float) wer.length;
				fiveFoldCV_sumWER += averageWER;
				errOutBR.write("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
			}
				
				//System.out.println(" Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}
	
	/**
	 * Computes word error rate (WER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using the following linear function:
	 * rank' = rank + 10 * underconstrained_query + 10 * incosnsistent_tense.
	 * The effect is to pick the first (i.e. smallest in rank) hypothesis 
	 * which doesn't fail on one of underconstrained_query or incosnsistent_tense.    
	 *
	 */
	public static void computeWER4LinearRerankingBaseline(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] wer = getWER4LinearReranking(xmlFileName);
			
				float averageWER = 0;
				for (int i = 0; i< wer.length; i++){
					averageWER += wer[i]; 
				}
				averageWER = averageWER / (float) wer.length;
				fiveFoldCV_sumWER += averageWER;
				errOutBR.write("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("For fold no  = " + k + " , the average WER = " + averageWER*100 + "%");
			}
				
				//System.out.println(" Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}
	
	/**
	 * Private static method for computing the error rate of the SVM predictions.
	 * The predictions were obtained using 5-fold SVM training with a polynomial kernel 
	 * and varying the d parameter, where the polynomial function : (s a*b+c)^d
	 *
	 */
	static void computeWER4polynomialKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			for (int n = 2; n<=18; n++){
				float fiveFoldCV_sumWER = 0;
				for (int k = 1; k<6;k++){
					
					setXmlFileName(xmlDirName + "nbest_test_fold" + k + ".xml");
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t1_d" + n + ".txt";

					float[] wer = getWER4Predicted(getXmlFileName(), svmPredFileName);
			
					float averageWER = 0;
					for (int i = 0; i< wer.length; i++){
						averageWER += wer[i]; 
					}
					averageWER = averageWER / (float) wer.length;
					fiveFoldCV_sumWER += averageWER;
					}
				
				errOutBR.write("For parameter d in polynomial kernel  = " + n + " , the average WER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("\n");
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
	}



	/**
	 * Private static method for computing the 
	 * semantic error rate of the SVM predictions.
	 * The predictions were obtained using 5-fold SVM training with a polynomial kernel 
	 * and varying the d parameter, where the polynomial function : (s a*b+c)^d
	 *
	 */
	static void computeSER4polynomialKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			for (int n = 2; n<=18; n++){
				float fiveFoldCV_sumSER = 0;
				int k;
				for (k = 1; k<6;k++){
					
					setXmlFileName(xmlDirName + "nbest_test_fold" + k + ".xml");
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t1_d" + n + ".txt";
					java.io.File f = new java.io.File(svmPredFileName);
					if (f.exists()){
						float[] ser = getSER4Predicted(getXmlFileName(), svmPredFileName);
			
						float averageSER = 0;
						for (int i = 0; i< ser.length; i++){
							averageSER += ser[i]; 
						}
						averageSER = averageSER / (float) ser.length;
						fiveFoldCV_sumSER += averageSER;
					}
					else
						k = 1000;
				} // end for
				if (k>=1000){
					errOutBR.write("For parameter d in polynomial kernel  = " + n + " the reranking predictions do not exist! ");
					errOutBR.write("\n");		
				}
				else{
					errOutBR.write("For parameter d in polynomial kernel  = " + n + " , the average SER = " + 100 * (fiveFoldCV_sumSER/5) + "%");
					errOutBR.write("\n");
				}
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
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
	public static float[] getSER4LinearReranking3Feat(String xmlFileName) throws Exception{		
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

				WordErrorRate wer = new WordErrorRate(refDM, dm4rec, wordDeliminator);
				serArray[i] = wer.computeNumerator() ;
			}
			
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * 1) Performs re-ranking using a linear function, 
	 * which takes into account only three features:
	 * newRank = 1 * oldRank + 10 * underconstrained_query + 10 * incosistent_tense;
	 * <br>
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the semantic error rate (WER) as follows:
	 * -- a recognition is considered as semantically correct if the dialogue move 
	 * representation is perfectly equal to the one that would have been produced
	 * from a perfect recognition result.
	 *   
	 * @param xmlFileName - input XML file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public static boolean[] getSemPrecision4LinearReranking(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			boolean[] serArray = new boolean[nbestList.size()];
			int noUtt = 0;			
			int minNewRankID = 1;
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE SEMANTIC ERROR RATE (ser), 
				// get the dialogue_move feature value for the correct transcription 
				// dialogue_move
				Element dmElem = nbestElem.getChild("dialogue_move");
				String dmValue = "";
				if ( dmElem != null)
					dmValue = dmElem.getValue();
					
				// In the xml tree: find hyp_transcription, 
				// i.e. the transcription corresponding to the 1-rank predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");
									
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
				String dmValue4rec = "";
				if (dm4recElem != null) 
					dmValue4rec =  dm4recElem.getValue();

				boolean sem_prec_numerator = dmValue.equalsIgnoreCase(dmValue4rec);
				serArray[i] = sem_prec_numerator ;
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
	 * (The effect is to pick the first (i.e. smallest in rank) hypothesis 
	 * which doesn't fail on one of underconstrained_query or incosnsistent_tense.)    
	 *
	 */
	public static void computeSemPrecision4LinearRerankingBaseline(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				boolean[] ser = getSemPrecision4LinearReranking(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< ser.length; i++){
					if (ser[i])
						averageSER += 1; 
				}
				averageSER = averageSER / (float) ser.length;
				fiveFoldCV_sumWER += averageSER;
				errOutBR.write("For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
			}
				
				//System.out.println(" After linear re-ranking: average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("After linear re-ranking: average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * Computes semantic error rate (SER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using the following linear function:
	 * rank' = rank + 10 * underconstrained_query + 10 * incosnsistent_tense.
	 * The effect is to pick the first (i.e. smallest in rank) hypothesis 
	 * which doesn't fail on one of underconstrained_query or incosnsistent_tense.    
	 *@deprecated
	 */
	public static void computeSER4LinearRerankingBaseline3Feat(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] wer = getSER4LinearReranking(xmlFileName);
			
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
	 * 1) No re-ranking: just take 1-best proposed by the recogniser
	 * 2) Takes the hypothesis which is smallest in (new) rank as the best one.
	 * <br>
	 * 3) Computes the semantic error rate (WER) as follows:
	 * -- a recognition is considered as semantically correct if the dialogue move 
	 * representation is perfectly equeal to the one that would have been produced
	 * from a perfect recognition result.
	 *   
	 *   @deprecated 
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public static boolean[] getSemPrecisionNoReranking(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			boolean[] serArray = new boolean[nbestList.size()];
			int noUtt = 0;			
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE SEMANTIC ERROR RATE (ser), 
				// get the dialogue_move feature value for the correct transcription: 
				// dialogue_move (dm):
				Element dmElem = nbestElem.getChild("dialogue_move");
				String dmValue = "";
				if ( dmElem != null)
					dmValue = dmElem.getValue();
					
				// In the xml tree: take 1-rank predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");								
				Element recElem = (Element) recList.get(1);// get 1-best
				Element dm4recElem = recElem.getChild("dialogue_move");
				String dmValue4rec = "";
				if (dm4recElem != null) 
					dmValue4rec =  dm4recElem.getValue();

				boolean sem_prec_numerator = dmValue.equalsIgnoreCase(dmValue4rec);
				serArray[i] = sem_prec_numerator ;
			}
			
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Computes semantic error rate (SER) for the baseline algorithm,
	 * which takes the 1-best hypothesis
	 * @deprecated    
	 */
	public static void computeSemPrecision4NoRerankingBaseline(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				boolean[] ser = getSemPrecisionNoReranking(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< ser.length; i++){
					if (ser[i])
						averageSER += 1; 
				}
				averageSER = averageSER / (float) ser.length;
				fiveFoldCV_sumWER += averageSER;
				errOutBR.write("For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
				errOutBR.write("\n");
				System.out.println("For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
			}
				
				System.out.println(" 1-best average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("1-best average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * 1) No re-ranking: just takes randomly one of the hypothesis proposed by the recogniser in the n-best list.
	 * <br>
	 * 3) Computes the semantic error rate (WER) as follows:
	 * -- a recognition is considered as semantically correct if the dialogue move 
	 * representation is perfectly equeal to the one that would have been produced
	 * from a perfect recognition result.
	 *  @deprecated
	 * @param xmlFileName - input xml file containing both the n-best hypothesis 
	 * and the reference transcription. 
	 * @return
	 * @throws Exception
	 */
	public static boolean[] getSemPrecisionRandomReranking(String xmlFileName) throws Exception{		
		// read the xml file in order to get the utterance transcripts
		try{
			Document d = new org.jdom.input.SAXBuilder().build(new File(xmlFileName)); // PARSE THE XML FILE 
			java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
			boolean[] serArray = new boolean[nbestList.size()];
			int noUtt = 0;			
			java.util.Random rand = new java.util.Random();
			
			for (int i = 0; i< nbestList.size(); i++){
				Element nbestElem = (Element) nbestList.get(i);
				noUtt++; 
			
				// In order to COMPUTE SEMANTIC ERROR RATE (ser), 
				// get the dialogue_move feature value for the correct transcription: 
				// dialogue_move (dm):
				Element dmElem = nbestElem.getChild("dialogue_move");
				String dmValue = "";
				if ( dmElem != null)
					dmValue = dmElem.getValue();
					
				// In the xml tree: take 1-rank predicted hypothesis 		
				java.util.List recList = nbestElem.getChildren("recognition");
				int rankIdx = rand.nextInt(recList.size()-1) + 1 ;
				if (rankIdx  == 0)
					rankIdx++;
				Element recElem = (Element) recList.get(rankIdx);// get rankIdx hyp, where rankIdx is randomly chosen
				Element dm4recElem = recElem.getChild("dialogue_move");
				String dmValue4rec = "";
				if (dm4recElem != null) 
					dmValue4rec =  dm4recElem.getValue();

				boolean sem_prec_numerator = dmValue.equalsIgnoreCase(dmValue4rec);
				serArray[i] = sem_prec_numerator ;
			}
			
			return serArray;
		}
		catch (IOException eIO) {eIO.printStackTrace();}
		catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
		return null;
	}

	/**
	 * Computes semantic error rate (SER) for the baseline algorithm,
	 * which takes randomly one of the hypothesis in the n-best list.    
	 *@deprecated
	 */
	public static void computeSemPrecision4RandomRerankingBaseline(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumWER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				boolean[] ser = getSemPrecisionRandomReranking(xmlFileName);
			
				float averageSER = 0;
				for (int i = 0; i< ser.length; i++){
					if (ser[i])
						averageSER += 1; 
				}
				averageSER = averageSER / (float) ser.length;
				fiveFoldCV_sumWER += averageSER;
				errOutBR.write("Random reranking, For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
				errOutBR.write("\n");
				System.out.println("Random reranking, For fold no  = " + k + " , the average semantic precision = " + averageSER*100 + "%");
			}
				
				System.out.println("Random reranking: average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.write("Random reranking: average SER = " + 100 * (fiveFoldCV_sumWER/5) + "%");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

}
