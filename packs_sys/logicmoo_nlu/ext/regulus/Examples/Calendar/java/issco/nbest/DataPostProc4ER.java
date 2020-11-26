package issco.nbest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

/**
 * Abstract class implementing general methods useful for data postprocessing 
 * in order to compute Error Rate (either Word error Rate or Semantic Error Rate). 
 * @author GEORGESCUL Maria, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 *
 */
public abstract class DataPostProc4ER {
	private String xmlFileName = "nbest_train.xml";
	protected static final String wordDeliminator = ",";

	abstract float[] getER4Predicted(String xmlFileName, String svmPredFileName) throws Exception;
	
	abstract float[] getER4RandomHyp(String xmlFileName) throws Exception;
	abstract float[] getER4Rank1Hyp(String xmlFileName) throws Exception;
	abstract float[] getBestERCanBeAchieved(String xmlFileName) throws Exception;	

	abstract float[] getER4LinearReranking(String xmlFileName) throws Exception;
	abstract float[] getER4LinearReranking3Feat(String xmlFileName) throws Exception; // deprecated		

	
	// naive baselines algorithms
	// void computeER4LinearReranking3Feat(String errOutFN1, String xmlDirName); // deprecated
	
	/**
	 * @param xmlFileName the xmlFileName to set
	 */
	public void setXmlFileName(String xmlFileName) {
		this.xmlFileName = xmlFileName;
	}


	/**
	 * @return the xmlFileName
	 */
	public String getXmlFileName() {
		return xmlFileName;
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
	 * Computes the error rate (ER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using the following linear function:
	 * rank' = ...
	 *
	 */
	public void computeER4LinearReranking(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] er = this.getER4LinearReranking(xmlFileName);
			
				float averageER = 0;
				for (int i = 0; i< er.length; i++){
					averageER += er[i]; 
				}
				averageER = averageER / (float) er.length;
				fiveFoldCV_sumER += averageER;
				System.out.println("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				//System.out.println("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				System.out.println("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * Computes error rate (ER) for a baseline algorithm,
	 * which changes the rankings of the 6-best hypothesis by using 
	 * only 3 features and the following linear function:
	 *  <br/>
	 *  newRank = 1 * oldRank + 10 * underconstrained_query + 10 * incosistent_tense;
	 * <br>
	 *@deprecated Use computeER4LinearReranking instead. 
	 */
	public void computeER4LinearReranking3Feat(String errOutFN1, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN1);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] er = this.getER4LinearReranking3Feat(xmlFileName);
			
				float averageER = 0;
				for (int i = 0; i< er.length; i++){
					averageER += er[i]; 
				}
				averageER = averageER / (float) er.length;
				fiveFoldCV_sumER += averageER;
				System.out.println("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("\n");
				//System.out.println("Linear reranking, for fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				//System.out.println("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				System.out.println("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Linear reranking, Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n"); errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}
	
	/**
	 * Method for computing the error rate of the SVM predictions.
	 * The predictions were obtained using 5-fold SVM training with RBF kernel and varying the gamma parameter.
	 * The radial basis function: exp(-gamma ||a-b||^2).
	 *
	 */
	public void computeER4rbfKernel(String errOutFN, String xmlDirName, String svmPredDirName, int minGamma, int maxGamma, int stepGamma){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			// for %%n in (-256 -128 -64 -32 -16 -8 -4 -2 -1 1 2 4 8 16 32 64 128 256 ) do svm_classify nbest_adhoc2/nbest_test_fold%%j.txt nbest_adhoc2/fold%%i_model2_t2_g%%n.txt nbest_adhoc2/predictions_%%i_%%j_t2_g%%n.txt > nbest_adhoc2/out_classify_%%i_%%j_t2_g%%n.txt
	// for (int n = -256; n<257;){
//			for (int n = 300; n<1600; n+=100){
			for (int n = minGamma; n < maxGamma + 1; ){
				float fiveFoldCV_sumER = 0;
				int k;
				for (k = 1; k<6;k++){
					String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t2_g" + n + ".txt";
					// check whether this prediction file exists
					File f = new File(svmPredFileName);
					if (f.exists()){
						float[] er = getER4Predicted(xmlFileName, svmPredFileName);
						if (er == null){}
						else{
							float averageER = 0;
							for (int i = 0; i< er.length; i++){
								averageER += er[i]; 
							}	
							averageER = averageER / (float) er.length;
							fiveFoldCV_sumER += averageER;
						System.out.println("For fold no  = " + k + "; gamma = " + n + " , the ER = " + 100*averageER + "%");
						errOutBR.write("For gamma = " + n + " and fold no  = " + k + " , the ER = " + 100*averageER + "%");
						errOutBR.write("\n");
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
					System.out.println("For gamma = " + n + " , the average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
					errOutBR.write("For gamma = " + n + " , the average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
					errOutBR.write("\n");
				}
				if (stepGamma == 2){
					if (n == -1)
						n = 1;
					else{
						if (n < 0)
							n /= 2;
						if (n > 0)
							n *=2;
					}
				}// end if stepGamma is power of 2
				else
					n+= stepGamma;
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
	}
		
	/**
 	 * Method for computing the  error rate of the SVM re-ranking predictions 
 	 * when the linear kernel has been used.
 	 * The predictions were obtained using 5-fold SVM training with linear kernel.
	 *
	 */
	public void computeER4linearKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){				
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				String svmPredFileName = svmPredDirName + "predictions_" + k + "_t0.txt"; 

					float[] er = getER4Predicted(xmlFileName, svmPredFileName);
			
					float averageER = 0;
					for (int i = 0; i< er.length; i++){
						averageER += er[i]; 
					}
					averageER = averageER / (float) er.length;
					fiveFoldCV_sumER += averageER;
					errOutBR.write("For fold no  = " + k + " , the average ER = " + averageER *100 + "%");
					errOutBR.write("\n");
					System.out.println("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				System.out.println(" Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};
		
	}

	/**
	 * Computes error rate (ER) for a baseline naive algorithm,
	 * which takes randomly one of the 6-best hypothesis as the best one.  
	 *
	 */
	public void computeER4RandomBaseline(String errOutFN, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] er = getER4RandomHyp(xmlFileName);
			
				float averageER = 0;
				for (int i = 0; i< er.length; i++){
					averageER += er[i]; 
				}
				averageER = averageER / (float) er.length;
				fiveFoldCV_sumER += averageER;
				errOutBR.write("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("\n");
				System.out.println("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				System.out.println(" Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * Computes error rate (ER) which is the best that can be achieved by starting with n-best hypothesis. 
	 *@param String errOutFN, String xmlDirName
	 */
	public void computeBestERCanBeAchieved(String errOutFN, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] er = getBestERCanBeAchieved(xmlFileName);
			
				float averageER = 0;
				for (int i = 0; i< er.length; i++){
					averageER += er[i]; 
				}
				averageER = averageER / (float) er.length;
				fiveFoldCV_sumER += averageER;
				errOutBR.write("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("\n");
				System.out.println("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				System.out.println(" Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * Computes error rate (ER) for a baseline naive algorithm,
	 * which takes randomly one of the 6-best hypothesis as the best one.  
	 *
	 */
	public void computeER4Rank1Baseline(String errOutFN, String xmlDirName){
		try{	
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			float fiveFoldCV_sumER = 0;
			for (int k = 1; k<6;k++){
				String xmlFileName = xmlDirName + "nbest_test_fold" + k + ".xml";
				float[] er = getER4Rank1Hyp(xmlFileName);
			
				float averageER = 0;
				for (int i = 0; i< er.length; i++){
					averageER += er[i]; 
				}
				averageER = averageER / (float) er.length;
				fiveFoldCV_sumER += averageER;
				errOutBR.write("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
				errOutBR.write("\n");
				System.out.println("For fold no  = " + k + " , the average ER = " + averageER*100 + "%");
			}
				
				System.out.println(" Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("Average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				errOutBR.write("\n");
				errOutBR.flush();
				errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		};	
	}

	/**
	 * Method for computing the error rate of the SVM predictions when the polynomial kernel has been used.
	 * The predictions were obtained using 5-fold SVM training with a polynomial kernel 
	 * and varying the d parameter, where the polynomial function : (s a*b+c)^d
	 *
	 */
	public void computeER4polynomialKernel(String errOutFN, String xmlDirName, String svmPredDirName){
		try{
			BufferedWriter errOutBR = getBufferedWriter(errOutFN);
			for (int n = 2; n<=18; n++){
				float fiveFoldCV_sumER = 0;
				int k;
				for (k = 1; k<6;k++){
					
					setXmlFileName(xmlDirName + "nbest_test_fold" + k + ".xml");
					String svmPredFileName = svmPredDirName + "predictions_" + k + "_t1_d" + n + ".txt";
					java.io.File f = new java.io.File(svmPredFileName);
					if (f.exists()){
						float[] er = getER4Predicted(getXmlFileName(), svmPredFileName);
			
						float averageER = 0;
						for (int i = 0; i< er.length; i++){
							averageER += er[i]; 
						}
						averageER = averageER / (float) er.length;
						fiveFoldCV_sumER += averageER;
					}
					else
						k = 1000;
				} // end for
				if (k>=1000){
					errOutBR.write("For parameter d in polynomial kernel  = " + n + " the reranking predictions do not exist! ");
					errOutBR.write("\n");
					System.out.println("For parameter d in polynomial kernel  = " + n + " the reranking predictions do not exist! ");

				}
				else{
					errOutBR.write("For parameter d in polynomial kernel  = " + n + " , the average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
					errOutBR.write("\n");
					System.out.println("For parameter d in polynomial kernel  = " + n + " , the average ER = " + 100 * (fiveFoldCV_sumER/5) + "%");
				}
			}
			
			errOutBR.flush();
			errOutBR.close();
		}
		catch(Exception ex){
			ex.printStackTrace();
		}
	}

	

}