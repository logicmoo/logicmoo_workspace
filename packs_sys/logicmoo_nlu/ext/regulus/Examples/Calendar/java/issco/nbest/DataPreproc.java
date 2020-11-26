package issco.nbest;

import issco.eval.*;

import java.io.*;

import org.jdom.Element;
import org.jdom.Document;
import org.jdom.output.XMLOutputter;
import org.jdom.*;

/** Abstract class containing methods useful for data pre-processing 
 * required before feature selection or machine learning based reranking.
 * 
 *@see DataPreprocNfold
 *@author GEORGESCUL Maria, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA  
*/
abstract public class DataPreproc {
	
	private static int noHypotheses = 7; 
	private static final String wordDeliminator = ",";
	
	// 
	 protected java.util.HashSet responseTypeFeatSet = new java.util.HashSet();
	 protected java.util.HashSet dialogueMoveFeatSet = new java.util.HashSet();
	 
	// Start defining feature attributes
	protected Feature rank = new Feature(100, true,"rank", -1); 
	protected Feature lenRecWords = new Feature(200, false, "", 0); 
	protected Feature conf = new Feature(300, false,"confidence", 0);  
	protected Feature noWordsResponse = new Feature(400, false,"", 0); 
	protected Feature availableResponse= new Feature(500, false,"", 0); 
	protected Feature available_lf = new Feature(600, false,"lf_context_available", 0); 
	protected Feature available_referent =new Feature(700, false,"referent_available", 0); 
	protected Feature resolution = new Feature(800, false,"resolution", 0); 
	protected Feature elliptical = new Feature(900, false,"elliptical_utterance", 0); 
	protected Feature u_query = new Feature (1000, true,"underconstrained_query", -10); 
	protected Feature i_tense = new Feature (1100, false,"inconsistent_tense", 0); 
	protected Feature responseType = new Feature (1200, false,"response_type", 0); 
	protected Feature dialogueMove = new Feature (1300, false,"dialogue_move", 0);
	protected Feature definiteMeetingReferent = new Feature (1500, false,"definite_meeting_and_meeting_referent", 0);
	protected Feature noDialogueMove = new Feature (1600, true,"no_dialogue_move", -50);
	protected Feature non_indefinite_existential = new Feature (1700, true,xmlTags.non_indefinite_existential, -10);
	protected Feature non_show_imperative = new Feature (1800, true,"non_show_imperative", -50);	
	protected Feature definite_meeting = new Feature (1900, false, "definite_meeting", 0);
	protected Feature indefinite_meeting = new Feature (2000, false, "indefinite_meeting", 0);
	protected Feature indefinite_meeting_and_meeting_referent = new Feature (2100, true, "indefinite_meeting_and_meeting_referent", -2 );
	protected Feature meeting_referent_available = new Feature (2200, false, "meeting_referent_available", 0);
	// end features
	/* Features used in first experiments: 
	rank (100, true,"rank", -1), 
	lenRecWords (200, false, "", 0), 
	conf (300, true,"confidence", 0),  
	noWordsResponse (400, false,"", 0), 
	availableResponse (500, true,"", 0), 
	available_lf (600, true,"lf_context_available", 0), 
	available_referent (700, true,"referent_available", 0), 
	resolution (800, true,"resolution", 0), 
	elliptical (900, true,"elliptical_utterance", 0), 
	u_query (1000, true,"underconstrained_query", -10), 
	i_tense (1100, true,"inconsistent_tense", 0), 
	responseType (1200, true,"response_type", 0), 
	dialogueMove (1300, true,"dialogue_move", 0),
	definiteMeetingReferent (1500, true,"definite_meeting_and_meeting_referent", 0),
			// "definite_meeting_and_meeting_referent", 0),
	noDialogueMove(1600, true,"no_dialogue_move", -50),
	non_indefinite_existential(1700, true,xmlTags.non_indefinite_existential, -10),
	non_show_imperative(1800, true,"non_show_imperative", -50),
	
	definite_meeting(1900, true, "definite_meeting", 0),
	indefinite_meeting(2000, true, "indefinite_meeting", 0),
	indefinite_meeting_and_meeting_referent(2100, true, "indefinite_meeting_and_meeting_referent", -2 ),
	meeting_referent_available(2200, true, "meeting_referent_available", 0);
	*/
	
	
	private final static class xmlTags{
	  static String nbest = "nbest";
	  static String nbest_data = "nbest_data";
	  static String wavfile = "wavfile";
	  static String correct_words ="correct_words";

	  static String recognition = "recognition";
	  static String recognised_words = "recognised_words";
	  static String confidence = "confidence";
		static String rank = "rank";
		static String response = "response";
		static String response_type = "response_type";
		static String lf_context_available = "lf_context_available";
		static String referent_list = "referent_list";
		static String referent = "referent";
		static String dialogue_move = "dialogue_move";
		static String resolution = "resolution";
		static String elliptical_utterance = "elliptical_utterance";
		static String underconstrained_query ="underconstrained_query";
		static String inconsistent_tense = "inconsistent_tense";
		static String no_dialogue_move = "no_dialogue_move";
		static String non_indefinite_existential = "non_indefinite_existential";
		static String non_show_imperative ="non_show_imperative";
		static String semantically_correct = "semantically_correct";
		static String definite_meeting = "definite_meeting";
		static String indefinite_meeting ="indefinite_meeting";
		static String indefinite_meeting_and_meeting_referent = "indefinite_meeting_and_meeting_referent";
		static String meeting_referent_available = "meeting_referent_available";
	}
		
		/**
		 * Reads the data which is in XML format and 
		 * transforms categorical features into numerical-valued features. 
		 * That is, any categorical feature having V different values is converted into V separate binary-valued features. 
		 * Then writes the (FEATURE, VALUES) PAIRS INTO A FILE HAVING THE FORMAT REQUIRED BY SVMlight (ranking module).
		 * @param String inXMLFileName - name of the input file containing categorical features 
		 * @param String outFileName - name of the output file containing numerical-valued features in the format required by the classifier.
		 * @param String targetRankValue - parameter taking two values ("WER" or "SER") specifying whether
		 * Word Error Rate (WER) or Semantic error Rate (SER) should be used as target ranking value   
		 */
			void transformCategFeatIntoNumFeat(String inFileName, String outFileName, String targetRankValue, boolean ranking){
				try{
					File f = new File(outFileName);
					FileOutputStream outS = new FileOutputStream(f);
					PrintStream outPStream = new PrintStream(outS);
					
					try{
						Document d = new org.jdom.input.SAXBuilder().build(new File(inFileName)); // PARSE THE XML FILE BY USING THE SAX PARSER 
						java.util.List nbestList = d.getRootElement().getChildren(xmlTags.nbest_data);
						int noUtt = 0;
						java.util.ArrayList<String> arrayResTypeVal = new java.util.ArrayList<String>(); 
						java.util.ArrayList<String> arrayDialMoveVal = new java.util.ArrayList<String>();
						for (int i = 0; i< nbestList.size(); i++){
							Element nbestElem = (Element) nbestList.get(i);
							noUtt++; // noUtt will represent the "qid" in the SVM input file
							outPStream.println("# utterance " + noUtt);
							
							// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
							Element transcriptionElem = nbestElem.getChild("correct_words");
							String transcription =  transcriptionElem.getValue();
										
							java.util.List recList = nbestElem.getChildren("recognition");
												
							Element ref_recElem = (Element) recList.get(0);
							Element ref_dm_Elem = ref_recElem.getChild("dialogue_move");
							String ref_dialogue_move = ref_dm_Elem.getValue();
														
							for (int j = 1; j< recList.size(); j++){						
								Element recElem = (Element) recList.get(j);					

								if (targetRankValue.equalsIgnoreCase("WER")){
									// 	compute WORD ERROR RATE (WER) and use WER as the target ranking value 
									printWER(recElem, outPStream, transcription);
								}
								if (targetRankValue.equalsIgnoreCase("SER")){
									// 	compute Semantic Error Rate (SER) and use it as the target ranking value
									// the reference dialogue move is in recList.get(0).
										Element dm_Elem = recElem.getChild("dialogue_move");
										String hyp_dm =  dm_Elem.getValue();
										// printSER(outPStream, ref_dialogue_move, hyp_dm);
										// Element recElem, PrintStream outPStream, String featName, String trueVal
										printSemanticErrorRate(recElem, outPStream, "semantically_correct", "good" );
								}

								// if ranking is true then prepare data for ranking, i.e. add "qid:uttID"
								// otherwise prepare data for classification
								if (ranking)
									outPStream.print("qid:" + noUtt + " ");

						
								// "definite_meeting_and_meeting_referent"
								if (this.rank.isUsed()){
									printRankFeature(recElem, outPStream, rank.getID());							
		
		//							printRankFeature_3vals(recElem, outPStream, rankID);																			
			//						printInconsistentUnderconstrained(recElem, outPStream, u_query_ID, i_tense_ID);
		
								}
								if (this.lenRecWords.isUsed())
									printLenRecWords(recElem, outPStream, this.lenRecWords.getID());
								
								if (this.conf.isUsed())
									printConfidence(recElem, outPStream, this.conf.getID());
								if (this.noWordsResponse.isUsed())
									printResponseAvailAndLen(recElem, outPStream, this.noWordsResponse.getID(), this.availableResponse.getID());
								
								if (this.available_lf.isUsed())
									printLF_Referent(recElem, outPStream, this.available_lf.getID(),this.available_referent.getID());
								if (this.resolution.isUsed()){
									printResolutionTrivial(recElem, outPStream, this.resolution.getID());
								}
									if (this.elliptical.isUsed())
									printElliptical(recElem, outPStream, this.elliptical.getID());
								if (this.u_query.isUsed())
									printUnderconstrainedQuery(recElem, outPStream, this.u_query.getID());
								
								if (this.i_tense.isUsed())
									printInconsistentTense(recElem, outPStream, this.i_tense.getID());
								
								// TRANSFORM the categorical feature response_type into binary-valued features
								// eg: <response_type>say_yes</response_type>
								// eg: <response_type>say_list_of_n_referents(3)</response_type>
								if (this.responseType.isUsed())
									arrayResTypeVal = printResponseType(recElem, outPStream, this.responseType.getID(), arrayResTypeVal);
											
								// 
								// TRANSFORM the categorical feature dialogue_move into binary-valued features
								// eg: <dialogue_move>referent_from_context,tense_information,utterance_type</dialogue_move>			
								if (this.dialogueMove.isUsed())
									arrayDialMoveVal = printDialogueMove(recElem, outPStream, this.dialogueMove.getID(), arrayDialMoveVal);
								
								// System.out.println(" definiteMeetingReferent");								
								if (this.definiteMeetingReferent.isUsed()){
									printFeatureBooleanValue(recElem, outPStream, this.definiteMeetingReferent.getID(), this.definiteMeetingReferent.name );
								}
								if (this.noDialogueMove.isUsed())
									{
										printFeatureBooleanValue(recElem, outPStream, 
											this.noDialogueMove.getID(),
											this.noDialogueMove.name );
								}
								if (this.non_indefinite_existential.isUsed()){
									printFeatureBooleanValue(recElem, outPStream, 
											this.non_indefinite_existential.getID(),
											this.non_indefinite_existential.name );
								};
								if (this.non_show_imperative.isUsed()){
									printFeatureBooleanValue(recElem, outPStream, 
											this.non_show_imperative.getID(),
											this.non_show_imperative.name );
								}
								if (this.indefinite_meeting_and_meeting_referent.isUsed())
									printFeatureBooleanValue(recElem, outPStream, 
											this.indefinite_meeting_and_meeting_referent.getID(),
											this.indefinite_meeting_and_meeting_referent.name );
								
								outPStream.println();						
							}
						}// end for 
					}
				catch (IOException eIO) {eIO.printStackTrace();}
				catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
				
				}
				catch(FileNotFoundException ex1){
					ex1.printStackTrace();
				}  
			}


			/**
			 * @deprecated Use transformCategFeatIntoNumFeat instead and set the Feature attributes
			 * Choose only a subset of three features ((rank, underconstrained_query and incosistent_tense) 
			 * and write them into the format required by SVMlight. 
			 * @param inXMLFileName
			 * @param svmFileName
			 */
			void takeSubsetOf3Feat(String inXMLFileName, String svmFileName){
				try{
						File f = new File(svmFileName);
						FileOutputStream outS = new FileOutputStream(f);
						PrintStream outPStream = new PrintStream(outS);

						final int rankID = this.rank.getID();
						final int u_query_ID = this.u_query.getID();
						final int i_tense_ID = this.i_tense.getID();
										
						try{
							Document d = new org.jdom.input.SAXBuilder().build(new File(inXMLFileName)); // PARSE THE XML FILE BY USING THE SAX PARSER 
							java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
							int noUtt = 0;
							for (int i = 0; i< nbestList.size(); i++){
								Element nbestElem = (Element) nbestList.get(i);
								noUtt++; // noUtt will represent the "qid" in the SVM input file
								outPStream.println("# utterance " + noUtt);
								
								// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
								Element transcriptionElem = nbestElem.getChild("correct_words");
								String transcription =  transcriptionElem.getValue();
											
								java.util.List recList = nbestElem.getChildren("recognition");
								for (int j = 0; j< recList.size(); j++){						
									Element recElem = (Element) recList.get(j);					

									// TODO: COMPUTE CONCEPT ERROR RATE AS A RANKING. That is, DO EXPERIMENTS IN PARALLEL WITH "SEMANTIC" ERROR RATE (INSTEAD OF WER) AS THE RANKING SCOPE					
									printWER(recElem, outPStream, transcription); // 	COMPUTE WORD ERROR RATE (WER) and use WER AS A RANKING
									outPStream.print("qid:" + noUtt + " ");
									
									printRankFeature(recElem, outPStream, rankID);																			
									printInconsistentTense(recElem, outPStream, i_tense_ID);
									printUnderconstrainedQuery(recElem, outPStream, u_query_ID);
																											
									outPStream.println();						
								}
							}// end for 
						}
					catch (IOException eIO) {eIO.printStackTrace();}
					catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
					
					}
					catch(FileNotFoundException ex1){
						ex1.printStackTrace();
					}			  
				}
			
			/**
			 * Choose only a subset of three features ((rank, underconstrained_query and incosistent_tense) 
			 * and write them into the format required by SVMlight.
			 * @deprecated Use transformCategFeatIntoNumFeat instead and set the Feature attributes 
			 * @param inXMLFileName
			 * @param svmFileName
			 */
			void takeSubsetOf3Feat_rank3vals(String inXMLFileName, String svmFileName){
					try{
						File f = new File(svmFileName);
						FileOutputStream outS = new FileOutputStream(f);
						PrintStream outPStream = new PrintStream(outS);

						final int rankID = this.rank.getID();
						final int u_query_ID = this.u_query.getID();
						final int i_tense_ID = this.i_tense.getID();
										
						try{
							Document d = new org.jdom.input.SAXBuilder().build(new File(inXMLFileName)); // PARSE THE XML FILE BY USING THE SAX PARSER 
							java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
							int noUtt = 0;
							for (int i = 0; i< nbestList.size(); i++){
								Element nbestElem = (Element) nbestList.get(i);
								noUtt++; // noUtt will represent the "qid" in the SVM input file
								outPStream.println("# utterance " + noUtt);
								
								// In order to COMPUTE WORD ERROR RATE (wer), get the correct transcription
								Element transcriptionElem = nbestElem.getChild("correct_words");
								String transcription =  transcriptionElem.getValue();
											
								java.util.List recList = nbestElem.getChildren("recognition");
								for (int j = 0; j< recList.size(); j++){						
									Element recElem = (Element) recList.get(j);					

									// TODO: COMPUTE CONCEPT ERROR RATE AS A RANKING. That is, DO EXPERIMENTS IN PARALLEL WITH "SEMANTIC" ERROR RATE (INSTEAD OF WER) AS THE RANKING SCOPE					
									printWER(recElem, outPStream, transcription); // 	COMPUTE WORD ERROR RATE (WER) and use WER AS A RANKING
									outPStream.print("qid:" + noUtt + " ");
									
									printRankFeature_3vals(recElem, outPStream, rankID);		
									printInconsistentTense(recElem, outPStream, i_tense_ID);
									printUnderconstrainedQuery(recElem, outPStream, u_query_ID);
																											
									outPStream.println();						
								}
							}// end for 
						}
					catch (IOException eIO) {eIO.printStackTrace();}
					catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
					
					}
					catch(FileNotFoundException ex1){
						ex1.printStackTrace();
					}			  
				}

			
			/**
			 * Computes Word Error Rate for two utterances and prints the result into the specified output stream.  
			 * @param recElem
			 * @param outPStream
			 * @param transcription
			 */
			private static void printWER(Element recElem, PrintStream outPStream, String transcription){
				Element recWordsElem = recElem.getChild("recognised_words");
				String recWords =  recWordsElem.getValue();
				float wer = computeWordErrorRate(transcription, recWords);
				outPStream.print(wer + " ");
			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param responseTypeID
			 * @param arrayTypeValues
			 * @return
			 */
			private java.util.ArrayList<String> printResponseType(Element recElem, PrintStream outPStream, int responseTypeID, java.util.ArrayList<String> arrayTypeValues ){
				// eg: <response_type>say_yes</response_type>
				//     <response_type>say_list_of_n_referents(3)</response_type>
				// TODO: only one feature for: say_list_of_n_referents(3), say_list_of_n_referents(2), say_list_of_n_referents(n), etc
				Element responseTypeElem = recElem.getChild("response_type");
				String responseTypeVal =  responseTypeElem.getValue();
				if (responseTypeVal.contains("say_list_of_n_referents") )
					responseTypeVal = "say_list_of_n_referents";
				int responseTypeIdx = arrayTypeValues.indexOf(responseTypeVal);
				if ( responseTypeIdx == -1 ){
					arrayTypeValues.add(responseTypeVal);
					responseTypeIdx = arrayTypeValues.indexOf(responseTypeVal); 
				}
				int feat_ID = responseTypeID + responseTypeIdx;
				
				if (this.responseTypeFeatSet.contains(feat_ID))
					outPStream.print( feat_ID + ":" + 1 + " ");
				
				return arrayTypeValues;
			}

			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param dialogueMoveID
			 * @param arrayDialogueMove
			 * @return
			 */
			private java.util.ArrayList<String> printDialogueMove(Element recElem, PrintStream outPStream, int dialogueMoveID, java.util.ArrayList<String> arrayDialogueMove ){
				// <dialogue_move>referent_from_context,tense_information,utterance_type</dialogue_move>
				Element dialogueMoveElem = recElem.getChild("dialogue_move");
				if (dialogueMoveElem.getContent().size() > 0){
					String dialogueMoveVal =  dialogueMoveElem.getValue();
					String[] dialogueMoveVal2 = dialogueMoveVal.split(",");
					int[] features = new int[dialogueMoveVal2.length];
					
					for (int i = 0; i< dialogueMoveVal2.length; i++){
						int dialogueMoveIdx = arrayDialogueMove.indexOf(dialogueMoveVal2[i]);
						if ( dialogueMoveIdx == -1 ){
							arrayDialogueMove.add(dialogueMoveVal2[i]);
							dialogueMoveIdx = arrayDialogueMove.indexOf(dialogueMoveVal2[i]); 
						}
						features[i] = dialogueMoveID + dialogueMoveIdx;
					}
					// FEATURES MUST BE IN INCREASING ORDER
					java.util.Arrays.sort(features);
					for (int i = 0; i< features.length; i++){
						if ((i>0) && (features[i] != features[i-1]))
							if (this.dialogueMoveFeatSet.contains(features[i]))
							outPStream.print(  features[i] + ":" + 1 + " ");
					}
				}
				return arrayDialogueMove;
			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param rankID
			 */
			private static void printRankFeature(Element recElem, PrintStream outPStream, int rankID){
				// rank <-- the position of a hypothesis in the n-best list													
				Element rankElem = recElem.getChild("rank");
				Integer rankElemVal =  new Integer(rankElem.getValue());
				int rankValue = rankElemVal.intValue();
				outPStream.print(rankID + ":0." + rankValue + " ");
			}
			
			/**
			 * 
			 */
			private static void printFeatureBooleanValue(Element recElem, PrintStream outPStream, int featID, String featName){
				Element elem = recElem.getChild(featName);
				Integer elemVal =  new Integer(elem.getValue());
				int value = elemVal.intValue();
				if (value == 0)
					outPStream.print(featID + ":0.5" + " ");
				else
					outPStream.print(featID + ":" + value + " ");
			}

			/**
			 * 
			 */
			private static void printSemanticErrorRate(Element recElem, PrintStream outPStream, String featName, String goodValue){
				Element elem = recElem.getChild(featName);
				String elemVal =  elem.getValue();
				if (elemVal.equalsIgnoreCase(goodValue))
					outPStream.print("0 ");
				else
					outPStream.print("1 ");

			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param rankID
			 */
			private static void printRankFeature_3vals(Element recElem, PrintStream outPStream, int rankID){
				// rank <-- the position of a hypothesis in the n-best list													
				Element rankElem = recElem.getChild("rank");
				Integer rankElemVal =  new Integer(rankElem.getValue());
				int rankValue = rankElemVal.intValue();
				if (rankValue >=3)
					rankValue = 3;
				outPStream.print(rankID + ":" + rankValue + " ");
			}
	/**
	 * 
	 * @param recElem
	 * @param outPStream
	 * @param lenRecWordsID
	 */
			private static void printLenRecWords(Element recElem, PrintStream outPStream, int lenRecWordsID){
				// number_recognized_words <-- the length in words of the hypothesized utterance				
				String recWordsStr = recElem.getChild("recognised_words").getValue();				
				int lenRecWords = numberOf(recWordsStr, ",") + 1;
				outPStream.print(lenRecWordsID + ":0." + lenRecWords + " ");
			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param confID
			 */
			private static void printConfidence(Element recElem, PrintStream outPStream, int confID){
				// confidence <-- the confidence assessment given by the recognizer				
				Element confElem = recElem.getChild(xmlTags.confidence);
				String confidence = confElem.getValue();
				if (confidence.equals("")){
					System.out.println("Empty confidence for " + recElem.getChild(xmlTags.recognised_words).getValue());
				}
				else{
					Integer confElemVal =  new Integer(confidence);
				
					int confValue = confElemVal.intValue();
					outPStream.print(confID + ":0." + confValue + " ");
				}
			}

			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param noWordsResponseID
			 * @param availableResponseID
			 */
			private static void printResponseAvailAndLen(Element recElem, PrintStream outPStream, int noWordsResponseID, int availableResponseID){
				// 	response_available and response_lenght
				String responseStr = recElem.getChild(xmlTags.response).getValue();
				// 	response_lenght <-- the number of words inside the response
				int noWordsInResponse = numberOf(responseStr, " ") + 1;						
				outPStream.print(noWordsResponseID + ":0." + noWordsInResponse + " ");
				
				// response_available: indicating whether the system gives a response 
				// 	possible values: “yes”, “no”, 
				// 	check whether the response is "sorry" : 
				// if yes then it means that there was no response
				if (noWordsInResponse == 1){
					if (responseStr.equalsIgnoreCase("sorry")){
						double availableResponseVal = 0.5; // there was NO response from the system
						outPStream.print(availableResponseID + ":" + availableResponseVal + " ");
					}
					else{
						int availableResponseVal = 1; // there was a response from the system
						outPStream.print(availableResponseID + ":" + availableResponseVal + " ");
					}
				}
				else{
					int availableResponseVal = 1; // there was a response from the system
					outPStream.print(availableResponseID + ":" + availableResponseVal + " ");
				}
			}			
				

			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param available_lf_ID
			 * @param available_referent_ID
			 */
			private static void printLF_Referent(Element recElem, PrintStream outPStream, int available_lf_ID, int available_referent_ID ){
				// lf_context_available
				//   --> indicating whether there was an LF context available for ellipsis resolution,
				String lf_context = recElem.getChild(xmlTags.lf_context_available).getValue();
				if (lf_context.equals("yes")){
					double available_lf_val = 1; // there was an available LF 
					// System.out.print(available_lf_ID + ":" + nbestCurrent[available_lf_ID] + " ");
					outPStream.print(available_lf_ID + ":" + available_lf_val + " ");
				}
				else{
					double available_lf_val = 0.5; // there was NO LF available
					// System.out.print(available_lf_ID + ":" + nbestCurrent[available_lf_ID] + " ");
					outPStream.print(available_lf_ID + ":" + available_lf_val + " ");
				}

				// referent_available 
				// --> indicating whether there was a referent of the specified type available
				// <referent_list>
					// <referent>meeting</referent>
					// <referent>attendee</referent>
					// <referent>start_time</referent>
				// </referent_list>
				Element referentElem = recElem.getChild(xmlTags.referent_list);
				// eg: <referent_list>	<referent>meeting</referent> </referent_list>
				if (referentElem == null){
					double available_referent_val = 0.5; // there was NO available referent
					outPStream.print(available_referent_ID + ":" + available_referent_val + " ");
				}
				else{
					if (referentElem.getChildren(xmlTags.referent).size() > 0){
						int available_referent_val = 1; // there was an available referent 
						outPStream.print(available_referent_ID + ":" + available_referent_val + " ");
					}
					else{
						double available_referent_val = 0.5; // there was NO available referent
						outPStream.print(available_referent_ID + ":" + available_referent_val + " ");
					}
				}

			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param resolution_ID
			 */
			private static void printResolutionTrivial(Element recElem, PrintStream outPStream, int resolution_ID){
				// resolution_trivial --> indicating whether the ellipsis resolution was trivial or non_trivial 
				// eg: <resolution>trivial</resolution> 
				String resolution = recElem.getChild(xmlTags.resolution).getValue();
				if (resolution.equals("non_trivial")){
					double resolution_val = 0.3;  
					outPStream.print(resolution_ID + ":" + resolution_val + " ");
				}else{
					if (resolution.equals("trivial")){
						double resolution_val = 0.6;  
						outPStream.print(resolution_ID + ":" + resolution_val + " ");
					}
					else{
						double resolution_val = 0.9; 
						outPStream.print(resolution_ID + ":" + resolution_val + " ");
					}
				}
			}
			
			/**
			 * 
			 * @param recElem
			 * @param outPStream
			 * @param elliptical_ID
			 */
			private static void printElliptical(Element recElem, PrintStream outPStream, int elliptical_ID){
				// elliptical_utterance --> indicating whether the utterance was elliptical or non-elliptical,
				// eg: <elliptical_utterance>yes</elliptical_utterance>
				String elliptical = recElem.getChild(xmlTags.elliptical_utterance).getValue();
				if (elliptical.equals("no")){
					double elliptical_val = 0.3;  
					outPStream.print(elliptical_ID + ":" + elliptical_val + " ");
				}
				else{
					if (elliptical.equals("yes")){
						double elliptical_val = 0.6;  
						outPStream.print(elliptical_ID + ":" + elliptical_val + " ");
					}
					else{
						double elliptical_val = 0.9; 
						outPStream.print(elliptical_ID + ":" + elliptical_val + " ");
					}
				}

			}
			
			private static void printInconsistentTense(Element recElem, PrintStream outPStream, int i_tense_ID){
							
				// inconsistent_tense indicating whether tenses were consistent or not according to heuristic.
				// eg: 	<inconsistent_tense>0</inconsistent_tense>
				String it = recElem.getChild(xmlTags.inconsistent_tense).getValue();
				if (it.equals("")){}
				else{
					Integer i_tense = new Integer(it);			
					int i_tense_val = i_tense.intValue(); 
					if (i_tense_val==0)
						outPStream.print(i_tense_ID + ":" + 0.5 + " "); // print 0.5 instead 0
					else
						outPStream.print(i_tense_ID + ":" + i_tense_val + " ");// print 1
				}
			}

			private static void printUnderconstrainedQuery(Element recElem, PrintStream outPStream, int u_query_ID){
				// underconstrained_query indicating whether the query was underconstrained or not underconstrained according to heuristic,
				// eg: <underconstrained_query>0</underconstrained_query>
				String uq = recElem.getChild(xmlTags.underconstrained_query).getValue();
				if (uq.equals("")){
					System.out.println("The underconstrained_query feature is not available");
				}
				else{
					Integer u_query = new Integer(uq);			
					int u_query_val = u_query.intValue(); 
					if (u_query_val == 0)
						outPStream.print(u_query_ID + ":" + 0.5 + " ");
					else
						outPStream.print(u_query_ID + ":" + u_query_val + " ");
				}
			}

			protected int getNoHyp(){
				return noHypotheses;
			}
			
			/**
			 * Computes how many word substitutions you need in order to obtain the "ref" data from "hyp" data 
			 * @param ref
			 * @param hyp
			 * @return
			 */
			private static float computeWordErrorRate(String ref, String hyp){			
				WordErrorRate wer = new WordErrorRate(ref, hyp, wordDeliminator);
				return (wer.computeNumerator() / wer.computeDenominator() );
				
			}
			
			
			/**
			 * The method returns the number of times the "substr" occurs inside the "str"
			 * @param str
			 * @param substr
			 * @return
			*/ 
			private static int numberOf(String str, String substr){
				String[] str1 = str.split(",");
				return (str1.length -1) ;
			};
			
			
			/**
		 * Reads the data which is in prologue format and puts (key, value) pairs into an xml format
		 * @param String infileName - name of the input file containing features values in prolog format
		 * @param String outXMLFileName - name of the output xml file
		 * @param noHyp - number of nbest recognition variants
		 */
		void transformPrologIntoXML(String inFileName, String outXMLFileName, int noHyp){
				
			Element root = new Element(xmlTags.nbest);
		
			java.io.FileInputStream fis; 
			try{
				fis = new java.io.FileInputStream(inFileName);
			}
			catch(java.io.FileNotFoundException ex){
				fis = null;
			}
			if (fis !=null){	
				try {
					// read the file containing the features/values in prolog format 				 			
				      BufferedReader br
				      	= new BufferedReader(new InputStreamReader(fis));
				    
				      try {
				    	  String nbestCurrent = "";
				    	  boolean first_nbest = true;
				    	  String thisLine;
				    	  
				    	  while ((thisLine = br.readLine()) != null) {  // while not end of file
				    		if (thisLine.contains("%")){}
				    		else{
				    		  if (thisLine.contains("nbest_data")){
				    			  if (first_nbest){
				    				 first_nbest = false;
				    			  }
				    			  else{					    				  
				    			  		String correctWords = ParsePrologData.getCorrectWords(nbestCurrent);
				    			  		correctWords = correctWords.replace("\\'","{");
				    			  		correctWords = correctWords.replace("'", "");
				    			  		correctWords = correctWords.replace("{", "'");
				    			  		if(! correctWords.contains("out-of-coverage")){
					    				  // Parse the content of the previous nbestCurrent and put it into the xml format
					    			  		Element item1 = new Element(xmlTags.nbest_data);				    			  		
					    			  		
					    			  		String wavFile = ParsePrologData.getWavFile(nbestCurrent);				    			  		
											Element wavFileItem = new Element(xmlTags.wavfile);
											wavFileItem.addContent(wavFile);
					    			  		item1.addContent(wavFileItem);
					    			  		
					    			  		Element correctWordsItem = new Element(xmlTags.correct_words);
					    			  		correctWordsItem.addContent(correctWords);
					    			  		item1.addContent(correctWordsItem);				    			  						    			  		
					    			  		
					    			  		item1 = getFeat4OneUtt(item1, noHyp, nbestCurrent, "confidence");
					    			  		// System.out.println("After proc the utterance");
					    			  		
					    			  		root.addContent(item1);
					    			  	}
					    			  		
					    				  // END HERE: Parse the content of the previous nbestCurrent 
					    			  		// and put it into the xml format
					    				  // THEN: simply reset the content of nbestCurrent 
					    				  nbestCurrent = "";			    			  
					    			  }
					    		  }
					    		nbestCurrent += thisLine;
					    		} // end if line does not contain %
					    	  } // end while 
					      }
					      catch (Exception e) {
					       System.out.println("Error: " + e);
					      }
					    } // end try
					    catch (Exception e) {
					      System.out.println("Error: " + e);
					    }
					
					try{
						FileOutputStream fos = new FileOutputStream (outXMLFileName);
					
						XMLOutputter outputter = new XMLOutputter();
						outputter.output(new Document(root), fos);
					}
					catch(java.io.FileNotFoundException ex)
					{
						System.out.println("Problems when creating the xml file.");
						ex.printStackTrace();
					}
					catch(java.io.IOException ex)
					{
						ex.printStackTrace();
					}
				}
				
				}

			

			/**
			 * Gets all features for one utterance and writes them into 
			 * an XML element. This static private method is used 
			 * by transformPrologIntoXML method. 
			 * @param item1
			 * @param noHyp
			 * @param nbestCurrent
			 * @param firstFeatureName
			 * @return XML element containing the entire set of features
			 * @see transformPrologIntoXML method
			 * 
			 */
			private Element getFeat4OneUtt(Element item1, int noHyp, String nbestCurrent, String firstFeatureName){
		  		int i1 = 0; 
		  		boolean stillRecVariants = true;
		  		while ( (i1 < noHyp) && stillRecVariants ){
		  			i1++;				    			  			
		  			Element recognitionItem = new Element(xmlTags.recognition);
		  			
		  			// get into "recCurrent" the substring containing the features for one of the nbest variants  
		  			int idxRecStart = nbestCurrent.indexOf(firstFeatureName);
		  			int idxRecEnd = nbestCurrent.indexOf(firstFeatureName, idxRecStart + 1);
		  			if (idxRecEnd < 1){
		  				idxRecEnd = nbestCurrent.length();
		  				stillRecVariants = false;
		  			}
		  			String recCurrent = nbestCurrent.substring(idxRecStart + 1, idxRecEnd);	    			  						    			  		
		  			nbestCurrent = nbestCurrent.substring(idxRecEnd, nbestCurrent.length());
		  			
		  			String recognisedWords = ParsePrologData.getRecWords(recCurrent);
		  			recognisedWords = recognisedWords.replace("\\'","{");
		  			recognisedWords = recognisedWords.replace("'", "");
		  			recognisedWords = recognisedWords.replace("{", "'");
		  			
		  			Element recWordsItem = new Element(xmlTags.recognised_words);
		  			recWordsItem.addContent(recognisedWords);
		  			recognitionItem.addContent(recWordsItem);
		  			
		  			String confidenceValue = ParsePrologData.getFirstFeature(recCurrent, "confidence");
		  			confidenceValue = confidenceValue.replace("(","");
		  			confidenceValue = confidenceValue.replace(")","");
		  			Element confidenceItem = new Element(xmlTags.confidence);
		  			confidenceItem.addContent(confidenceValue);
		  			recognitionItem.addContent(confidenceItem);

		  			String rankValue = ParsePrologData.getFeatureValue(recCurrent, "rank");
		  			rankValue = rankValue.replace("(","");
		  			rankValue = rankValue.replace(")","");
		  			Element rankItem = new Element(xmlTags.rank);
		  			rankItem.addContent(rankValue);
		  			recognitionItem.addContent(rankItem);

		  			String response = ParsePrologData.getResponse(recCurrent);
		  			response = response.replace("\\'","{");
		  			response = response.replace("'", "");
		  			response = response.replace("{", "'");
		  			Element responseItem = new Element(xmlTags.response);
		  			responseItem.addContent(response);
		  			recognitionItem.addContent(responseItem);	  			
		  					  			
		  			String responseType = ParsePrologData.getResponseType(recCurrent);
		  			Element responseTypeItem = new Element(xmlTags.response_type);
		  			responseTypeItem.addContent(responseType);
		  			recognitionItem.addContent(responseTypeItem);

		  			
		  			String lfContextAvailable = ParsePrologData.getLFContextAvailable(recCurrent);
		  			Element lfContextAvailableItem = new Element(xmlTags.lf_context_available);
		  			lfContextAvailableItem.addContent(lfContextAvailable);
		  			recognitionItem.addContent(lfContextAvailableItem);
		  			
		  			if (this.available_referent.used){
		  				// put this feature in the xml file only if is used later
		  				// there might be more than one feature called "referent_available"
		  				java.util.Vector<String> referentList = ParsePrologData.getReferentAvailable(recCurrent);
		  				if (referentList != null){
		  					Element referentListItem = new Element(xmlTags.referent_list);
	  					
		  					for (int index = 0; index < referentList.size(); index++){
		  						String referent = referentList.get(index);
		  						Element referentItem = new Element(xmlTags.referent);
		  						referentItem.addContent(referent);
		  						referentListItem.addContent(referentItem);				    			  					
		  					}
		  				recognitionItem.addContent(referentListItem);
		  			}
		  			}

		  			String dialogueMove = ParsePrologData.getFeatBetweenSquarePar(recCurrent, "dialogue_move=");
			  		Element dialogueMoveItem = new Element(xmlTags.dialogue_move);
			  		dialogueMoveItem.addContent(dialogueMove);
			  		recognitionItem.addContent(dialogueMoveItem);
			  		
			  		String resolution = ParsePrologData.getFeature(recCurrent, "resolution=");
		  			Element resolutionItem = new Element(xmlTags.resolution);
		  			resolutionItem.addContent(resolution);
		  			recognitionItem.addContent(resolutionItem);
		  			
			  		String ellipticalUtt = ParsePrologData.getFeature(recCurrent, "elliptical_utterance=");
		  			Element ellipticalUttItem = new Element(xmlTags.elliptical_utterance);
		  			ellipticalUttItem.addContent(ellipticalUtt);
		  			recognitionItem.addContent(ellipticalUttItem);
		  			
		  			String underconstrainedQuery = ParsePrologData.getLastFeature(recCurrent, "underconstrained_query=");
		  			Element underconstrainedQueryItem = new Element(xmlTags.underconstrained_query);
		  			underconstrainedQueryItem.addContent(underconstrainedQuery);
		  			recognitionItem.addContent(underconstrainedQueryItem);
		  			
		  			String inconsistent_tense = ParsePrologData.getFeature(recCurrent, "inconsistent_tense=");
		  			// inconsistent_tense = inconsistent_tense.replace("]", "");
		  			Element inconsistent_tenseItem = new Element(xmlTags.inconsistent_tense);
		  			inconsistent_tenseItem.addContent(inconsistent_tense);
		  			recognitionItem.addContent(inconsistent_tenseItem);

		  			String valueAux = ParsePrologData.getFeatureValue(recCurrent, this.definiteMeetingReferent.name);
		  			Element itemAux = new Element(this.definiteMeetingReferent.name);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);

		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "no_dialogue_move");
		  			itemAux = new Element(xmlTags.no_dialogue_move);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  			
		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "non_indefinite_existential");
		  			itemAux = new Element(xmlTags.non_indefinite_existential);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  			
		  			String valueAux1 = ParsePrologData.getFeatureValue(recCurrent, "non_show_imperative");
		  			Element itemAux1 = new Element(xmlTags.non_show_imperative);
		  			itemAux1.addContent(valueAux1);
		  			recognitionItem.addContent(itemAux1);

		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "semantically_correct");
		  			valueAux = valueAux.replace(")", "");
		  			valueAux = valueAux.replace("(", "");
		  			itemAux = new Element(xmlTags.semantically_correct);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  			
		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "definite_meeting");
		  			itemAux = new Element(xmlTags.definite_meeting);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  			
		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "indefinite_meeting");
		  			itemAux = new Element(xmlTags.indefinite_meeting);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  		
		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "indefinite_meeting_and_meeting_referent");
		  			itemAux = new Element(xmlTags.indefinite_meeting_and_meeting_referent);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  		
		  			valueAux = ParsePrologData.getFeatureValue(recCurrent, "meeting_referent_available");
		  			itemAux = new Element(xmlTags.meeting_referent_available);
		  			itemAux.addContent(valueAux);
		  			recognitionItem.addContent(itemAux);
		  		
		  			item1.addContent(recognitionItem);
		  		}
		  		return item1;
			}
			

}
