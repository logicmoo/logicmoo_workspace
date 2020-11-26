package issco.nbest;

/** Abstract class containing methods useful for data preprocessing 
 * required before machine learning re-ranking.
 */
public class DataPreproc4Learning extends DataPreproc{
	
	DataPreproc4Learning(){
		super();
		//
	    Integer elements[] = { 1201, 1202, 1204, 1205, 1206 };
	    responseTypeFeatSet = new java.util.HashSet(java.util.Arrays.asList(elements));
	    
	    Integer[] elements2 = { 1302, 1307 };
	    dialogueMoveFeatSet = new java.util.HashSet(java.util.Arrays.asList(elements2));
	    
		//	Redefine the set of features to be used for machine learning
		rank.setUsed(true); 
		lenRecWords.setUsed(false); 
		conf.setUsed(true);  
		noWordsResponse.setUsed(false); 
		availableResponse.setUsed(false); 
		available_lf.setUsed(true); // false prev 
		available_referent.setUsed(false); 
		resolution.setUsed(false); 
		elliptical.setUsed(true); // false prev 
		u_query.setUsed(true); 
		i_tense.setUsed(false); 
		responseType.setUsed(false); // if true then use certain values only (feature Ids given by responseTypeFeatSet )
		dialogueMove.setUsed(false); // if true then use certain values only (feature Ids given by dialogueMoveFeatSet
		definiteMeetingReferent.setUsed(false);	// "definite_meeting_and_meeting_referent", 0),
		noDialogueMove.setUsed(true);
		non_indefinite_existential.setUsed(true);
		non_show_imperative.setUsed(false); // true before		
		definite_meeting.setUsed(false);
		indefinite_meeting.setUsed(false);
		indefinite_meeting_and_meeting_referent.setUsed(false); // true before
		meeting_referent_available.setUsed(false);

		/*
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
		 */

	}
	/**
	 * 
	 * @param args String[3]
	 */	
	public static void main(String[] args){
		
		// For instance:
		args = new String[3];
		args[0] = "D:/svm_light/calendar_data_WERtrain/nbest_training_data.pl";
		args[1] = "D:/svm_light/calendar_data_WERtrain/nbest_training_data.xml";				
		args[2] = "D:/svm_light/calendar_data_WERtrain/";
		
		String prologFileName = "";
		String xmlFileName ="";
	
		String dirName ="";
		try{
			prologFileName = args[0];
			xmlFileName = args[1];
			dirName = args[2];
		}
		catch(java.lang.ArrayIndexOutOfBoundsException ex){
			System.out.println("Please provide the following 3 args : \n" +
					" 1) the name of the prolog file that contains feature values \n" +
					" 2) the name of the xml file that will contain feature values (in xml format) \n" +
					" 3) folder name where the output data will be stored. " +
					"( This folder name is required since the data will be divided \n in five folds and then " +
					"the features will be stored in xml format and in the format required by SVMs.)");
			System.exit(1);
		}

		DataPreproc4Learning thisC = new DataPreproc4Learning();
		// convert the data from prolog format into XML format:
		thisC.transformPrologIntoXML(prologFileName, xmlFileName, thisC.getNoHyp());
	
			String[] trainFileNames = new String[5];
			String[] testFileNames = new String[5];

			String[] trainSVM = new String[5];
			String[] testSVM = new String[5];

			for (int i = 1; i< 6; i++){
				trainFileNames[i-1] = dirName + "xmlInFiles/" + "nbest_train_fold" + i + ".xml";
				testFileNames[i-1] = dirName + "xmlInFiles/" + "nbest_test_fold" + i + ".xml";
			}
		DataPreprocNfold.divideFor_5FoldCV(xmlFileName, trainFileNames, testFileNames);
		
			for (int i = 1; i< 6; i++){
				trainSVM[i-1] = dirName + "svmInFiles/" + "nbest_train_fold" + i + ".txt";
				testSVM[i-1] = dirName + "svmInFiles/" + "nbest_test_fold" + i + ".txt";
				
				// Put the entire set of features 
				 //in the format required by SVMs:
				thisC.transformCategFeatIntoNumFeat(trainFileNames[i-1], trainSVM[i-1], "WER", true);
				thisC.transformCategFeatIntoNumFeat(testFileNames[i-1], testSVM[i-1], "WER", true);
				
		
				/*
				//Extract only three features (rank, incosistent tense and ...):
				takeSubsetOf3Feat(trainFileNames[i-1], trainSVM[i-1]); 
				// takeSubsetOf3Feat does the same task as transformCategFeatIntoNumFeat, 
				// but only for 3 features
				takeSubsetOf3Feat(testFileNames[i-1], testSVM[i-1]);
										
				//Extract only three features (rank, incosistent tense and ...)
				// WHERE the feature "rank" TAKES ONLY THREE VALUES 
				takeSubsetOf3Feat_rank3vals(trainFileNames[i-1], trainSVM[i-1]); 
				// takeSubsetOf3Feat does the same task as transformCategFeatIntoNumFeat, 
				// but only for 3 features 
				takeSubsetOf3Feat_rank3vals(testFileNames[i-1], testSVM[i-1]); 
*/
			}

		}


}
