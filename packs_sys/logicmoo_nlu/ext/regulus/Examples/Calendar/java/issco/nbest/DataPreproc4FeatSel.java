package issco.nbest;

import issco.eval.*;
import issco.nbest.DataPreproc;

import java.io.*;

import org.jdom.Element;
import org.jdom.Document;
import org.jdom.output.XMLOutputter;
import org.jdom.*;

/** Class implementing methods useful for data pre-processing 
 * required before feature selection.
 * 
 *@see DataPreprocNfold
 *@author GEORGESCUL Maria, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA  
*/
public class DataPreproc4FeatSel extends DataPreproc{
	
	DataPreproc4FeatSel(){
		super();
		// Start re-defining feature attributes (i.e. redefine the set of features to be used for feature selection
		rank.setUsed(true); 
		lenRecWords.setUsed(false); 
		conf.setUsed(true);  
		noWordsResponse.setUsed(false); 
		availableResponse.setUsed(false); 
		available_lf.setUsed(true); 
		available_referent.setUsed(false); 
		resolution.setUsed(true); 
		elliptical.setUsed(true); 
		u_query.setUsed(true); 
		i_tense.setUsed(true); 
		responseType.setUsed(true);
		dialogueMove.setUsed(true);
		definiteMeetingReferent.setUsed(true);	// "definite_meeting_and_meeting_referent", 0),
		noDialogueMove.setUsed(true);
		non_indefinite_existential.setUsed(true);
		non_show_imperative.setUsed(true);		
		definite_meeting.setUsed(true);
		indefinite_meeting.setUsed(true);
		indefinite_meeting_and_meeting_referent.setUsed(true);
		meeting_referent_available.setUsed(true);
		
	    
		
		
	} // end constructor
		
		
			/**
			 * 
			 * @param args String[3]
			 */	
			public static void main(String[] args){
				DataPreproc4FeatSel thisC = new DataPreproc4FeatSel();
				
				// For instance :
				args = new String[3];
				args[0] = "D:/svm_light/calendar_data_allFeatures/nbest_training_data.pl";
				args[1] = "D:/svm_light/calendar_data_allFeatures/nbest_training_data.xml";				
				args[2] = "D:/svm_light/calendar_data_allFeatures/nbest_training_data.txt";
				
				String prologFileName = "";
				String xmlFileName ="";			
				String svmFileName ="";
				try{
					prologFileName = args[0];
					xmlFileName = args[1];
					svmFileName = args[2];
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
					// convert the data from prolog format into XML format:
				thisC.transformPrologIntoXML(prologFileName, xmlFileName, thisC.getNoHyp());
			
	
				// Put the entire set of features in the format required by SVMs:
				thisC.transformCategFeatIntoNumFeat(xmlFileName, svmFileName, "SER", false);
				
				}

}
