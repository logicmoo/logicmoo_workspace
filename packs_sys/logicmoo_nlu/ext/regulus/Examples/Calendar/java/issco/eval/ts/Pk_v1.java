package issco.eval.ts;

import issco.eval.util.SegmentedText;
import issco.util.mgDebug;

/**
* Implementation of P_k as described in:
* <br/> Allan, J., Carbonell, J., Doddington, G., Yamron, J. & Yang, Y. (1998). Topic Detection and Tracking Pilot Study: Final Report. 
* In DARPA Broadcast News Transcription and Understanding Workshop, 194–218, Morgan Kaufmann, Landsdowne, VA. <br/>
* <br/>
* Given the reference and the hypothesised segmentation map, compute P_k components
 * (i.e. P_miss and P_false_alarm ) as follows: <br/>
 *    stat.miss = sum (1-delta_ref)* delta_hyp ; <br/>
 *    stat.diff = sum (1-delta_ref) <br/>
 *<br/> Therefore, <br/>
 *   P_miss = [ sum (1-delta_ref)* delta_hyp ] / [ sum (1 - delta_ref )] = 
 *   	    = stat.miss / stat.diff
 *<br/>
 *    stat.same =  sum (delta_ref); <br/>
 *    stat.false_alarm = sum [delta_ref * (1 - delta_hyp) ] <br/>
 *    P_false_alarm = [ sum [delta_ref * (1 - delta_hyp) ] ] / [ sum (delta_ref) ] =
 *    				= stat.false_alarm / stat.same
 *<br/>
 *      * Obs. (by Maria Georgescul): In this form, P_k is not appropriate since 
 *      segmentation errors within a segment of size smaller than K
 *      are not detected correctly. <br/>
 *      
 *<br/>
 * <p> Copyright (C) 2006 Maria Georgescul, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA </p>
 * <br/>
 * 
 * This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*<br/>
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
 */
public class Pk_v1 extends MetricComputation {
	
    public Pk_v1(SegmentedText ref, SegmentedText hyp, int K) {
    	super(ref, hyp, K);
    }
    
	/**
     *  Computes Pk by taking a window of a fixed number of words (the window size being denoted by K). <br/>
     *  K is usually set as half segment length. <br/>
      *
      * @author Maria Georgescul
      * @returns EvalStats
      *
      * @param refMap int[] refMap contains topic boundary positions (word index) in reference file; i.e.
      * -- for each segment i, refB[i] = word index before the segment, in the reference file
      *        * refMap.length = number of segments
      * @param hypMap int[] contains topic boundary positions in hyp file
      * @param K int
      */
     protected EvalStats computeStats4Error(int[] refB, int[] hypB, int K) {
             EvalStats stat = new EvalStats();

             int ie=refB[refB.length-1] - K ;
             stat.noWords = ie + K ;

             for (int i=0; i<ie; i++) {
               try{
                 // verify if there is a boundary in the interval (i, i+K)
                 // loop all the boundaries in the
                 int noBoundRef = 0;
                 for (int tb = 0; tb < refB.length; tb++){
                   // check if the current boundary is in the interval (i, i+k)
                   if ((refB[tb] >= i) && (refB[tb] < i+K)){
                     noBoundRef++; // count here how many boundaries are in our interval
                   }
                 }
                 int noBoundHyp = 0;
                 for (int tb = 0; tb < hypB.length; tb++){
                   // check if the current boundary is in the interval (i, i+k)
                   if ((hypB[tb] >= i) && (hypB[tb] < i+K)){
                     noBoundHyp++; // count here how many boundaries are in our interval
                   }
                 }
                   if (noBoundRef == 0) {
                     stat.same++;
                     if (noBoundHyp == 0) stat.okay++;
                     else {
                       stat.falseAlarm++;
                     }
                   }
                   else {
                     stat.diff++;
                     if (noBoundHyp == 0) stat.miss++;
                     else stat.okay++;
                   }

               }
               catch(Exception e){
            	   System.out.println("hypmap len = " + hypB.length);
            	   System.out.println("refMap len = " + refB.length);
            	   System.out.println(" refMAp i = " + refB[i]);
            	   System.out.println("hypMap i = " + hypB[i]);
               }
             }// end for
             
             stat = computeScore(stat);   
             return stat;
     }

     
     /**
     * Also computes Pk by taking a window of a fixed number of words (the window size being denoted by K). <br/>
     *  
      * @author Maria Georgescul
      * @returns TopicSegEvalStats
      * @param refMap int[] -- refMap[i] topic index, where i is the word index -- for easch word i, refMap[i]=segment number
      * @param hypMap int[]
      * @param K int
      */
     protected EvalStats computeStats4RefMap(int[] refMap, int[] hypMap, int K) {
             EvalStats stat = new EvalStats();
             boolean refSame, hypSame;
             int ie;

             if (refMap.length > hypMap.length){
               ie=hypMap.length-K;
             }
             else{
               ie=refMap.length-K;
             }

             for (int i=0, ik=K; i<ie; i++, ik++) {
               try{
                     refSame = ((refMap[ik] - refMap[i]) == 0);
                     hypSame = ((hypMap[ik] - hypMap[i]) == 0);
                     if (refSame) {
                             stat.same++;
                             if (hypSame) stat.okay++;
                             else {
                               stat.falseAlarm++; 
                             }
                     }
                     else {
                             stat.diff++;
                             if (hypSame) stat.miss++;
                             else stat.okay++;
                     }
             }
             catch(Exception e){
               System.out.println("ERR when compute stats at i = " + i + " ik = " + ik );
               System.out.println("hypMap len = " + hypMap.length);
               System.out.println("refMap len = " + refMap.length);

               System.out.println( "refMap ik = " + refMap[ik] );
               System.out.println(" refMAp i = " + refMap[i]);
               System.out.println("hypMap ik = " + hypMap[ik]);
               System.out.println("hypMap i = " + hypMap[i]);
             }

             }
             
             stat = computeScore(stat);            
             return stat;
     }   

     /**
      * Compute P_k (version 1) for two annotation files.
      * @param arg
      */
     public static void main(String args[]) {
    	 mgDebug.header("Implementation of the P_k evaluation metric proposed by (Allan et al., 1998).");
    	 
    	 String refFile, hypFile;
    	 int K = 2;
    	 float pseg = (float)0.5;
    	 
    	 //	refFile = "C:/Documents and Settings/GEORGESC/Desktop/eval/ref/test.ref";
    	 // hypFile = "C:/Documents and Settings/GEORGESC/Desktop/eval/hyp/test.out";
    	 
    	 if(args.length == 4){
    		 refFile = args[0];
        	 hypFile = args[1];
        	 try{
        		 K = new Integer(args[2]).intValue();
        	 }
        	 catch(Error er){
 				System.out.println("Please provide as the third argument, an integer value specifying k, i.e. the window dimmension.");
 				System.exit(0);
        	 }
        	 try{
        		 pseg = new Float(args[3]).floatValue();        		
        	 }
        	 catch(Error er){
  				System.out.println("The fourth argument should be a float value specifying P_seg for your reference data.");
 				System.exit(0);
        	 }
    
        	 SegmentedText hyp = new SegmentedText();
        	 if (hyp.initSegmentedText(hypFile) == false){
     	        System.out.println("Err when loading hypothesised segmentation file  ");
     	        return;
        	 };
        	 SegmentedText ref = new SegmentedText();
     	    	if (ref.initSegmentedText(refFile) == false ){
     	    		System.out.println("Err when loading reference segmentation file  ");
     	    		return;
     	    	};

     	  
     	    	Pk_v1 eval = new Pk_v1(ref, hyp, K);
	    	    eval.stats.error = (eval.stats.p_miss * pseg + eval.stats.p_fa * (1 - pseg));

     	  /* Print results */
     	  // System.out.println(eval.stats.toString());
    	  System.out.println("For k = " + K + " and P_seg = " + pseg + " : \n   P_k = " + (eval.stats.p_miss * pseg + eval.stats.p_fa * (1 - pseg)) );
    	  System.out.println("   P_miss  = " + eval.stats.p_miss );
    	  System.out.println("   P_FalseAlarm  = " + eval.stats.p_fa );
    	 }    
    	 else{
				System.out.println("The following arguments should be provided : ");
				System.out.println("1) the name of the file containing the reference topic segmentation;");
				System.out.println("2) the name of the file containing the hypothetised topic segmentation;");
				System.out.println("3) an integer value specifying k, i.e. the window dimmension;");
				System.out.println("4) a float value specifying P_seg for your reference data.");
				System.exit(-1);
    	 }

     	}


}
