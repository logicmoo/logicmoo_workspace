package issco.eval.ts;

import issco.eval.util.SegmentedText;
import issco.util.mgDebug;

/**
* Implementation of WindowDiff, metric proposed by (Pevzner & Hearst, 2002).
* <br/> )  

* <br/>
* Given the reference and the hypothesised segmentation map, compute WindowDiff
 *  as follows: <br/>
 *  WindowDiff = ( 1/(N-K) ) / sum_{i=1,..,N-k} (diff_i), where <br/>
 *  diff_i = 1 if |r_i-h_i| > 0; <br/>
 *  diff_i = 0 if (r_i == h_i). <br/>
 *  r_i is the number of reference boundaries in the interval (i, i+k)  <br/>
 *  h_i is the number of hypothetised boundaries in the interval (i, i+k) <br/>
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
public class WindowDiff extends MetricComputation {
	
    public WindowDiff(SegmentedText ref, SegmentedText hyp, int K) {
    	super(ref, hyp, K);
    }
    
	/**
     *  Computes ... <br/>
      *
      * @author Maria Georgescul
      * @returns EvalStats
      *
      * refMap int[] refMap contains topic boundary positions (word index) in reference file; i.e.
      * -- for each segment i, refB[i] = word index before the segment, in the reference file
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
                   if (noBoundRef != noBoundHyp ) {
                     stat.diff++;
                   }                   
               }
               catch(Exception e){
            	   System.out.println("ERR occuring when computin WindowDif at : ");
            	   System.out.println("hypmap len = " + hypB.length);
            	   System.out.println("refMap len = " + refB.length);
               }
             }// end for
             
             stat.error = (float) stat.diff / (float) ie;
             return stat;
     }

     
     
     /**
      * Compute WindowDiff for two annotation files.
      * @param arg
      */
     public static void main(String args[]) {
    	 mgDebug.header("Implementation of WindowDiff, metric proposed by (Pevzner & Hearst, 2002).");
    	 
    	 String refFile, hypFile;
    	 int K = 2;   	 
    	 //	refFile = "C:/Documents and Settings/GEORGESC/Desktop/eval/ref/test.ref";
    	 // hypFile = "C:/Documents and Settings/GEORGESC/Desktop/eval/hyp/test.out";
    	 
    	 if(args.length == 3){
    		 refFile = args[0];
        	 hypFile = args[1];
        	 try{
        		 K = new Integer(args[2]).intValue();
        	 }
        	 catch(Error er){
 				System.out.println("Please provide as the third argument, an integer value specifying k, i.e. the window dimmension.");
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

     	  
     	    	WindowDiff eval = new WindowDiff(ref, hyp, K);

     	  /* Print results */
    	  System.out.println("For k = " + K + " : \n  WindowDiff  = " + eval.stats.error );
    	 }    
    	 else{
				System.out.println("The following arguments should be provided : ");
				System.out.println("1) the name of the file containing the reference topic segmentation;");
				System.out.println("2) the name of the file containing the hypothetised topic segmentation;");
				System.out.println("3) an integer value specifying k, i.e. the window dimmension;");
				System.exit(-1);
    	 }

     	}


}
