package issco.eval.ts;

import issco.eval.util.SegmentedText;
import issco.util.mgDebug;

/*
* <p> Implementation of the evaluation metric proposed by (Georgescul et al., 2006) for topic segmentation algorithms. </p> <br>
* The evaluation metric called "Pr_error" is defined as:
* <br> Pr_error = C_Miss * Pr_Miss + C_FalseAlarm * Pr_FalseAlarm
* <br> For a detailed description see: <br>
*inproceedings{Georgescul_eval:2006, <br>
*   Author = "Georgescul, Maria and Clark, A. and Armstrong, S.", <br>
*   Title = "{An Analysis of Quantitative Aspects in the Evaluation of Thematic Segmentation Algorithms}", <br>
*   BookTitle = "SIGDIAL", <br>
*   Address = "Sydney, Australia", <br>
*   Year = "2006" }<br> <br>
* <br>
* <br>
* In this implementation, the Pr_error is estimated for the given reference and hypothetised files.
* Thus the first parameter of the main method should be a String specifying the full path name of 
* the file containing the reference segmentation.
* The second parameter of the main method should be the full path name of the 
* file containing the hypothetised segmentation.
* <br> Each ref/hyp file should be in a text format, where each topic segment is marked by "==========" string.
* <br> Please also note that any input (ref / hyp) file should contain the string '==========' 
* as marking the start/end of the document 
* (also when the entire document contains only one thematic episode).
*<br>
* 
* Copyright (C) 2006 Maria Georgescul, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
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
* <br>
* 
*@author Maria GEORGESCUL
*@version 1.0 
*/
public class Pr_error extends MetricComputation{

    public Pr_error(SegmentedText ref, SegmentedText hyp, int K) {
    	super(ref, hyp, K);
    }
    
    /**
     *  Computes Pr_error by taking a window of a fixed number of words (denoted by K).<br/>
     *  K is usually set as half segment length.<br/>
     *  
     * Given the reference and the hypothesised segmentation map, compute Pr_error components
     * (i.e. Pr_miss and Pr_false_alarm ) as follows: <br/>
     *   Pr_miss = [ sum (1-delta_ref)* teta_hyp ] / [ sum (1 - delta_ref )] </br>
     *   Pr_false_alarm = [ sum [ tau_hyp) ] ] / [N - k] </br>
     * delta_ref = 0 when r(i, i+k) > 0; </br>
     *           = 0, when r(i, i+k) = 0 </br>
     * teta_hyp = 1 when h(i, i+k) < r(i, i+k) </br>
     *           = 0, otherwise </br>
     * tau_hyp  = 1 when h(i, i+k) > r(i, i+k) </br>
     *          = 0 otherwise </br>
     *
     * @author Maria Georgescul
     * @returns TopicSegEvalStats
     * @param refB int[] -- for each segment i, refB[i] = word index before the thematic segment, in the reference file
     * @param hypB int[] -- for each segment i, hypB[i] = word index before the thematic segment, in the hyp file
     * @param K int
     */
    protected EvalStats computeStats4Error(int[] refB, int[] hypB,  int K){
            EvalStats stat = new EvalStats();
            int ie = refB[refB.length-1] - K ;
            stat.noWords = ie ;
            int noLoops = 0;

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

              int delta_ref = 0;
              int teta_hyp = 0;
              int tau_hyp = 0;

              if (noBoundRef == 0) {
                delta_ref = 1;
              }
              else { // there are > 0 boundaries in that interval for "ref" file
                delta_ref = 0;
              }

                if (noBoundHyp < noBoundRef ) {
                  teta_hyp = 1;
                  tau_hyp = 0;
                }
                else {
                  if (noBoundHyp > noBoundRef ){
                    teta_hyp = 0;
                    tau_hyp = 1;
                  }
                  else{
                    teta_hyp = 0;
                    tau_hyp = 0;
                  }
                }

              stat.p_miss_numerator += ( teta_hyp * (1-delta_ref));
              stat.p_miss_denominator += (1-delta_ref) ;
              stat.p_fa_numerator += tau_hyp;
              noLoops++;
            }
            catch(Exception e){
              System.out.println("ERR when compute stats at i = " + i + " ik = " + i+ K );
            }

            } // end for
            stat.p_fa_denominator = ie;
            if ((stat.p_miss_denominator !=0 ) && (stat.p_fa_denominator !=0 )){
              stat.p_miss = ((float) stat.p_miss_numerator / ((float) stat.p_miss_denominator));
              stat.p_fa = (float)stat.p_fa_numerator / (float)stat.p_fa_denominator;

              // stat.error = stat.p_miss + stat.p_fa ;
            }
            else {
              stat.error = -1;
            }
            return stat;
    }

    
    /**
     * Not implemented.
     
    protected EvalStats computeStats4RefMap(int[] refMap, int[] hypMap, int K) {
    	return new EvalStats();
    }
    */
    
    /**
     * Compute Pr_error for two annotation files.
     * @param arg
     */
    public static void main(String args[]) {
   	 mgDebug.header("Implementation of the Pr_error evaluation metric proposed by (Georgescul et al., 2006).");
	 
	 String refFile, hypFile;
	 int K = 2;
	 float C_miss = (float) 0.5;
	 
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
    		 C_miss = new Float (args[3]).floatValue();        		
    	 }
    	 catch(Error er){
				System.out.println("The fourth argument should be a double value ( in the interval [0, 1] ) specifying C_miss for your reference data.");
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

    	  
    	    Pr_error eval = new Pr_error(ref, hyp, K);

    	    
    	  /* Print results */
       	  // System.out.println(eval.stats.toString());
    	  eval.stats.error = C_miss * eval.stats.p_miss + (1 - C_miss ) * eval.stats.p_fa; 
      	  System.out.println("For k = " + K + " and C_miss = " + C_miss + 
      			  " : \n   Pr_error = " + eval.stats.error );
      	  System.out.println("   Pr_miss  = " + eval.stats.p_miss );
      	  System.out.println("   Pr_fa    = " + eval.stats.p_fa );
      	 }    
      	 else{
  				System.out.println("The following arguments should be provided : ");
  				System.out.println("1) the name of the file containing the reference topic segmentation;");
  				System.out.println("2) the name of the file containing the hypothetised topic segmentation;");
  				System.out.println("3) an integer value specifying k, i.e. the window dimmension;");
  				System.out.println("4) a double value ( in the interval [0, 1] ),  specifying the constant C_miss.");
  				System.exit(-1);
      	 }

       	}

}
