package issco.eval.ts;

import issco.eval.util.SegmentedText;
import issco.util.mgDebug;

	/**
	*  Implementation of Pk as described in "TDT Evaluation Specification Version 3.7. "
	*
	* Given the reference and the hypothesised segmentation map, compute Pk components
	* (i.e. P_miss and P_false_alarm ) as follows:
	*   P_miss = [ sum (1-delta_ref)* (1 - omega_hyp ] / [ sum (1 - delta_ref )]
	*   P_false_alarm = [ sum [delta_ref * (1 - omega_hyp) ] ] / [ sum (delta_ref) ]
	*
	* delta_ref = 1 when # boundaries between words i and j in reference file f;
	*           = 0, otherwise
	* omega_hyp = 1 when # boundaries between words 1 and j is the same in hyp as in ref
	*           = 0, otherwise
	*
	* Obs. (by Maria Georgescul): P_miss is incremented instead P_false_alarm for some examples
	* e.g. if   ( #boundaries in ref =  1) and (#boundaries in hyp =  2)
	*      then delta_ref = 0; omega_hyp = 0 and consequently the P_miss is incremented
	*      but in reality we do have a false alarm
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
	*@author Maria Georgescul
	*/
	public class Pk_v2 extends MetricComputation{

	    public Pk_v2(SegmentedText ref, SegmentedText hyp, int K) {
	    	super(ref, hyp, K);
	    }
	    
		/**
		 * Computes P_k (version 2) by taking a window of a fixed number of words (the window size being denoted by K). <br/>
	     *  K is usually set as half segment length. <br/>
	     *    
	     * @returns EvalStats
	     * @param refB int[] -- for each segment i, refB[i] = word index before the segment, in the reference file
	     * @param hypB int[] -- for each segment i, hypB[i] = word index before the segment, in the hyp file
	     * @param K int
	     */
	    protected EvalStats computeStats4Error(int[] refB, int[] hypB,  int K) {
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
	                  noBoundHyp++; 
	                }
	              }

	              int delta_ref = 0;
	              int omega_hyp;

	              if (noBoundRef == 0) {
	                delta_ref = 1;
	                if (noBoundRef == noBoundHyp) {
	                  omega_hyp = 1;
	                }
	                else { // noRefSame == 0 and noHypSame > 0
	                  omega_hyp = 0;
	                }
	              }
	              else { // there are boundaries in that interval for "ref" file
	                delta_ref = 0;
	                if (noBoundRef == noBoundHyp)
	                  omega_hyp = 1;
	                else {
	                  omega_hyp = 0;
	                }
	              }
	              stat.p_miss_numerator += ( (1- omega_hyp) * (1-delta_ref));
	              stat.p_miss_denominator += (1-delta_ref) ;
	              stat.p_fa_numerator += ( (1- omega_hyp)* delta_ref );
	              stat.p_fa_denominator += delta_ref;
	              noLoops++;
	            }
	            catch(Exception e){
	              System.out.println("ERR when compute stats at i = " + i + " ik = " + i+ K );
	              System.out.println("hypmap len = " + hypB.length);
	              System.out.println("refMap len = " + refB.length);
	            }

	            } // end for
	            if (stat.p_miss_denominator ==0)
	            	stat.p_miss_denominator = 1;
	            if (stat.p_fa_denominator ==0)
	            	stat.p_fa_denominator =1;
	            // if ((stat.p_miss_denominator !=0 ) && (stat.p_fa_denominator !=0 )){

	              stat.p_miss = ((float) stat.p_miss_numerator / ((float) stat.p_miss_denominator));
	              stat.p_fa = (float)stat.p_fa_numerator / (float)stat.p_fa_denominator;
	              //stat.error = stat.p_miss * p_seg + stat.p_fa * ((float)1 - p_seg);
	             
/*
	            }
	            else {
	            	System.out.println("p_miss_denominator ==0  or p_fa_denominator ==0 ");
	              stat.error = -1;
	            }*/
	            //stat = computeScore(stat);
	            return stat;
	    }
	    
	    
	    /**
		 * As computeStats4Error, this method also computes P_k (version 2) by taking a window of a fixed number of words (the window size being denoted by K). <br/>
	     *  K is usually set as half segment length. <br/>
	     *
	     * Given the reference and the hypothesised segmentation map, compute the
	     * evaluation statistics.
	     * Creation date: 1 oct 2005
	     * @author Maria Georgescul
	     * @returns EvalStats
	     * @param refMap int[]
	     * @param hypMap int[]
	     * @param K int
	     */
	    protected EvalStats computeStats4RefMap(int[] refMap, int[] hypMap, int K) {
	            EvalStats stat = new EvalStats();

	            int noRefSame, noHypSame;
	            int delta_ref;
	            int omega_hyp;
	            int ie;

	            if (refMap.length > hypMap.length){
	              ie=hypMap.length-K;
	            }
	            else{
	              ie=refMap.length-K;
	            }
	            stat.noWords = ie ;

	            for (int i=0, ik=K; i<ie; i++, ik++) {
	              try{
	                    noRefSame = refMap[ik] - refMap[i];
	                    noHypSame = hypMap[ik] - hypMap[i];
	                    if (noRefSame == 0) {
	                            delta_ref = 1;
	                            if (noHypSame == noRefSame) {
	                              omega_hyp = 1;
	                            }
	                            else { // noRefSame == 0 and noHypSame > 0
	                              omega_hyp = 0;
	                            }
	                    }
	                    else {
	                            delta_ref = 0;
	                            if (noHypSame == noRefSame)
	                              omega_hyp = 1;
	                            else {
	                              omega_hyp = 0;
	                            }
	                    }
	                    stat.p_miss_numerator += ( (1- omega_hyp) * (1-delta_ref));
	                    stat.p_miss_denominator += (1-delta_ref) ;
	                    stat.p_fa_numerator += ( (1- omega_hyp)* delta_ref );
	                    stat.p_fa_denominator += delta_ref;
	            }
	            catch(Exception e){
	              System.out.println("ERR when compute stats at i = " + i + " ik = " + ik );
	              System.out.println("hypmap len = " + hypMap.length);
	              System.out.println("refMap len = " + refMap.length);

	              System.out.println( "refMap ik = " + refMap[ik] );
	              System.out.println(" refMAp i = " + refMap[i]);
	              System.out.println("hypMap ik = " + hypMap[ik]);
	              System.out.println("hypMap i = " + hypMap[i]);
	            }

	            } // end for
	            float p_seg =  (float)stat.p_miss_denominator / (float)ie;
	            System.out.println("p_miss_den = " + stat.p_miss_denominator + "  p_fa_den = " + stat.p_fa_denominator);
	            if ((stat.p_miss_denominator ==0 ))
	            	stat.p_miss_denominator = 1; 
	            if ((stat.p_fa_denominator ==0 ))
	            	stat.p_fa_denominator =1; 
	            // if ((stat.p_miss_denominator !=0 ) && (stat.p_fa_denominator !=0 )){

	              stat.p_miss = ((float) stat.p_miss_numerator / ((float) stat.p_miss_denominator));
	              stat.p_fa = (float)stat.p_fa_numerator / (float)stat.p_fa_denominator;
	              System.out.println("p_miss = " + stat.p_miss + "  p_fa = " + stat.p_fa);
	              stat.error = stat.p_miss * p_seg + stat.p_fa * ((float)1 - p_seg);
	            // }
	            //else {
//	              stat.error = -1;
	//            }
	            return stat;
	    }


	    /**
	     * Compute P_k (version 2) for two annotation files.
	     * @param arg
	     */
	    public static void main(String args[]) {
	        
	    	mgDebug.header("Implementation of the P_k evaluation metric proposed in ''TDT Evaluation Specification Version 3.7.''");
	    	 
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
	    	  
	    	    Pk_v2 eval = new Pk_v2(ref, hyp, K);
	    	    eval.stats.error = (eval.stats.p_miss * pseg + eval.stats.p_fa * (1 - pseg));
	    	    
	    	  /* Print results */
	     	  // System.out.println(eval.stats.toString());
	    	    
	    	  System.out.println("For k = " + K + " and P_seg = " + pseg + 
	    			  " : \n   P_k = " +  eval.stats.error);
	    	  System.out.println("   P_miss  = " + eval.stats.p_miss );
	    	  System.out.println("   P_FalseAlarm  = " + eval.stats.p_fa );
	      	 } // end if 
	      	 else{
	  				System.out.println("The following arguments should be provided : ");
	  				System.out.println("1) the name of the file containing the reference topic segmentation;");
	  				System.out.println("2) the name of the file containing the hypothetised topic segmentation;");
	  				System.out.println("3) an integer value specifying k, i.e. the window dimmension;");
	  				System.out.println("4) a float value specifying P_seg for your reference data.");
	  				System.exit(-1);
	      	 }// end else
	    }// end main

	}
