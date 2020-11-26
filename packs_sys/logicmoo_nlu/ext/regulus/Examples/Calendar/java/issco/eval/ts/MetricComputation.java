package issco.eval.ts;

import issco.eval.util.Vectorx;
import issco.eval.util.SegmentedText;


/**
 * Abstract class for topic segmentation metric computation</p>
 *
 *
 * <p> * Copyright (C) 2006, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA </p>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <br>
 *
 * @author Maria Georgescul
 * @version 1.0
 */

abstract class MetricComputation {
     
        EvalStats stats = new EvalStats();
        int noBoundsRef = 0;
     
	
        /**
         * @deprecated Use computeBothPkAndMyMetric(int[] refB, int[] hypB, int K) instead
         */
        public MetricComputation() {}

        /**
         * Given the reference and hypothesised segmentation compute the metric. 
         * @see computeError, computeStats4Error
         */
        public MetricComputation(SegmentedText ref, SegmentedText hyp, int K) {
        	final int[] refB = Vectorx.toIntArray(ref.topicBoundaries()); // Convert a list of Integers into an array of int
        	final int[] hypB = Vectorx.toIntArray(hyp.topicBoundaries());
        	noBoundsRef = ref.topicBoundaries.size() - 1;
        	computeError(refB, hypB, K);
        }

        protected class EvalStats {
            protected int okay=0;
            protected int miss=0;
            protected int falseAlarm=0;
            protected int same=0;
            protected int diff=0;
            
            protected float p_miss = 0;
            protected float p_fa = 0;
            
            protected int p_miss_numerator = 0;
            protected int p_miss_denominator = 0;
            protected int p_fa_numerator = 0;
            protected int p_fa_denominator = 0;
            protected float error = -1;
            protected float denominator = 0;
            protected int noWords = 0;
     
            public String toString(){
            	String rez = "";
            	rez += "okay        = " + okay + "\n";
            	rez += "miss        = " + miss + "\n";
            	rez += "false alarm = " + falseAlarm + "\n";
            	rez += "same        = " + same + "\n";
            	rez += "diff        = " + diff + "\n";
            	rez += "p_miss      = " + p_miss + "\n";
            	rez += "p_fa        = " + p_fa + "\n";
            	return rez;
            }
}
        
        // abstract protected EvalStats computeStats4RefMap(int[] refMap, int[] hypMap, int K);   	
        
        /**
         * 	Given the reference and hypothesised segmentation
         * compute the score. 
         * @param refB int[] Reference boundaries
         * @param hypB int[] Hypothesised boundaries
         * @see computeStats4Error
         */
        private void computeError(int[] refB, int[] hypB, int K) {
        	try{
        		// int[] mapRef = computeMap(refB);
        		// int[] mapHyp = computeMap(hypB);
        		stats = computeStats4Error(refB, hypB, K);
        		//stats = computeStats4RefMap(mapRef, mapHyp ,K);        		
        	}
        	catch(Exception ex){
        		if (refB == null){
        			System.out.println("The pointer to the reference data is null!! ");
        			System.exit(1);
        		}
        		else{
        			if (refB.length == 0){
        				System.out.println("The 'reference' file does not contain any boundaries.");
        				System.out.println("Please note that any reference/hypothetise file should contain the string '==========' as marking the start/end of the document (even if the entire document contains only one thematic episode).");
        				System.exit(1);
        			}
        		}
        		if (hypB == null){
        			System.out.println("The pointer to the hypothetised data is null!! ");
        			System.exit(1);
        		}
        		else{
        			if (hypB.length == 0){
        				System.out.println("The '.hyp' file does not contain any boundaries!");
        				System.out.println("Please note that any reference/hypothetise file should contain the string '==========' as marking the start/end of the document (even if the entire document contains only one thematic episode).");
        				System.exit(1);
        			}
        		}
        		ex.printStackTrace();
        	}
        }

/**
 * Given the reference segmentation, compute K.
 * @return int
 * @param B int[] The boundaries
 */
private static int computeK(int[] B) {
        /* K is defined as the half mean segment length (in words) */

        /* Handle odd cases */
        if (B.length < 2) return B[B.length-1] / 2;

        /* Compute mean segment length */
        int sum = 0;
        for (int i=B.length; i-->1;) sum += (B[i] - B[i-1]);
        float mean = sum / (float) (B.length - 1);

        return Math.round(mean / (float) 2);
}


/**
 * Given a piece of segmented text, generate a map where
 *    each array position corresponds to a token and
 *    the array cell value records the segment number of the word.
 * @return int[] The map
 * @param B int[] The boundaries
 */
private static int[] computeMap(int[] B) {
        int[] map = new int[B[B.length-1]];

        int number=1; // Start at one, since we can ignore the first implicit boundary

        for (int i=0, ie=map.length; i<ie; i++) {
                if (i == B[number]) number++;
                map[i]=number;
        }

        return map;
}

/**
 * Given the raw statistics, compute the score; available from "Pk_original"
 * @param stat TopicSegEvalStats
 */
protected EvalStats computeScore(EvalStats stat) {

        float p_diff = stat.diff / (float) (stat.diff + stat.same);

        //p(same ref segments | ref, k) 
        float p_same = stat.same / (float) (stat.diff + stat.same);

        //p(miss | ref, hyp, different ref segment, k)
        if (stat.diff == 0)
        	stat.diff = 1;
        stat.p_miss = (float)stat.miss / (float)stat.diff;
        stat.p_miss_numerator = stat.miss;
        stat.p_miss_denominator = stat.diff;

        // p(false alarm | ref, hyp, same ref segment, k)
        if (stat.same == 0)
        	stat.same=1;
        stat.p_fa = stat.falseAlarm / (float) stat.same;
        stat.p_fa_numerator = stat.falseAlarm;
        stat.p_fa_denominator = stat.same;

        // p(error| ref, hyp, k) 
        float p_error2 = (stat.p_miss * p_diff) + (stat.p_fa * p_same);
        stat.error = p_error2;

        return stat;
}

      
      /**
       * Given the reference and the hypothesised segmentation map, compute the evaluation statistics.
       * Creation date: 13 oct 2005
       * @author Maria Georgescul
       * @returns TopicSegEvalStats
       * @param refB int[] -- for each segment i, refB[i] = word index before the thematic segment, in the reference file
       * @param hypB int[] -- for each segment i, hypB[i] = word index before the thematic segment, in the hyp file
       * @param K int
       */
      protected abstract EvalStats computeStats4Error(int[] refB, int[] hypB,  int K); 



  public EvalStats getStats() {
    return stats;
  }
  
  public int getNoBoundsRef() {
    return noBoundsRef;
  }

}



