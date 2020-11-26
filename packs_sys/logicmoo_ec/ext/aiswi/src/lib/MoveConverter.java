package lib;

import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class MoveConverter {
    

    /**
     *
     * @param sciffOutput
     * @return
     */
    public static List<Move> convertStringToMoves(String sciffOutput) {

        System.out.print(sciffOutput);
        List<Move> lm = new Vector<Move>();
        sciffOutput = sciffOutput.replace("'", "");
        StringTokenizer st = new StringTokenizer(sciffOutput, "(");
        int counter = 1;
        while (st.hasMoreTokens()) {
             if (counter == 1){
                 // abbiamo necessita di scartare un token in pii la prima volta
                st.nextToken();
                st.nextToken();
                st.nextToken();
                st.nextToken();
                st.nextToken();
             }else{
                st.nextToken();
                st.nextToken();
                st.nextToken();
                st.nextToken();
             }
             String substring = st.nextToken();
             StringTokenizer st2 = new StringTokenizer(substring, "," );
             String substring2 = st2.nextToken();
             int x = Integer.parseInt(substring2.trim());
             substring2 = st2.nextToken();
             int y = Integer.parseInt(substring2.trim());
             substring2 = st2.nextToken();
             String color = substring2.trim();
             substring2 = st2.nextToken();
             String figure = substring2.trim();
             figure = figure.replace(")", "");
             substring2 = st2.nextToken().trim();
             substring2 = substring2.replace(")", "");
             StringTokenizer st3 = new StringTokenizer(substring2);
             int time = Integer.parseInt(st3.nextToken("."));
             lm.add(new Move(x,y,color, figure, time));
             if (time==0){
                 break;
             }
             counter++;
         }
        Collections.reverse(lm);
        return lm;

    }

}
