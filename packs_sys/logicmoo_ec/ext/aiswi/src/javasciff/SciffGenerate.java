package javasciff;

import gamegui.ChessBoard;
import java.util.Hashtable;
import jpl.Query;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class SciffGenerate implements Runnable {

    private String projectName;

    public SciffGenerate(String projectName) {
        this.projectName = projectName;
    }

    public void run() {
        String result = "";
        if(Query.hasSolution("project(" + projectName + ")")) {
            //Hashtable solution = Query.oneSolution("run, findall_constraints(h(_,_,_),L)");
            Hashtable solution = Query.oneSolution("game(L)");
            if (solution != null) {
                System.out.println("L = " + solution.get("L"));
                result = "" + solution.get("L");
                
            }
        }
        ChessBoard.getInstance().updateGenTrace(result);
        
    }





}
