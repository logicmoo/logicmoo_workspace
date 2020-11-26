package test;

import lib.Move;
import javasciff.Project;
import javasciff.SProject;
import javasciff.SciffBridge;

/**
 *
 * @author Andrea Grandi
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        System.out.println(System.getProperty("user.dir"));

        // Crea un nuovo pogettto chiamato "nuovo"
        SProject project = new SProject("nuovo2");

        // Aggiunge delle regole
//        project.rules.add("H(start,0) ---> E(sono(1,1,_,_),0).");
//        project.rules.add("H(start,0) ---> E(sono(10,10,_,_),T) /\\ T > 0 /\\ EN(sono(_,_,_,_),T2) /\\ T2 > T.");
//        project.rules.add("H(sono(X,Y,C,fiore),T) ---> E(sono(X2,Y2,C2,teschio),T2) /\\ T2 > T.");

        project.kb = "inzio(1,1,_,_).";

        // Aggiunge la traccia degli eventi
        project.trace.add(new Move(1, 1, "blue", "skull", 0));
//        project.trace.add("hap(sono(5,5,rosa,null),1).");
//        project.trace.add("hap(sono(10,10,blu,teschio),2).");


        // Crea un nuovo SciffBridge per poter verificare il progetto
        SciffBridge sciff = new SciffBridge("sciff/");

        // Lo verifica e stampa l'esito su terminale
        System.out.println("Nuovo: " + sciff.runProject(project));
    }

    
/**
    private static void execute(String goal) {
        System.out.println(goal + ": " + Query.hasSolution(goal) + " - " + Query.oneSolution(goal));
    }

    private static void testPL() {
        Query q1 = new Query("consult", new Term[]{new Atom("test.pl")});
        System.out.println("consult " + (q1.query() ? "succeeded" : "failed"));
        Query q2 = new Query("child_of", new Term[]{new Atom("joe"), new Atom("ralf")});
        System.out.println("child_of(joe,ralf) is " + (q2.query() ? "provable" : "not provable"));
        Hashtable solution = Query.oneSolution("descendent_of(X, ralf)");
        System.out.println("first solution of descendent_of(X, ralf)");
        System.out.println("X = " + solution.get("X") + " - " + solution);
        System.out.println(Query.oneSolution("child_of(joe,X)"));
    } 
 */
}
