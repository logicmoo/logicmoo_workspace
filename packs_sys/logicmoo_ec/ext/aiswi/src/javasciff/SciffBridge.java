package javasciff;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Hashtable;
import jpl.*;

/**
 * Classe per l'interfacciamento con SCIFF.
 * Consente di compilare sciff ed eseguire un progetto invocando gli opportuni comandi prolog.
 *
 * L'esecuzione dei comandi prolog viene effettuata tramite jpl.
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class SciffBridge {

    /***
     * Percorso relativo della cartella contenente SCIFF
     */
    private String path;


    /***
     * Crea un nuovo oggetto SciffBridge cercando di compilare SCIFF
     * a partire dal percorso specificato.
     *
     * @param path percorso della cartella contenente SCIFF
     */
    public SciffBridge(String path) {
        this.path = path;
    }

    
    public SciffBridge() {
        this("");
    }

    /**
     * Esegue un progetto SCIFF e ritorna l'esito.
     * Il progetto deve essere precedentemente creato, cioè memorizzato su filesystem.
     *
     * @param project Progetto SCIFF
     * @return esito dell'esecuzione
     */
    public boolean runProject(SProject project) {
        project.generateProject();
        return runProject(project.getProjectName());
    }

    public String runGenerativeProject(SProject project) {
        project.generateProject();
        return runGenerativeProject(project.getProjectName());
    }

    public Thread runThreadGenerativedProject(SProject project) {
        project.generateProject();
        return runThreadGenerativeProject(project.getProjectName());
    }

    /**
     * Esegue il progetto SCIFF - il cui nome è pasato come parametro - e ritorna l'esito.
     * L'esecuzione avviene invocando i comandi prolog "project(nome)." e "run."
     *
     * @param project Nome del progetto SCIFF
     * @return esito dell'esecuzione
     */
    public boolean runProject(String projectName) {
        boolean result;
        
        setDefaultPath();
        if(Query.hasSolution("project(" + projectName + ")")) {
            result = Query.hasSolution("run");
            if (!result) {
                Query.hasSolution("set_option(violation_causes_failure, no)");
                Query.hasSolution("run");
                Query.hasSolution("set_option(violation_causes_failure, yes)");
            }

            return result;
        }



        return false;
    }

    public Thread runThreadGenerativeProject(String projectName) {
        setDefaultPath();
        SciffGenerate sg = new SciffGenerate(projectName);
        Thread sgThread = new Thread(sg);
        sgThread.start();

        return sgThread;
    }


    public String runGenerativeProject(String projectName) {

        String result = "";

        setDefaultPath();
        if(Query.hasSolution("project(" + projectName + ")")) {
//            Hashtable solution = Query.oneSolution("game(L)");
//            System.out.println(Query.oneSolution("run, findall_constraints(h(_,_,_),X)"));

            Hashtable solution = Query.oneSolution("run, findall_constraints(h(_,_,_),L)");
            if (solution != null) {
                System.out.println("L = " + solution.get("L"));
                result = "" + solution.get("L");
                return result;
             }
        }
        return result;
    }

    /***
     * Esegue il comando prolog: "compile(sciff)"
     */
    public void compileSciff() {
        SciffCompiler sc = new SciffCompiler(path);
        Thread scThread = new Thread(sc);
        scThread.start();
    }

    /***
     * Esegue il comando prolog: "compile(sciff)"
     */
    public void loadClpModule() {
        Query.hasSolution("use_module(library(clpfd))");
    }


    /**
     * Inserisce all'interno del file "default.pl" il percorso corrente.
     * In questo modo sarà possibile eseguire correttamente i progetti creati.
     */
    private void setDefaultPath() {
        try {
            FileWriter fstream = new FileWriter(path + "/defaults.pl");
            BufferedWriter out = new BufferedWriter(fstream);
            System.getProperty("user.dir");
            out.write("default_dir('" + System.getProperty("user.dir") + "/').");
            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }


    /***
     * @deprecated 
     * @param goal
     */
    private static void execute(String goal) {
        System.out.println(goal + ": " + Query.hasSolution(goal) + " - " + Query.oneSolution(goal));
    }
}
