package javasciff;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.List;
import java.util.Vector;

/**
 * Rappresenta un progetto SCIFF.
 *
 * Consente di creare un progetto tramite Java, specificando le regole e la traccia.
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class Project {

    /**
     * Nome del file che conterrà le regole
     */
    public static final String rulesFile = "rules.txt";

    /**
     * Nome del file che conterrà la traccia
     */
    public static final String traceFile = "trace.txt";

    /**
     * Lista di regole
     */
    public List<String> rules;

    /**
     * Lista della traccia
     */
    public List<String> trace;

    /**
     * Nome del progetto. Coincide con il nome della cartella che conterrà il progetto
     */
    private String projectName;

    /**
     * Restituisce il nome del progetto
     * @return nome del progetto
     */
    public String getProjectName() {
        return projectName;
    }

    /**
     * Crea un nuovo progetto con il nome specificato
     * @param projectName nome del progetto
     */
    public Project(String projectName) {
        this.projectName = projectName;
        rules = new Vector<String>();
        trace = new Vector<String>();
    }

    
    /**
     * Memorizza il progetto su filesystem per permetterne l'esecuzione tramite SCIFF.
     *
     * Crea la cartella che conterrà il progetto e crea tutti i file necessari.
     */
    public void create() {
        (new File(projectName)).mkdir();
        createRulesFile();
        createTraceFile();
        createKBFile();
        createProjectFile();
    }


    /**
     * Crea il file delle regole, a partire dalla relativa lista
     */
    private void createRulesFile() {
        listToFile(rules, projectName + "/" + rulesFile);
    }

    /**
     * Crea il file della traccia, a partire dalla relativa lista
     */
    private void createTraceFile() {
        listToFile(trace, projectName + "/" + traceFile);
    }

    
    /**
     * Crea il file kb.pl, secondo il relativo modello
     */
    private void createKBFile() {
        try {
            FileWriter fstream = new FileWriter(projectName + "/kb.pl");
            BufferedWriter out = new BufferedWriter(fstream);

            out.write("society_goal.");
            out.write("\n");
            out.write("%%%% TEORIA PROLOG %%%%");

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }

    }

    /**
     * Crea il file project.pl, secondo il relativo modello
     */
    private void createProjectFile() {
        try {
            FileWriter fstream = new FileWriter(projectName + "/project.pl");
            BufferedWriter out = new BufferedWriter(fstream);

            out.write(":- dynamic ics_file/1, sokb_file/1, history_file/1, required_option/2.\n");
            out.write("\n");
            out.write("history_file('" + traceFile + "').\n");
            out.write("ics_file('" + rulesFile + "').\n");
            out.write("sokb_file('kb.pl').\n");
            out.write("\n");
            out.write("%%%%%%%%%%%%%%%%%%%%%%% Constant Part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
            out.write("build_prj(Path):-");
            out.write("    findall(F,ics_file(F),ICS_files), append_path(Path,ICS_files,IcsPathFiles),\n");
            out.write("    translate_ics_files(IcsPathFiles,'./ics.pl'),\n");
            out.write("    findall(F,history_file(F),Hist_files),  append_path(Path,Hist_files,HistPathFiles),\n");
            out.write("   translate_histories(HistPathFiles,'./history.pl'),\n");
            out.write("    findall(F,sokb_file(F),Sokb_files),     append_path(Path,Sokb_files,SokbPathFiles),\n");
            out.write("    convert_sokb(SokbPathFiles,'./sokb.pl'),\n");
            out.write("    compile(sokb), compile(history), compile(ics),\n");
            out.write("    findall([O,V],required_option(O,V),LOptions),\n");
            out.write("    set_options(LOptions).\n");
            out.write("\n");
            out.write("% Default:\n");
            out.write("run(_):- run.\n");
            out.write("run_open(_):- run_no_close.\n");
            out.write("run_closed(_):- run.\n");

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    /**
     * Metodo di utilità che permette di scrivere il contenuto di una lista
     * all'interno di un file
     * 
     * @param list
     * @param fileName
     */
    private void listToFile(List<String> list, String fileName) {
        try {
            FileWriter fstream = new FileWriter(fileName);
            BufferedWriter out = new BufferedWriter(fstream);

            for (String el : list) {
                out.write(el + "\n");
                out.write("\n");
            }

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
