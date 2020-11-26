package javasciff;

import gamegui.ChessBoard;
import lib.Move;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.List;
import java.util.Vector;
import lib.FileManager;
import lib.WorldDescription;

/**
 * Rappresenta un progetto SCIFF.
 *
 * Consente di creare un progetto tramite Java, specificando le regole e la traccia.
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class SProject {

    /**
     * Nome del file che conterrà le regole
     */
    public static final String rulesFile = "rules.txt";

    /**
     * Nome del file che conterrà la traccia
     */
    public static final String traceFile = "trace.txt";

    /**
     * Nome del file che conterrà la base di conoscenza prolog
     */
    public static final String kbFile = "kb.pl";

    /**
     * Nome del file di configurazione del progetto
     */
    public static final String projectFile = "project.pl";

    /**
     * Lista di regole SCIFF
     */
    public String rules;

    /**
     * Lista contente la traccia, cioè le mosse effettuate
     */
    public List<Move> trace;

    /**
     * Base di conoscenz prolog
     */
    public String kb;

    /**
     * 
     */
    public String project;

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
    public SProject(String projectName) {
        this.projectName = projectName;
        trace = new Vector<Move>();
        kb = "";
        rules = "";
        // rules = readFileAsString(projectTemplate + "/" + rulesFile);
        //project = FileManager.readFileAsString(projectTemplate + "/" + projectFile);
    }

    
    /**
     * Memorizza il progetto su filesystem per permetterne l'esecuzione tramite SCIFF.
     *
     * Crea la cartella che conterrà il progetto e crea tutti i file necessari.
     */
    public void generateProject() {
        (new File(projectName)).mkdir();
        generateRulesFile();
        generateTraceFile();
        generateKBFile();
        generateProjectFile();
    }


    /**
     * Crea il file delle regole, a partire dalla relativa lista
     */
    private void generateRulesFile() {
        FileManager.writeStringToFile(rules, projectName + "/" + rulesFile);
    }

    /**
     * Crea il file della traccia, a partire dalla relativa lista
     */
    private void generateTraceFile() {

        try {
            FileWriter fstream = new FileWriter(projectName + "/" + traceFile);
            BufferedWriter out = new BufferedWriter(fstream);

            out.write("hap(start,0.0).\n");
            for (Object el : trace) {
                out.write(el.toString() + "\n");
            }

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    
    /**
     * Crea il file kb.pl, secondo il relativo modello
     */
    private void generateKBFile() {
         FileManager.writeStringToFile("society_goal.\n%%%% TEORIA PROLOG %%%%\n\n" + 
                 kb + "\n\n%%%% DESCIZIONE MONDO %%%%\n\n" +   WorldDescription.generateWorldDescription(ChessBoard.getInstance().getCells()), projectName + "/" + kbFile);
    }

    /**
     * Crea il file project.pl, secondo il relativo modello
     */
    private void generateProjectFile() {

        // dovrebbe essere
        // writeStringToFile(project, projectName + "/" + projectFile);

        try {
            FileWriter fstream = new FileWriter(projectName + "/" + projectFile);
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
            out.write("game(L) :- run, findall_constraints(h(_,_,_), L).\n");

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
    

}
