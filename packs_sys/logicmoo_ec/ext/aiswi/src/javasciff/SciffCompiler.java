package javasciff;

import jpl.Query;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class SciffCompiler implements Runnable {

    private String sciffPath;

    /**
     *
     * @param sciffPath
     */
    public SciffCompiler(String sciffPath) {
        this.sciffPath = sciffPath;
    }

    /**
     * Esecuzione del comando prolog compile(sciff).
     */
    public void run() {
        Query.hasSolution("set_prolog_flag(optimise,true)");
        Query.hasSolution("compile(" + sciffPath + "sciff)");
    }





}
