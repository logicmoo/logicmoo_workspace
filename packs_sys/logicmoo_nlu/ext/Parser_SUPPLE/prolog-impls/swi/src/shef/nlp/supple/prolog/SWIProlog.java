package shef.nlp.supple.prolog;

import java.io.File;
import java.io.IOException;

import gate.creole.ExecutionException;

import gate.util.ProcessManager;

/**
 * Prolog wrapper for SUPPLE running on SWI Prolog (http://www.swi-prolog.org).
 */
public class SWIProlog extends Prolog
{
  private File parserFile = null;
  
  /**
   * ProcessManager to handle running external SWI processes.
   */
  private ProcessManager processManager = new ProcessManager();

  /**
   * Initialise this Prolog wrapper, passing the path to the saved state.
   *
   * @return <code>true</code> if the supplied file exists, <code>false</code>
   * otherwise.
   */
  public boolean init(File f)
  {
    parserFile = f;

    return (parserFile != null && parserFile.exists());
  }

  /**
   * Run the parser, taking input from and sending output to the specified
   * temporary files.
   *
   * @param in temporary file containing input for the parser
   * @param out temporary file to receive output from the parser
   * @param debugMode should we write debugging information to the console?
   */
  public void parse(File in, File out, boolean debugMode)
                  throws ExecutionException {
    boolean windows = false;
    if(System.getProperty("os.name").toLowerCase().startsWith("windows"))
    {
      windows = true;
    }

    String[] commandArgs;
    commandArgs = new String[7];

    // find the prolog executable.  The default value varies by platform
    if(windows) {
      commandArgs[0] = System.getProperty("supple.swi.executable", "plcon.exe");
    }
    else {
      commandArgs[0] = System.getProperty("supple.swi.executable", "swipl");
    }

    commandArgs[1] = "-x";
    commandArgs[2] = parserFile.getAbsolutePath();
    commandArgs[3] = "--";

    // add command arguments
    commandArgs[4] = "-o";
    commandArgs[5] = out.getAbsolutePath();
    commandArgs[6] = in.getAbsolutePath();

    if(debugMode) {
      System.err.println("Executing SWI prolog with the command line:");
      System.err.println();
      for(int i = 0; i < commandArgs.length; i++) {
        System.err.println(commandArgs[i]);
      }
      System.err.println();
    }

    try {
      int exitCode = processManager.runProcess(commandArgs, debugMode);

      if(exitCode != 0) {
        String message = null;
        if(debugMode) {
          throw new ExecutionException("SWI Prolog exit code: " + exitCode);
        }
        else {
          throw new ExecutionException("SWI Prolog exited with error code "
              + exitCode + ".  Rerun with debug = true to see the console "
              + "output, which may help show the reasons for this error.");
        }
      }
    }
    catch(IOException e) {
      ExecutionException ee = new ExecutionException("I/O error executing "
          + commandArgs[0] + ".  If this is not the correct name for SWI "
          + "prolog on your machine, please set the supple.swi.executable "
          + "system property (see the user guide).");
      ee.initCause(e);
      throw ee;
    }
  }
}
