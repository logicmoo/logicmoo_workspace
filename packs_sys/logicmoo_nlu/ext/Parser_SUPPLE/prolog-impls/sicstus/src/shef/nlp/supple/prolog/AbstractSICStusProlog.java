package shef.nlp.supple.prolog;

import java.io.File;
import java.io.IOException;

import gate.creole.ExecutionException;

import gate.util.ProcessManager;

/**
 * Abstract superclass for prolog wrappers for SUPPLE running on SICStus prolog
 * (http://www.sics.se).  The precise syntax of the command line varies between
 * SICStus versions, so this class is not used directly, rather you should
 * choose the appropriate subclass (SICStusProlog3 or SICStusProlog4) to match
 * your SICStus version.
 */
public abstract class AbstractSICStusProlog extends Prolog
{
  private File parserFile = null;

  /**
   * ProcessManager to handle running external sicstus processes.
   */
  private ProcessManager processManager = new ProcessManager();

  /**
   * Initialise this Prolog wrapper, passing the path to the saved state.
   *
   * @return <code>true</code> if the supplied file exists, <code>false</code>
   * otherwise.
   */
  public boolean init(File f) {
    parserFile = f;

    return (parserFile != null && parserFile.exists());
  }

  /**
   * Construct the command line to pass to Runtime.exec.
   */
  protected abstract String[] getCommandLineArgs(File parserFile, File in, File out);

  /**
   * Returns the command used to run sicstus.  The default varies by platform,
   * and can be overridden by a system property.
   */
  protected String getSicstusCommand() {
    boolean windows = false;
    if(System.getProperty("os.name").toLowerCase().startsWith("windows"))
    {
      windows = true;
    }

    // find the prolog executable.  The default value varies by platform
    if(windows) {
      return System.getProperty("supple.sicstus.executable", "sicstus.exe");
    }
    else {
      return System.getProperty("supple.sicstus.executable", "sicstus");
    }
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
    String[] commandArgs = getCommandLineArgs(parserFile, in, out);

    if(debugMode) {
      System.err.println("Executing SICStus prolog with the command line:");
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
          throw new ExecutionException("SICStus Prolog exit code: " + exitCode);
        }
        else {
          throw new ExecutionException("SICStus Prolog exited with error code "
              + exitCode + ".  Rerun with debug = true to see the console "
              + "output, which may help show the reasons for this error.");
        }
      }
    }
    catch(IOException e) {
      ExecutionException ee = new ExecutionException("I/O error executing "
          + commandArgs[0] + ".  If this is not the correct path to SICStus "
          + "prolog on your machine, please set the supple.sicstus.executable "
          + "system property (see the user guide).");
      ee.initCause(e);
      throw ee;
    }
  }
}
