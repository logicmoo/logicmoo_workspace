package shef.nlp.supple.prolog;

import java.io.File;

/**
 * Prolog wrapper for SUPPLE running on SICStus prolog version 4.
 */
public class SICStusProlog4 extends AbstractSICStusProlog {

  /**
   * Returns the command line suitable for running SUPPLE in SICStus 4.
   */
  protected String[] getCommandLineArgs(File parser, File in, File out) {
    return new String[] {
      getSicstusCommand(),
      "-r",
      parser.getAbsolutePath(),
      "-a",
      "-o",
      out.getAbsolutePath(),
      in.getAbsolutePath()
    };
  }
}
