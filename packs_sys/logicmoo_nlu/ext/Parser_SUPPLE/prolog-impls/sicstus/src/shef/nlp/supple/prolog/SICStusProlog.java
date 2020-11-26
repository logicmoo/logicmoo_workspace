package shef.nlp.supple.prolog;

import java.io.File;

/**
 * Deprecated class that behaves as SICStusProlog3 but is left for backwards
 * compatibility.
 *
 * @deprecated use SICStusProlog3 or SICStusProlog4 instead.
 */
public class SICStusProlog extends SICStusProlog3
{
  /**
   * Prints a warning, then delegates to SICStusProlog3.
   */
  public boolean init(File f) {
    System.err.println(this.getClass().getName() + " is deprecated, use "
        + "SICStusProlog3 or SICStusProlog4 instead.");
    return super.init(f);
  }
}
