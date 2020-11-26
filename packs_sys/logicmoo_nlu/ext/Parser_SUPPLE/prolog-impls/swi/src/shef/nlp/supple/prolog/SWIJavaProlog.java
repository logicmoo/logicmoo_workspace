package shef.nlp.supple.prolog;

import java.io.File;
import gate.creole.ExecutionException;

import jpl.JPL;
import jpl.Query;

/**
 * Prolog implementation using the JPL Java interface to SWI Prolog.
 */
public class SWIJavaProlog extends Prolog
{
	private File parserFile = null;

	public boolean init(File f)
	{
		parserFile = f;

		if (parserFile != null && parserFile.exists())
		{
			String[] args = new String[]{"pl","-x",parserFile.getAbsolutePath(),"-g","true"};

			JPL.init(args);

			return JPL.getActualInitArgs() != null;
		}

		return false;

	}

	public void parse(File in, File out, boolean debugMode)
                  throws ExecutionException {
		String oFile = out.getAbsolutePath();
		String iFile = in.getAbsolutePath();

		if(System.getProperty("os.name").toLowerCase().startsWith("windows"))
		{
			oFile = oFile.replace('\\','/');
			iFile = iFile.replace('\\','/');
		}

		Query query = new Query("parse(['-o','"+oFile+"','"+iFile+"'])");

		if(!query.hasSolution()) {
      throw new ExecutionException("SWIJavaProlog: parser failed");
    }
	}
}
