package org.cs3.plbenchmarks;

import java.awt.EventQueue;
import java.io.File;
import java.util.List;

import org.cs3.plbenchmarks.data.Configuration;
import org.cs3.plbenchmarks.gui.PbtGui;
import org.cs3.plbenchmarks.utils.IOUtils;

public class RunPBT {

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		
		if (args.length >= 1) {
			File f = new File(args[0]);
			if (f.isFile()) {
				List<Configuration> configs = IOUtils.loadConfigsFromFile(f);
				PbtController controller = new PbtController();

				for (Configuration conf : configs) {
					conf.setSaveFiles(false); // TODO: just for demo reasons, add this option to the gui
					PbtRunner runner = new PbtRunner(controller, conf, null);
					System.out.println("run \"" + conf.getName() + "\"");
					runner.run();
					System.out.println("finished \"" + conf.getName() + "\"");
				}
				
			} else {
				System.err.println("first parameter must be a valid configuration file");
			}
		} else {
			// start in GUI mode
			EventQueue.invokeLater(new Runnable() {
				@Override
				public void run() {
					try {
						PbtGui frame = new PbtGui();
						frame.setVisible(true);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			});
		}
	}
}
