package TestGUI;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import se.sics.prologbeans.*;
import java.io.*;
 

public class TestGUI implements ActionListener {

	private JFrame frame;
	private JTextArea text = new JTextArea(20, 40);
	private JTextField input = new JTextField(36);
	private JButton evaluate = new JButton("Evaluate");
	private PrologSession session = new PrologSession();
	private ProgressMonitor monitor;
	private String record = null;
	private Integer recCount = 1;
	private String done = "";
	
	

	public TestGUI() {
		session.setTimeout(20000000);
		frame = new JFrame("Prolog Evaluator");
		Container panel = frame.getContentPane();
		panel.add(new JScrollPane(text), BorderLayout.CENTER);
		JPanel inputPanel = new JPanel(new BorderLayout());
		inputPanel.add(input, BorderLayout.CENTER);
		inputPanel.add(evaluate, BorderLayout.EAST);
		panel.add(inputPanel, BorderLayout. SOUTH);
		text.setEditable(false);
		evaluate.addActionListener(this);
		input.addActionListener(this);
		
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

	public void actionPerformed(ActionEvent event)
	{
		new FileOutputStreamTest().start(); 
		new ReadOneFile().start();
		// EventQueue.invokeLater(new Runnable () {
		//	 public void run ()	 {
		// 	 System.out.println("done");
		//	 String status = "OK";
		//	 JOptionPane.showMessageDialog(null,"Done, status = " + status,"Finished",JOptionPane.INFORMATION_MESSAGE);
		//	 }
		// });
	}
		
		//new handleTheCommand().start();
		//try
		//{
		//openFile ();
		//}
		//catch (IOException exception)
		//{
		//	exception.printStackTrace();
		//}
	
	
	public String handleCommand(String s) {
		try {
			Bindings bindings = new Bindings().bind("E", "[100, " + s + "].");
			QueryAnswer answer = session.executeQuery("evaluate(E, R)", bindings);
			Term result = answer.getValue("R");
			if (result != null) {
				return result.toString();
			} else {
				return "Error: " + answer.getError() + '\n';
			}
		} catch (Exception e) {
			text.append("Error when querying Prolog Server: " + e.getMessage() + '\n');
			e.printStackTrace();
			return null;
		}
	}
	
	public class handleTheCommand extends Thread {
		handleTheCommand() {}
			public void run() {
			String message = "";
			String s = input.getText();
			String result = handleCommand(s);
			if (result != null) {
				text.append(result + '\n');
				input.setText("");
				message = "OK";
				}  
			else {
				message = "Error";
			}
			}
		}
	
 
	/*This first example uses the most basic (I think) of all the output streams, the FileOutputStream */
	
	public class FileOutputStreamTest extends Thread{
		FileOutputStreamTest()  {}
		public void run ()  {
	try {
	FileOutputStream out = new FileOutputStream("c:/java/regulus/java/myFile.txt");
    PrintStream p; // declare a print stream object
    // Connect print stream to the output stream
    p = new PrintStream( out );
    for (int i=0; i < 100; i++){
    	p.println ("This is a testrecord written to a file");
    	System.out.println("i "+ i +" This is a testrecord written to a file");
    	try {
			sleep(1000);
		}
		catch (InterruptedException e){
			
		}
	 }
    p.close();
    }
	  catch (Exception e)
      {
              System.err.println ("Error writing to file");
      }
}
}
	
	public class ReadOneFile extends Thread {
		public ReadOneFile() {}
		public void run() {
		final ProgressMonitor monitor;
		 try {
			 
		FileInputStream filein = new FileInputStream("c:/java/regulus/java/myFile.txt");
		ProgressMonitorInputStream progressIn = new  ProgressMonitorInputStream (null,"Loading",filein);
		 InputStreamReader inReader	 = new InputStreamReader(progressIn);
		 final BufferedReader in = new BufferedReader(inReader);
		 monitor = new ProgressMonitor(null,"monitoring textfile",null,0,100);
		
			 while ((record = in.readLine()) !=null)
			 {
				recCount++;
				monitor.setProgress(recCount);
				text.append(record);
			 	text.append("\n");
				System.out.println("record "+record);
				try {
					sleep(1000);
				}
				catch (InterruptedException e){
					
				}
			 }
			 done = "Done";
			 in.close();
			 EventQueue.invokeLater(new Runnable () {
				 public void run ()	 {
			 	 System.out.println("done");
				 String status = "OK";
				 JOptionPane.showMessageDialog(null,"Done, status = " + status,"Finished",JOptionPane.INFORMATION_MESSAGE);
				 }
			 });
		 }
			 catch (IOException e)  {
			 }	
			 System.out.println("Done");
			 }
		 }
		
	public static void main(String[] args) {
		new TestGUI();
	}
	}

