// package survivalgame.imperial.ac.uk; ??
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Toolkit;
import java.util.ArrayList;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;

import javax.swing.JFrame;
import java.util.Scanner;

/** The display of a LPS game simulation, a ICL student project; requires LPS and a "Survival Melee" LPS program file to run. 
This display approach is somehow fragile because it depends on the LPS interpreter (state) output to have a precise format 
To run see comments in main()*/

public class SurvivalPanel extends JPanel {
	/* Holds all the information gained from running Survival Melee */
	public ArrayList<String> lpsOutput = new ArrayList<String>(); 
	/** Holds current cycle number */
	public int n = 1; 
	/** Current output line being parsed */
	private int lineNumber = -1;
	
	public SurvivalPanel(){
		/* Set up button */
		this.setLayout(null);
		final JLabel cycle = new JLabel("Cycle "+n,JLabel.CENTER);
		cycle.setBounds(825, 60, 70, 20);
		add(cycle);
		JButton next = new JButton("Next");
		next.addActionListener(new ActionListener(){
			/* When button is clicked, increment n in the panel (increase cycle number) */
			public void actionPerformed(ActionEvent arg0) {
				n++;
				cycle.setText("Cycle "+n);
				repaint();
			}
		});
		/* Button location and size */
		next.setBounds(825, 100, 70, 20);
		add(next);
		JButton previous = new JButton("Previous");
		previous.addActionListener(new ActionListener(){
			/* When button is clicked, decrement n in the panel (decrease cycle number) */
			public void actionPerformed(ActionEvent arg0) {
				n--;
				cycle.setText("Cycle "+n);
				repaint();
			}
		});
		previous.setBounds(825, 140, 70, 20);
		add(previous);
		
	
	}
		
	public void paint(Graphics grap) {
		super.paint(grap);
		int x = 0;
		int y = 0;
		// Set gridlines
		for(int i = 0; i < 20; i++) {
			for(int j = 0;j < 20; j++) {
				grap.drawRect(i*40, j*40, 40, 40);
			}
		}
		
		// Gets database for current cycle
		lineNumber = lpsOutput.indexOf("* Cycle "+String.valueOf(n)+" *");
		if (lineNumber==-1){
			Toolkit.getDefaultToolkit().beep();
			return;
		}
		lineNumber += 13; 
		if (lineNumber>=lpsOutput.size()){
			Toolkit.getDefaultToolkit().beep();
			return;
		}
		String line = lpsOutput.get(lineNumber);
		System.out.println("Cycle:"+n);
		System.out.println(line);
		
		/* Draw position of units. position(Unit, D, X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf("position(", count) > 0) {
				int i = line.indexOf("position(", count);
				int length = "position(".length();
				
				/* Get name */
				i = i + length;
				int j = line.indexOf(",", i);
				String name = line.substring(i, j);
				
				/* Get direction */
				j++;
				int k = line.indexOf(",", j);
				String direction = line.substring(j, k);
				
				/* Get x */
				k++;
				int l = line.indexOf(",", k);
				x = Integer.parseInt(line.substring(k, l));
				
				/* Get y */
				l++;
				int m = line.indexOf(")", k);
				y = Integer.parseInt(line.substring(l, m));
				count=i; // Move count along so next instance is found
				
				/* Draw appropriate colour to panel depending on unit */
				if (name.equals("amanda")) {
					grap.setColor(Color.MAGENTA);
					grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
					grap.setColor(Color.BLACK);
				} else if (name.equals("katherine")) { //Katherine is purple
					grap.setColor(Color.getHSBColor((float) 0.79, (float) 1.0, (float) 0.70));
					grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
					grap.setColor(Color.WHITE);
				} else if (name.equals("peter")) {
					grap.setColor(Color.RED);
					grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
					grap.setColor(Color.BLACK);
				} else if (name.equals("tom")) {
					grap.setColor(Color.BLUE);
					grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
					grap.setColor(Color.WHITE);
				} else {
					grap.setColor(Color.CYAN);
					grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
					grap.setColor(Color.BLACK);
				}
			
				/* Draw appropriate symbol to panel depending on direction */
				if (direction.equals("north")) {
					grap.drawString("^", ((x-1)*40)+20, ((y-1)*40)+20);
				} else if (direction.equals("east")) {
					grap.drawString(">", ((x-1)*40)+20, ((y-1)*40)+20);
				} else if (direction.equals("south")) {
					grap.drawString("V", ((x-1)*40)+20, ((y-1)*40)+20);
				} else {
					grap.drawString("<", ((x-1)*40)+20, ((y-1)*40)+20);
				}
				
			} else {
				break;
			}
		}
		
		/* Draw position of animals. animal(Type, H, D, X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf("animal(", count) > 0) {
				int i = line.indexOf("animal(", count);
				int length = "animal(".length();
				
				/* Get animal type */
				i = i + length;
				int j = line.indexOf(",", i);
				String animalType = line.substring(i, j);
				
				/* Skip animal health */
				j++;
				int k = line.indexOf(",", j);
				
				/* Get direction */
				k++;
				int l = line.indexOf(",", k);
				String direction = line.substring(k, l);
				
				/* Get x */
				l++;
				int m = line.indexOf(",", l);
				x = Integer.parseInt(line.substring(l, m));
				
				/* Get y */
				m++;	
				int o = line.indexOf(")", m);
				y = Integer.parseInt(line.substring(m, o));
				
				count=i;// Move count along so next instance is found
				
				/* Draw appropriate colour to panel depending on animal */
				if (animalType.equals("chicken")) {
					grap.setColor(Color.WHITE);
					
				} else if (animalType.equals("cow")) {
					grap.setColor(Color.ORANGE);
					
				} else {
					grap.setColor(Color.GRAY);
					
				}
				
				grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
				grap.setColor(Color.BLACK);
				
				/* Draw appropriate symbol to panel depending on direction */
				if (direction.equals("north")) {
					grap.drawString("^", ((x-1)*40)+20, ((y-1)*40)+20);
				} else if (direction.equals("east")) {
					grap.drawString(">", ((x-1)*40)+20, ((y-1)*40)+20);
				} else if (direction.equals("south")) {
					grap.drawString("V", ((x-1)*40)+20, ((y-1)*40)+20);
				} else {
					grap.drawString("<", ((x-1)*40)+20, ((y-1)*40)+20);
				}
				
			} else {
				break;
			}
		}
		
		
		/* Draw position of trees. tree(X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf("tree(", count) > 0) {
				int i = line.indexOf("tree(", count);
				int length = "tree(".length();
				
				/* Get x */
				i = i + length;
				int j = line.indexOf(",", i);
				x = Integer.parseInt(line.substring(i, j));
				
				/* Get y */
				j++;
				int k = line.indexOf(")", j);
				y = Integer.parseInt(line.substring(j, k));
				count=i; // Move count along so next instance is found
				grap.setColor(Color.GREEN);
				grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
			} else {
				break;
			}
			
		}
		
		/* Draw position of wood. wood(X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf(",wood(", count) > 0) {
				int i = line.indexOf(",wood(", count);
				int length = ",wood(".length();
				
				/* Get x */
				i = i + length;
				int j = line.indexOf(",", i);
				x = Integer.parseInt(line.substring(i, j));
				
				/* Get y */
				j++;
				int k = line.indexOf(")", j);
				y = Integer.parseInt(line.substring(j, k));
				
				count=i; // Move count along so next instance is found
				grap.setColor(Color.getHSBColor((float) 0.11, (float) 0.92, (float) 0.48)); //wood is brown
				grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
				
			} else {
				break;
			}
			
		}
		
		/* Draw position of shelters. shelter(Unit, H, X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf(",shelter(", count) > 0) {
				int i = line.indexOf(",shelter(", count);
				int length = ",shelter(".length();
				
				/* Get shelter owner */
				i = i + length;
				int j = line.indexOf(",", i);
				String name = line.substring(i, j);
				
				/* Skip health */
				j++;
				int k = line.indexOf(",", j);
				
				/* Get x */
				k++;
				int l = line.indexOf(",", k);
				x = Integer.parseInt(line.substring(k, l));
				
				/* Get y */
				l++;
				int m = line.indexOf(")", l);
				y = Integer.parseInt(line.substring(l, m));
				
				count=i; // Move count along so next instance is found
				
				grap.setColor(Color.BLACK);
				grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
				
				/* Draw appropriate colour to panel depending on shelter owner */
				if (name.equals("amanda")) {
					grap.setColor(Color.MAGENTA);
				} else if (name.equals("katherine")) {//Katherine is purple
					grap.setColor(Color.getHSBColor((float) 0.79, (float) 1.0, (float) 0.70));
				} else if (name.equals("peter")) {
					grap.setColor(Color.RED);
				} else if (name.equals("tom")) {
					grap.setColor(Color.BLUE);
				} else {
					grap.setColor(Color.CYAN);
				}
				grap.fillRect(((x-1)*40)+15, ((y-1)*40)+15, 10, 10);
				
			} else {
				break;
			}
		}
		
		/* Draw position of food. food(A, B, C, X, Y) */
		for (int count = 0; count < line.length(); count++) {
			if(line.indexOf(",food(", count) > 0) {
				int i = line.indexOf(",food(", count);
				int length = ",food(".length();
				
				/* Skip 1 f.p., A */
				i = i + length;
				int j = line.indexOf(",", i);
				
				/* Skip 3 f.p., B */
				j++;
				int k = line.indexOf(",", j);
				
				/* Skip 5 f.p., C */
				k++;
				int l = line.indexOf(",", k);
				
				/* Get x */
				l++;
				int m = line.indexOf(",", l);
				x = Integer.parseInt(line.substring(l, m));
				
				/* Get y */
				m++;
				int o = line.indexOf(")", m);
				y = Integer.parseInt(line.substring(m, o));
				
				count=i; // Move count along so next instance is found
				
				grap.setColor(Color.PINK);
				grap.fillRect((x-1)*40, (y-1)*40, 40, 40);
				
			} else {
				break;
			}
		}
		
		
		
	}
	
	/** 
	Simply compile with javac SurvivalPanel.java
	Then execute with java SurvivalPanel 
	This requires arguments: XSB_PATH, LPS_HOME, GAME_PROGRAM
	For example 
		java SurvivalPanel "/Users/mc/subversion/XSB/bin/xsb" "../../Wei-engine/interpreter.P" "Survival Melee.pl"
	If all these 3 arguments are not given the program will ask them to the user.
	*/
	public static void main(String[] args) {
		String xsb, lps_interpreter, lps_program;
		
		if (args.length!=3) {
			Scanner user_input = new Scanner(System.in);
			/* Get XSB path */
			System.out.println("Please type in the full file path to run XSB:\n");
			xsb = user_input.nextLine();
		
			/* Get path to LPS */
			System.out.println("Now the file path of the LPS system interpreter.P:\n");
			lps_interpreter = user_input.nextLine();
			
			/* Get path to LPS game program */
			System.out.println("..and now the file path of the LPS game program:\n");
			lps_program = user_input.nextLine();			
		} else {
			xsb = args[0];
			lps_interpreter = args[1];
			lps_program = args[2];
		}
		ProcessBuilder proBuild = new ProcessBuilder(xsb);
		//proBuild.directory(new File(directory));
		Process pro = null;
		try{
				pro = proBuild.start();
				OutputStream outStr = pro.getOutputStream();
				// Consult Wei's interpreter:
				outStr.write(("consult('"+lps_interpreter+"').\n").getBytes()); 
				// Run Survival Melee:
				outStr.write(("go('"+lps_program+"', [ultra_verbose, priority]).\n").getBytes()); // ultra_verbose needed to avoid '...' abstractions in output
				outStr.write("halt.\n".getBytes()); // End
				outStr.flush();
		} catch (IOException ioException) {
			ioException.printStackTrace();
		}
		
		/* Set up panel */
		JFrame frame = new JFrame();
		frame.setSize(900, 823);
		SurvivalPanel panel = new SurvivalPanel();
		frame.add(panel);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		/* Read information gained from running Survival Melee */
		BufferedReader buffread = new BufferedReader(new InputStreamReader(pro.getInputStream()));
		String line = null;
		try {
			line = buffread.readLine();
		} catch (IOException ioException) {
			ioException.printStackTrace();
		}
		
		/* If still information to be had, add to ArrayList */
		while(line != null) {
			panel.lpsOutput.add(line);
			try {
				line = buffread.readLine();
			} catch (IOException ioException) {
				ioException.printStackTrace();
			}
		}
		frame.setVisible(true);
		
	}
	
}

