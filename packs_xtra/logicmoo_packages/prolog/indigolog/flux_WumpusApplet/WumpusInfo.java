import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class WumpusInfo extends JPanel{
	public JTextArea text;
	public JTextArea log;
	public JButton startButton;

	WumpusInfo(Object parent, Dimension d){
		super(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		
		//set up the info area
		text = new JTextArea(20,15);
      	text.setBackground(Color.white);
      	text.setEditable(false);
		JPanel infoPanel = new JPanel();
      	infoPanel.setPreferredSize(new Dimension(d.width, d.height-125));
		infoPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createTitledBorder("Action history & Sensing results"),
                        BorderFactory.createEmptyBorder(5,5,5,5)));
      	infoPanel.add(new JScrollPane(text));
      	this.add(infoPanel,c);

		//set up the log area
		log = new JTextArea(6,15);
      	log.setBackground(Color.white);
      	log.setEditable(false);
		JPanel logPanel = new JPanel();
		logPanel.setPreferredSize(new Dimension(d.width, 150));
		logPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createTitledBorder("Log"),
                        BorderFactory.createEmptyBorder(5,5,5,5)));
        logPanel.add(new JScrollPane(log));
      	c.gridx = 0;
      	c.gridy = 1;      	
      	this.add(logPanel,c);
      	     	
     	//buttons?
     	startButton = new JButton("Pause");
      	startButton.setPreferredSize(new Dimension(80,25));
      	startButton.addActionListener((ActionListener)parent);
      	startButton.setEnabled(false);
      	c.gridx = 0;
      	c.gridy = 2;      	
      	this.add(startButton,c);
    }
    
    public void appendLog(String s){
    	log.append(s);
    	log.setCaretPosition(log.getText().length());
    }
    
    public void appendInfo(String s){
    	text.append(s);
    	text.setCaretPosition(text.getText().length());
    }
    
    public void reset(){
    	text.setText("");
    	log.setText("");
    }
}
