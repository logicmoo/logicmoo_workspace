import java.lang.*;
import java.io.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


public class WumpusWorld extends JPanel implements ActionListener{
	//wumpus world stuff
	public int robotX=-1;
	public int robotY=-1;
	int maxX, maxY;
	
	//gui stuff
	JLabel labels[][];
	boolean editMode = false;
	
	public WumpusWorld(Object parent, int maxX, int maxY, Dimension dim){
		this.maxX = maxX;
		this.maxY = maxY;
		int width = dim.width;
		int height = dim.height;
		
		//set up the border and size
		this.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createTitledBorder("Wumpus World"),
                        BorderFactory.createEmptyBorder(5,5,5,5)));
		this.setPreferredSize(dim);
		
		//set up cell properties
		int cellWidth = (width-10)/maxY;
		int cellHeight = (width-10)/maxX;
		int cell = Math.min(cellWidth, cellHeight);
		Dimension d = new Dimension(cell, cell);
		Font f = new Font(null,Font.BOLD,cell/5);
		
		//set up the grid
	  	labels = new JLabel[maxX][maxY];
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		for(int i=0; i<maxX; i++)
			for(int j=0; j<maxY; j++){
				c.gridx = i;
				c.gridy = maxY-j;
				//String id = "(" + i + "," + j + ")";
				String id = "";
				labels[i][j] = new JLabel(id,SwingConstants.CENTER); 
				labels[i][j].setFont(f);
				labels[i][j].setOpaque(true);
				labels[i][j].setBackground(Color.white);
				labels[i][j].setPreferredSize(d);
				labels[i][j].setBorder(BorderFactory.createLineBorder(Color.black));
				add(labels[i][j], c);
			}

		//set up edit mode
		editMode = false;
	}
	
	public void setEditMode(boolean b){
		editMode = b;
	}
	
	public boolean setCell(int x, int y, String data){
		if (x<0 || x>labels.length || y<0 || y>labels[0].length)
			return false;
		else{
			labels[x][y].setText(data);
			return true;
		}
	}
	
	public void setPit(int x, int y){
		setCell(x,y,"Pit");
	}

	public void setGold(int x, int y){
		setCell(x,y,"Gold");
	}
	
	public void setWumpus(int x, int y){
		setCell(x,y,"Wumpus");
	}
	
	public void moveRobot(int newRobotX, int newRobotY){
		if(robotX==newRobotX && robotY==newRobotY)
			return;
		if(setCell(newRobotX,newRobotY,"Robot")){
			labels[newRobotX][newRobotY].setBackground(Color.pink);
			setCell(robotX,robotY,"");
			robotX=newRobotX;
			robotY=newRobotY;
		}
	}
	
	public void reset(){
		for(int i=0; i<maxX; i++)
			for(int j=0; j<maxY; j++){
				labels[i][j].setText("");
				labels[i][j].setBackground(Color.white);
			}
	}
	
	public void actionPerformed(ActionEvent e){
	}
}

