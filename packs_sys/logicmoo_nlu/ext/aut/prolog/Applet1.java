import java.util.Vector;
import java.awt.*;
import java.applet.*;
//import java.lang.reflect.Field;
//import java.lang.*;

/**
 * This class reads PARAM tags from its HTML host page and sets
 * the color and label properties of the applet. Program execution
 * begins with the init() method. 
 */
public class Applet1 extends Applet
{
	/**
	 * The entry point for the applet.* 
	 */
	
//Point class redefinition for Netscape's propely work.
	public class Point extends Object {
		public Point() {};
		public Point(int x1,int y1) {x=x1; y=y1;};
		public float x;
		public float y; }
	
//----------------------------------------------------	
	
	public float min_x=0,max_x=0,min_y=0,max_y=0;
//vector of all data
	private Vector Drawdata = new Vector();	
//koef to fit ficture for best size
	float koef;
//data entry--------------------------------
	public void DataEntry(String parX, String parY){
		int posX=0;  int posbX=0;
		int posY=0;  int posbY=0;		
		String buffer;
		float flX; float flY;
		while(posX < parX.length() && posY < parY.length()){
			posX=parX.indexOf(' ', posbX);
			if(posX==-1) posX=parX.length();
			buffer=parX.substring(posbX, posX);
			try{flX= Float.valueOf(buffer).floatValue();}
			catch(NumberFormatException NFE){flX=0;}
			posbX=posX+1;
			
			posY=parY.indexOf(' ', posbY);
			if(posY==-1) posY=parY.length();
			buffer=parY.substring(posbY, posY);
			try{flY= Float.valueOf(buffer).floatValue();}
			catch(NumberFormatException NFE){flY=0;}
			posbY=posY+1;
			

		Point P =new Point();
		P.x=Math.round(flX);
		P.y=Math.round(flY);		
		Drawdata.addElement(P); 
	  //minimax procedure
		if(P.x>max_x) max_x=P.x;
		else if(P.x<min_x) min_x=P.x;
		if(P.y>max_y) max_y=P.y;
		else if(P.y<min_y) min_y=P.y;
	  }

	  float kx=190/(max_x-min_x);
	  float ky=190/(max_y-min_y);
	  if(kx>ky) koef=ky;
	  else koef=kx;
	}
//-------------------------------------------------	
	public void init()
	{
		initForm();
	    String par=getParameter("XvalA");
		String parX, parY;
		int delim=par.indexOf('&');
		parX=par.substring(0,delim);
		parY=par.substring(delim+1,par.length());
		DataEntry(parX, parY);
	}
	
	
	private int DrawdataSize;
	
  public void paint(Graphics g)
	{
//-------------------------	  
	  DrawdataSize=Drawdata.size();
	  Object obj = new Object();
	  Point pd = new Point();
	  Point pdold = new Point(0,0);
  
	  for(int i=0; i<DrawdataSize; ++i){
		 obj = Drawdata.elementAt(i);
		 try{
			 pd.x=obj.getClass().getField("x").getFloat(obj);
		 }
		 catch(IllegalArgumentException IAE){pd.x=1;}
		 catch(NoSuchFieldException NSFE){pd.x=2;}
		 catch(IllegalAccessException IAcE){pd.x=3;}
		 try{
			 pd.y=obj.getClass().getField("y").getFloat(obj); 
		 }
		 catch(IllegalArgumentException IAE){pd.y=0;}
		 catch(NoSuchFieldException NSFE){pd.y=0;}
		 catch(IllegalAccessException IAcE){pd.y=0;}
//		 g.drawString("i is "+i+". x is "+pd.x+". y is "+pd.y+". ",20,30+i*20);

    	g.drawLine(Math.round(koef*pdold.x+200),
				   Math.round(koef*pdold.y+200),
				   Math.round(koef*pd.x+200),
				   Math.round(koef*pd.y+200));			 
		pdold.x=pd.x;
		pdold.y=pd.y;
	  }
	}

	/**
	 * Converts a string formatted as "rrggbb" to an awt.Color object
	 */
	private Color stringToColor(String paramValue)
	{
		int red;
		int green;
		int blue;

		red = (Integer.decode("0x" + paramValue.substring(0,2))).intValue();
		green = (Integer.decode("0x" + paramValue.substring(2,4))).intValue();
		blue = (Integer.decode("0x" + paramValue.substring(4,6))).intValue();

		return new Color(red,green,blue);
	}

	/**
	 * External interface used by design tools to show properties of an applet.
	 */
	/**
	 * Intializes values for the applet and its components
	 */
	void initForm()
	{
		this.setBackground(stringToColor("008080"));
		this.setForeground(Color.white);
	}
}
