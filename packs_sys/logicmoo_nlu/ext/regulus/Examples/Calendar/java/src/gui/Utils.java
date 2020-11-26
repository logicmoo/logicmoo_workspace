package gui;

import java.awt.Component;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;


class Utils {
    private static Logger logger = Logger.getLogger(Utils.class.getPackage().getName());

	/** Returns an ImageIcon, or null if the path was invalid. */
	public final static ImageIcon createImageIcon(String path, String description) {
		URL imgURL = null;
		ImageIcon result = null;

		try {
			imgURL = Utils.class.getClassLoader().getResource(path);

			if (imgURL == null) {
				imgURL = (new File(path)).toURL();
			}

			if (imgURL != null) {
				result = new ImageIcon(imgURL, description);
			}
			else {
				result = null;
			}
		}
		catch (MalformedURLException e) {
			System.out.println("ERR appers in Utils class, createImageIcon method: ");
			System.out.println(e.toString());
		}

		return result;
	}

	/**
	 * @param newMessage
	 * @return
	 */
	public static boolean isStringEmpty(String str) {
		if ((str == null) || (str.trim().equals(""))) {
			return true;
		}
		return false;
	}
	
	/**
	 * Centers a window on the screen.
	 */
	public static void centerWindow(Component component) {
		Rectangle screen = new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
		Point center = new Point((int) screen.getCenterX(), (int) screen.getCenterY());
		Point newLocation = new Point(center.x - component.getWidth() / 2, center.y - component.getHeight() / 2);
		if (screen.contains(newLocation.x, newLocation.y, component.getWidth(), component.getHeight())) {
			component.setLocation(newLocation);
		}
	}
    
    public final static void createXSLTFile(String xsltFile) {
        final String fileContents = 
        	"<?xml version=\"1.0\" encoding=\"ISO8859-1\"?><xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"><xsl:output method=\"text\" indent=\"yes\" /><!-- xsl:strip-space elements=\"*\" / --><xsl:template match=\"log\"><xsl:for-each select=\"interaction\"><xsl:value-of select=\"wavfile\" /><xsl:text disable-output-escaping=\"yes\">  </xsl:text><xsl:value-of select=\"primaryRecognitionResult\" /><xsl:text disable-output-escaping=\"yes\">&#xA;</xsl:text></xsl:for-each></xsl:template></xsl:stylesheet>";
        
        try {
            OutputStream os = new FileOutputStream(xsltFile);
            os.write(fileContents.getBytes());
            os.close();
        }
        catch (FileNotFoundException e) {
            logger.log(Level.WARNING, "Could not open '" + xsltFile + "'", e);
        }
        catch (IOException e) {
            logger.log(Level.WARNING, "Could not write to '" + xsltFile + "'", e);
        }
    }

}
