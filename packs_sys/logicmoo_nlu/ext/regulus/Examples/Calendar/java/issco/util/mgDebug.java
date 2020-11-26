package issco.util;

import java.util.Vector;
import java.util.Date;
import java.io.*;


/*
 * myDebug.java
 *<p>Title: Class useful for printing out debugging messages.</p>
 *
 * @author Maria Georgescul
 * @version 1.0
 */
public class mgDebug {
	public final static PrintStream output = System.out;

        /** Fixed information */
        private final static String comment_marker = "*";
        private final static String separator = " : ";
        private final static int header_width = 90;
        private final static String info_license = "Free for educational, research and other non-profit making uses only.";
        private final static String info_author = "Copyright (C) 2007, Maria Georgescul, ISSCO / TIM, ETI, University of Geneva ";
        		//"(Dalle Molle Institute for Semantic and Cognitive Studies) - Multilingual Information Processing Unit (TIM) - School of Translation and Interpretation (ETI), University of Geneva";
        private final static String info_www = "Website : http://www.issco.unige.ch/staff/mariag/index.html";
        private final static String info_mail = "e:mail  : maria.georgescul@eti.unige.ch";

        /**
         * Prints header information
         * @param text java.lang.String
         */
        public static void header(String text) {
                String horizontal_line ="";
                for (int i = 0; i< header_width; i++)
                	horizontal_line += comment_marker;

                output.println("\n" + horizontal_line ); 
                printBlock(text);
                printBlock(info_license);
                printBlock(info_author);
                printBlock(info_www);
                printBlock(info_mail);
                output.println(horizontal_line);
        }
        /**
         * Print message to output.
         * @param message java.lang.Object
         */
        public static void msg(Object message) {
                msg(message.toString());
        }
        /**
         * Print message to output.
         * @param message java.lang.String
         */
        public static void msg(String message) {
        	output.println(comment_marker + " " + message);
        }
        
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message double
         */
        public static void msg(String header, double message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message float
         */
        public static void msg(String header, float message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message int
         */
        public static void msg(String header, int message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message java.lang.Exception
         */
        public static void msg(String header, Exception message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message java.lang.Object
         */
        public static void msg(String header, Object message) {
                output.println(comment_marker + " " + header + separator + message.toString());
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message java.lang.String
         */
        public static void msg(String header, String message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print message to output.
         * @param header java.lang.String
         * @param message boolean
         */
        public static void msg(String header, boolean message) {
                output.println(comment_marker + " " + header + separator + message);
        }
        /**
         * Print a part of the header block.
         * @param text java.lang.String
         */

        /**
 *
 * @return java.lang.String
 */
public static String date() {
        return (new Date()).toString();
}

/**
 * Get the stack trace of a throwable object
 * @return java.lang.String
 * @param e java.lang.Throwable
 */
public static String getStackTrace(Throwable e) {
        StringWriter st = new StringWriter();
        e.printStackTrace(new PrintWriter(st));
        return st.toString();
}

private static void printBlock(String text) {
	
        String[] line = wordWrap(text, header_width-4);
        for (int i=0, ie=line.length; i<ie; i++) 
        	if (line[i] != null)
        		output.println(comment_marker + " " + line[i] + " " + comment_marker);
}

/**
 * Given a string and the maximum width, break the string
 * down into blocks of string which is narrower then width.
 * @return java.lang.String[]
 * @param text java.lang.String
 * @param width int
 */
private static String[] wordWrap(String text, int width) {
	String[] lines = new String[text.length() / width + 1];
	int i=0; 
	int j = 0;
	do {
		lines[j] = text.substring(i, Math.min(i + width, text.length()));
		j++;
		i += width;
	} while (i < text.length());
	return lines;
}
}




