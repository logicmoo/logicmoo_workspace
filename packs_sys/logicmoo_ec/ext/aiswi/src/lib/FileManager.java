package lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javasciff.SProject;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class FileManager {
    
    /**
     * Legge una stringa da un file.
     *
     * @param filePath      nome del file da aprire
     *
     */
    public static String readFileAsString(String filePath) {
        try {
            StringBuffer fileData = new StringBuffer(1000);
            // BufferedReader reader = new BufferedReader(new InputStreamReader(this.getClass().getClassLoader().getResourceAsStream(filePath)));
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            char[] buf = new char[1024];
            int numRead = 0;
            while ((numRead = reader.read(buf)) != -1) {
                String readData = String.valueOf(buf, 0, numRead);
                fileData.append(readData);
                buf = new char[1024];
            }
            reader.close();
            return fileData.toString();
        } catch (IOException ex) {
            Logger.getLogger(SProject.class.getName()).log(Level.SEVERE, null, ex);
            return "";
        }
    }

    /**
     * Scrive una stringa intera in un file
     *
     * @param string
     * @param fileName
     */
    public static void writeStringToFile(String string, String fileName) {
        try {
            FileWriter fstream = new FileWriter(fileName);
            BufferedWriter out = new BufferedWriter(fstream);

            out.write(string);
            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    /**
     * Metodo di utilità che permette di scrivere il contenuto di una lista
     * all'interno di un file
     *
     * @param list
     * @param fileName
     */
    public static void writeListToFile(List list, String fileName) {
        try {
            FileWriter fstream = new FileWriter(fileName);
            BufferedWriter out = new BufferedWriter(fstream);

            for (Object el : list) {
                out.write(el.toString() + "\n");
                out.write("\n");
            }

            out.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

}
