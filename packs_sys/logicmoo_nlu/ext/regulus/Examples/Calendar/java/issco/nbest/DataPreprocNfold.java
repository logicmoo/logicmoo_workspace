package issco.nbest;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;


/**
 * Divide the data (being in xml format) into n folds. 
 * Useful for n-fold cross validation. 
 */
public class DataPreprocNfold {
	
	/**
	 * Divide the inpup xml File into five folds (i.e. 5 files to be used for training and 5 files to be used for testing). 
	 * @param inputXMLFileName
	 * @param trainFileName
	 * @param testFileName
	 */
	protected static void divideFor_5FoldCV(String inputXMLFileName, String[] trainFileName, String[] testFileName){
		Element[] nbestTrain = new Element[5];
		for (int i = 0; i< 5; i++)
			nbestTrain[i] = new Element("nbest");	
		Element[] nbestTest = new Element[5];
		for (int i = 0; i< 5; i++)
			nbestTest[i] = new Element("nbest");

		try{
			File[] fTrain = new File[5];
			FileOutputStream[] outSTrain = new FileOutputStream[5];
			PrintStream[] outPStreamTrain = new PrintStream[5];		
			
			File[] fTest = new File[5];
			FileOutputStream[] outSTest = new FileOutputStream[5];
			PrintStream[] outPStreamTest = new PrintStream[5];
			
			for (int t = 0; t< 5; t++){
				fTrain[t] = new File( trainFileName[t]);
				outSTrain[t] = new FileOutputStream(fTrain[t]);
				outPStreamTrain[t] = new PrintStream(outSTrain[t]);
				
				fTest[t] = new File( testFileName[t]);
				outSTest[t] = new FileOutputStream(fTest[t]);
				outPStreamTest[t] = new PrintStream(outSTest[t]);
			}
			try{
			
				Document d = new org.jdom.input.SAXBuilder().build(new File(inputXMLFileName)); 
				java.util.List nbestList = d.getRootElement().getChildren("nbest_data");
		
				String previousWavSession = "";		
				int foldNo = 0;
				boolean mustChangeFold = false;
				for (int i = 0; i< nbestList.size(); i++){
					Element nbestElem = (Element) nbestList.get(i);		
					Element wavElem = nbestElem.getChild("wavfile");
					String wavFile =  wavElem.getValue();
					String thisWavSession = (String) wavFile.subSequence(63, 82);
					for (int y = 1; y <= 5; y++){
						int foldNoAux = Math.max(0, i - y*(nbestList.size()/5) );
						if ((foldNoAux > 0) && (y > foldNo)){
							mustChangeFold = true;
							foldNo = y;
						}
						
					}
					if ((thisWavSession.equals(previousWavSession)) && (mustChangeFold)){
						// put the entire next set from this session in the same fold as the previous one
						foldNo = (foldNo > 0) ? (foldNo-1) : foldNo;
						mustChangeFold = false;
					}
					if (!(thisWavSession.equals(previousWavSession)) && (mustChangeFold)){
						mustChangeFold = false;
					}
									
					if (foldNo == 5)
						foldNo = 4;
					
					nbestTest[foldNo] = nbestTest[foldNo].addContent((Element)nbestElem.clone());
					for (int k=0; k<5; k++){
						if (k != foldNo){
							nbestTrain[k].addContent((Element)nbestElem.clone());
						}
					}
					previousWavSession = (String) wavFile.subSequence(63, 82);
				}
			}
			catch (IOException eIO) {eIO.printStackTrace();}
			catch (JDOMException eJDOM) {eJDOM.printStackTrace();}
			
			for (int i = 0; i< 5; i++){
				try{
					new org.jdom.output.XMLOutputter().output(new Document(nbestTrain[i]), outPStreamTrain[i]);
				}
				catch(IOException ex){
					ex.printStackTrace();
				}
			}
			for (int i = 0; i< 5; i++){
				try{
					new org.jdom.output.XMLOutputter().output(new Document(nbestTest[i]), outPStreamTest[i]);
				}
				catch(IOException ex){
					ex.printStackTrace();
				}
			}
			
		}	
		catch(FileNotFoundException ex1){
			ex1.printStackTrace();
		}
	}
	
	/**
	 * Only for testing the methods of this class.
	 * @param args
	 */
	public static void main(String[] args){
		String dirName ="";
		try{
			dirName = args[0];
		}
		catch(java.lang.ArrayIndexOutOfBoundsException ex){
			System.out.println("Please provide the following program parameters : \n" +
					"\n 1) the name of the folder where the nbest.xml file is. " +
					"\n     (nbest.xml being the output of DataPreproc.java and contains " +
					"the feature values in xml format ) \n" );
			System.exit(1);
		}
		
		
		String inputXMLFileName = dirName + "nbest.xml";
		String[] trainFileNames = new String[5];
		String[] testFileNames = new String[5];
		for (int i = 1; i< 6; i++){
			trainFileNames[i-1] = dirName + "xmlInFiles/" + "nbest_train_fold" + i + ".xml";
			testFileNames[i-1] = dirName + "xmlInFiles/" + "nbest_test_fold" + i + ".xml";
		}
		divideFor_5FoldCV(inputXMLFileName, trainFileNames, testFileNames);
	}

}
