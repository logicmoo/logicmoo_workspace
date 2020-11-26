package shef.nlp.supple;

/* test for the wrapper */

import gate.Corpus;
import gate.CreoleRegister;
import gate.Document;
import gate.Factory;
import gate.FeatureMap;
import gate.Gate;
import gate.ProcessingResource;
import gate.creole.ResourceInstantiationException;
import gate.creole.SerialAnalyserController;
import gate.util.GateException;

import java.io.File;
import java.net.MalformedURLException;

public class TestSuite
{

   public static void main(String[] args)
   {

      String creoleURL=args[0];
      String configURL=args[1];
      String tableURL=args[2];
      String parserFile=args[3];
      String implementation=args[4];

      File f = null;

      System.out.println("Testing wrapper " + implementation + "...");
      System.out.println(
         configURL + " \n" +
         tableURL + "\n" +
         parserFile
      );

      try
      {
         Gate.init();
         CreoleRegister reg=Gate.getCreoleRegister();
         f=new File(creoleURL);
         reg.registerDirectories(f.toURI().toURL());

         f = new File(System.getProperty("gate.home"),"plugins/Tools/");
         reg.registerDirectories(f.toURI().toURL());
         f = new File(System.getProperty("gate.home"),"plugins/ANNIE/");
         reg.registerDirectories(f.toURI().toURL());

         SerialAnalyserController controller = (SerialAnalyserController)Factory.createResource("gate.creole.SerialAnalyserController");

         /** tokeniser **/
         System.out.print("Loading Tokeniser...");
         FeatureMap fm_tokens = Factory.newFeatureMap();
         try
         {
            ProcessingResource tokens = (ProcessingResource) Factory.createResource("gate.creole.tokeniser.DefaultTokeniser",
            fm_tokens,
            Factory.newFeatureMap());
            controller.add(tokens);
            System.out.println(" Done");
         }
         catch (ResourceInstantiationException rie)
         {
            System.out.println(" FAILED");
            rie.printStackTrace();
         }

         /* sentence splitter */
         System.out.print("Loading Sentence Splitter...");
         FeatureMap fm_splitter=Factory.newFeatureMap();
         try
         {
            ProcessingResource splitter = (ProcessingResource) Factory.createResource("gate.creole.splitter.SentenceSplitter",
            fm_splitter,
            Factory.newFeatureMap());
            controller.add(splitter);
            System.out.println(" Done");
         }
         catch(ResourceInstantiationException rie)
         {
            System.out.println(" FAILED");
            rie.printStackTrace();
         }

         /* POS tagger */
         System.out.print("Loading POS Tagger...");
         FeatureMap fm_pos=Factory.newFeatureMap();
         try
         {
            ProcessingResource tagger = (ProcessingResource) Factory.createResource("gate.creole.POSTagger",
            fm_pos,
            Factory.newFeatureMap());
            controller.add(tagger);
            System.out.println(" Done");
         }
         catch(ResourceInstantiationException rie)
         {
            System.out.println(" FAILED");
            rie.printStackTrace();
         }

         if (gate.Main.version.startsWith("3"))
         {
            /* Morphology */
            System.out.print("Loading Morphological Analyzer...");
            try
            {
               ProcessingResource morphology = (ProcessingResource) Factory.createResource("gate.creole.morph.Morph",
               Factory.newFeatureMap());
               controller.add(morphology);
               System.out.println(" Done");
            }
            catch(ResourceInstantiationException rie)
            {
               System.out.println(" FAILED");
               rie.printStackTrace();
            }
         }


         /* Name Entity Recogniser */
         System.out.print("Loading NE Tagger...");
         try
         {
            ProcessingResource list= (ProcessingResource) Factory.createResource("gate.creole.gazetteer.DefaultGazetteer", Factory.newFeatureMap());
            controller.add(list);

            ProcessingResource ne= (ProcessingResource) Factory.createResource("gate.creole.ANNIETransducer");
            controller.add(ne);

            System.out.println(" Done");
         }
         catch(ResourceInstantiationException rie)
         {
            System.out.println(" FAILED");
            rie.printStackTrace();
         }

         /* Bottom-Up Chart Parser */
         System.out.print("Loading SUPPLE...");
         try
         {
            FeatureMap fm=Factory.newFeatureMap();
            fm.put(SUPPLE.CONFIG_FILE_PAR,(new File(configURL)).toURI().toURL());
            fm.put(SUPPLE.FEATURE_FILE_PAR,(new File(tableURL)).toURI().toURL());
            fm.put("SUPPLEFile",new File(parserFile).toURI().toURL());
            fm.put("prologImplementation",implementation);

            ProcessingResource supple=(ProcessingResource)Factory.createResource("shef.nlp.supple.SUPPLE",fm);
            controller.add(supple);

            System.out.println(" Done");
         }
         catch(ResourceInstantiationException rie)
         {
            System.out.println(" FAILED");
            rie.printStackTrace();
         }

         Document doc=Factory.newDocument("2 October 2004: this is a sentence. This is another sentence.");

         Corpus corpus=Factory.newCorpus("");
         corpus.add(doc);

         System.out.print("\nParsing document...");
         controller.setCorpus(corpus);
         controller.execute();
         System.out.println(" Done\n");

         System.out.println(doc.getAnnotations());
      }
      catch(GateException ge)
      {
         ge.printStackTrace();
      }
      catch(MalformedURLException murle)
      {
         murle.printStackTrace();
      }
   }
}
