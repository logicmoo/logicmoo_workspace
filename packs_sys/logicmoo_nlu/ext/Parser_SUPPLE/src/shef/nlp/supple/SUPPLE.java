package shef.nlp.supple;


/**
 *
 * <p>Title: SUPPLE</p>
 * <p>Copyright: Copyright (c) 2003-2006</p>
 * @version 1.0
 */

//gate stuff
import gate.Annotation;
import gate.AnnotationSet;
import gate.Document;
import gate.DocumentContent;
import gate.Factory;
import gate.FeatureMap;
import gate.ProcessingResource;
import gate.Resource;
import gate.creole.AbstractLanguageAnalyser;
import gate.creole.ExecutionException;
import gate.creole.ResourceInstantiationException;
import gate.gui.STreeNode;
import gate.util.InvalidOffsetException;
import gate.util.OffsetComparator;
import gate.util.SimpleFeatureMapImpl;
import gate.util.Files;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import shef.nlp.supple.category.Chart;
import shef.nlp.supple.prolog.Prolog;
import shef.nlp.supple.utils.BestParseOutput;
import shef.nlp.supple.utils.SemOutput;
import shef.nlp.supple.utils.SynOutput;
import shef.nlp.supple.utils.SynSemTriple;


public class SUPPLE extends AbstractLanguageAnalyser implements ProcessingResource, Serializable
{
   /** Name of the temp file prefix for the input buchart **/
   protected static String InTempFileName = "SUPPLE--IN--";

   /** Name of the temp file prefix for the output from buchart  **/
   protected static String OutTempFileName = "SUPPLE--OUT--";

   /** Name of the temp file prefix for the semantic output from buchart **/
   protected static String SemTempFileName = "SUPPLE--SEM--";

   public File InTempFile, OutTempFile, SemTempFile;


   /** The name of the executable BuChart **/
   private URL suppleFileUrl;
   private File suppleFile;
   public void setSUPPLEFile(URL suppleFile) { suppleFileUrl = suppleFile; }
   public URL getSUPPLEFile() { return suppleFileUrl; }

/*
     protected String syntaxSetName;

     public void setSyntaxSetName(String n) { syntaxSetName = n; }

     public String getSyntaxSetName() { return syntaxSetName; }
*/

   /* where to store the semantics of each parsed chunk */
   protected String semanticsSetName;
   public void setSemanticsSetName(String n) { semanticsSetName = n; }
   public String getSemanticsSetName() { return semanticsSetName; }

   /** The document under analysis. */
   protected Document document;
   public Document getDocument() { return document; }
   public void setDocument(Document doc) { document = doc; }

   /** the configuration file **/
   public URL configFile;
   public void setConfigFile(URL configFile) {this.configFile = configFile; }
   public URL getConfigFile() { return configFile;}

   /** the feature table **/
   public URL featureFile;
   public void setFeatureFile(URL featureFile) { this.featureFile = featureFile; }
   public URL getFeatureFile() { return featureFile;}

   /** only pass longest chunk to the chart parser **/
   public Boolean longestMatch;
   public Boolean getLongestMatch() { return longestMatch;}
   public void setLongestMatch(Boolean m) { longestMatch=m;}

   /** Different Prolog Implementations **/
   private Prolog prolog;
   private String prologImpl;
   public String getPrologImplementation() { return prologImpl;}
   public void setPrologImplementation(String prologImpl) { this.prologImpl=prologImpl; }

   /** Debug flag */
   private Boolean debug;
   public Boolean getDebug() { return debug; }
   public void setDebug(Boolean debug) { this.debug = debug; }

   /** Gate specification **/
   public ArrayList gateAnnotations;
   public ArrayList gateConstraints;
   public ArrayList gateVariables;

   /** Gate annotation sets and annotations to consider */
   public Hashtable annotationSetTable;

   /** Buchart specification **/
   private ArrayList buchartConstraints;
   private ArrayList buchartVariables;

   /** show mapping Gate-Buchart **/
   public void showMapping()
   {

      for(int i=0;i<gateConstraints.size();i++)
      {
         System.out.println("Annotation " + i);
         System.out.println(gateAnnotations.get(i));
         System.out.println("Constraint " );
         System.out.println(gateConstraints.get(i));
         System.out.println(buchartConstraints.get(i));
         System.out.println("Variables ");
         System.out.println(gateVariables.get(i));
         System.out.println(buchartVariables.get(i));
      }
   }

   /** valid categories and their buchart attributes **/
   private Hashtable buchartCategories;
   /** priorities associated with the buchart categories if longest match to be used */
   public Hashtable priorityList;

   private static final String CAT_DELIMITER=";";
   private static final String ATT_DELIMITER=";";
   private static final String GATE_LINE="Gate";
   private static final String SUPPLE_LINE="SUPPLE";
   private static final String SUPPLE_CAT="category";

   public static final String CONFIG_FILE_PAR="configFile";
   public static final String FEATURE_FILE_PAR="featureFile";

   public Resource init() throws ResourceInstantiationException
   {
      try {
        suppleFile = Files.fileFromURL(suppleFileUrl);
      }
      catch(IllegalArgumentException iae) {
        throw new ResourceInstantiationException(
            "SUPPLEFile parameter must be a valid file: URL");
      }
      /** Check the specified prolog saved state **/
      if (!suppleFile.exists() || !suppleFile.isFile())
      {
			throw new ResourceInstantiationException("SUPPLEFile parameter does not point to a file");
		}

      /** Check the specified prolog **/
      try
      {
         Class c = Class.forName(prologImpl);
         prolog = (Prolog)c.newInstance();
         prolog.init(suppleFile);
      }
      catch (Exception e)
      {
         e.printStackTrace();
         throw new ResourceInstantiationException("Unable to correctly load and initialise the Prolog interface");
      }

      /* read feature table */
      StringTokenizer tokenizer;
      /* gate constraints and variables */
      gateConstraints=new ArrayList();
      gateVariables=new ArrayList();
      gateAnnotations=new ArrayList();
      /* buchart constraints and variables */
      buchartConstraints=new ArrayList();
      buchartVariables=new ArrayList();
      /* annotations to pass */
      annotationSetTable=new Hashtable();

      /* the configuration file */

      priorityList=new Hashtable();
      int priority=0;
      try
      {
         BufferedReader in = new BufferedReader(new InputStreamReader(featureFile.openStream()));
         String line;
         String cat;
         String feature;
         buchartCategories=new Hashtable();
         ArrayList features;
         while((line = in.readLine()) != null)
         {
            tokenizer=new StringTokenizer(line,CAT_DELIMITER);
            /* first element if the category */
            if(tokenizer.hasMoreElements())
            {
               cat=tokenizer.nextToken();
               priorityList.put(cat,new Integer(priority));
               priority++;
               /* rest elements are the features in edge order */
               features=new ArrayList();
               while(tokenizer.hasMoreElements())
               {
                  features.add(tokenizer.nextToken());
               }
               /* create entry in table */
               buchartCategories.put(cat,features);
            }
         }
      }
      catch(IOException ioe)
      {
         throw new ResourceInstantiationException(ioe + " while reading " + getFeatureFile());
      }

      /* the mapping file */
      try
      {
         BufferedReader in = new BufferedReader(new InputStreamReader(configFile.openStream()));
         String line;
         String attVal;
         String attribute;
         String value;
         FeatureMap constraints;
         FeatureMap variables;
         FeatureMap annotations;
         int nextMapping=0;
         int lnum=0;
         /** format expected is: gate line followed by a buchart line
         *  anything else is ignored
         * **/
         while((line = in.readLine()) != null)
         {
            lnum++;
            if(line.startsWith(GATE_LINE))
            {
               constraints=Factory.newFeatureMap();
               variables=Factory.newFeatureMap();
               annotations=Factory.newFeatureMap();
               processGateLine(line,annotations,constraints,variables);
               try
               {
                  updateAnnotationSets(annotations, annotationSetTable);
               }
               catch(Exception e)
               {
                  throw new ResourceInstantiationException(e.getMessage()+ " config file (" + lnum + ")");
               }
               gateConstraints.add(constraints);
               gateVariables.add(variables);
               gateAnnotations.add(annotations);
               /* get next line */
               line = in.readLine();
               lnum++;
               if(line.startsWith(SUPPLE_LINE))
               {
                  constraints=Factory.newFeatureMap();
                  variables=Factory.newFeatureMap();
                  processLine(line,constraints,variables);
                  buchartConstraints.add(constraints);
                  buchartVariables.add(variables);
               }
               else
               {
                  /* format error */
                  throw new ResourceInstantiationException("Format error in config file line " + lnum);
               }
            }
         }
      }
      catch(IOException ioe)
      {
         throw new ResourceInstantiationException(ioe + " while reading " + getConfigFile());
      }
      /* initialize longest match at true */
      if(longestMatch==null) { longestMatch=new Boolean(true);}
      if(debug==null) { debug=new Boolean(false);}
      return this;
   }

   /** the same for reinitialization **/
   public Resource ReInit() throws ResourceInstantiationException
   {
      init();
      return this;
   }

   /* updates info on annotation sets and annotations to consider */
   public static void updateAnnotationSets(FeatureMap annotations, Hashtable table) throws Exception
   {
      /* annotations contains two keys 'AnnotationType'  and 'AnnotationSet' */
      String annSet;
      String annType;
      Set auxSet;
      if(annotations.containsKey("AnnotationSet"))
      {
         annSet=(String)annotations.get("AnnotationSet");
      }
      else
      {
         annSet="Default";
      }
      if(annotations.containsKey("AnnotationType"))
      {
         annType=(String)annotations.get("AnnotationType");
      }
      else
      {
         throw new Exception("No 'AnnotationType' specified");
      }

      if(table.containsKey(annSet))
      {
         auxSet=(Set)table.get(annSet);
      }
      else
      {
         auxSet=new HashSet();
      }
      auxSet.add(annType);
      table.put(annSet,auxSet);
   }

   /* creates the chart edges according to the information provided in the config files */
   public void execute() throws ExecutionException
   {
      DocumentContent dc=document.getContent();
      String stringContent=dc.toString();

      PrintWriter out = null;

      try
      {
         InTempFile = File.createTempFile(InTempFileName, "");
         OutTempFile = File.createTempFile(OutTempFileName, "");

         //SemTempFile = File.createTempFile(SemTempFileName,"");
         out = new PrintWriter(new FileWriter(InTempFile));
      }
      catch(IOException ioe)
      {
         throw new ExecutionException("Problems creating temporary files" + ioe.toString());
         //   ioe.printStackTrace();
         //   return;
      }

      //    System.out.println("Annotation Sets and Types to be considered");

      Iterator iteKey=annotationSetTable.keySet().iterator();
      Set annotationTypes;
      String annotationSet;
      String annotationSetName;
      String annotationType;
      Iterator iteTypes;
      AnnotationSet auxSet;

      AnnotationSet all=document.getAnnotations();

      AnnotationSet sentences=all.get("Sentence");

      //AnnotationSets and types



      if(sentences==null || sentences.isEmpty())
      {
         throw new ExecutionException("No sentences to parse");
      }

      ArrayList sentList=new ArrayList(sentences);
      /* put them in order */
      Collections.sort(sentList,new OffsetComparator());
      /* now for each sentence */
      Annotation sentence;
      Long startSent;
      Long endSent;

      ArrayList inSentenceList;
      Annotation auxAnnotation;
      Iterator iteAnnotation;
      AnnotationSet auxAnnotationSet;
      Set allInSentence;
      Annotation annotation;
      String type;
      FeatureMap fm;
      int index;
      Object[] pair;
      Chart[] charts=new Chart[sentList.size()];
      for(int s=0;s<sentList.size();s++)
      {
         inSentenceList=new ArrayList();

         sentence=(Annotation) sentList.get(s);
         startSent=sentence.getStartNode().getOffset();
         endSent=sentence.getEndNode().getOffset();

         /* create the empty chart */
         charts[s]=new Chart(new Long(s+1),startSent,endSent,new SUPPLERecord("top",startSent,startSent,Factory.newFeatureMap()));

         /* for each annotation set get annotation types that span the offsets of the sentence */
         iteKey=annotationSetTable.keySet().iterator();
         while(iteKey.hasNext())
         {
            annotationSet=(String)iteKey.next();
            if(annotationSet.compareTo("Default")==0)
            {
               auxSet=document.getAnnotations();
            }
            else
            {
               if (document.getNamedAnnotationSets().containsKey(annotationSet))
               {
                  auxSet = document.getAnnotations(annotationSet);

                  if (auxSet == null || auxSet.isEmpty())
                  {
                     throw new ExecutionException(annotationSet+" does not exist");
                  }
               }
               else
               {
                  auxSet=null;
               }
            }

            annotationTypes=(Set) annotationSetTable.get(annotationSet);
            iteTypes=annotationTypes.iterator();
            while(iteTypes.hasNext())
            {
               annotationType=(String)iteTypes.next();
               if(!(auxSet==null) && !auxSet.isEmpty())
               {
                  auxAnnotationSet=auxSet.get(annotationType, startSent,endSent);
                  if(!(auxAnnotationSet==null) && !auxAnnotationSet.isEmpty())
                  {
                     iteAnnotation = auxAnnotationSet.iterator();

                     while(iteAnnotation.hasNext())
                     {
                        auxAnnotation =(Annotation)iteAnnotation.next();
                        pair=new Object[2];
                        pair[0]=annotationSet;
                        pair[1]=auxAnnotation;
                        inSentenceList.add(pair);
                     }
                  }
               }
            }
         }

         ArrayList workingList;
         Collections.sort(inSentenceList,pairComparator());
         FeatureMap variables;
         Long annStart, annEnd;
         SUPPLERecord record;
         ArrayList buchartList=new ArrayList();
         for(int a=0;a<inSentenceList.size();a++)
         {
            pair=(Object[])inSentenceList.get(a);
            annotationSetName=(String)pair[0];
            annotation=(Annotation)pair[1];
            annStart=annotation.getStartNode().getOffset();
            annEnd=annotation.getEndNode().getOffset();
            type=annotation.getType();
            fm=annotation.getFeatures();
            /** look for constraints **/
            index=getIndex(annotationSetName,annotation,gateAnnotations,gateConstraints);
            if(index>=0)
            {
               /* instantiate variables  and consider defaults for 'string' and 'text' */
               FeatureMap fmvar=(FeatureMap)gateVariables.get(index);
               Iterator ite=fmvar.keySet().iterator();
               String feature, var, val;
               variables=Factory.newFeatureMap();
               while(ite.hasNext())
               {
                  feature=(String) ite.next();
                  var=(String)fmvar.get(feature);
                  if(fm.containsKey(feature))
                  {
                     val=(String)fm.get(feature);
                  }
                  else
                  {
                     if(feature.compareTo("text")==0)
                     {
                        val="body";
                     }
                     else if(feature.compareTo("string")==0)
                     {
                        val=stringContent.substring(annStart.intValue(),annEnd.intValue());
                     }
                     else
                     {
                        val = "_";
                     }
                  }
                  variables.put(var,val);
               }
               FeatureMap fmbuchartvars=(FeatureMap)buchartVariables.get(index);
               ite=fmbuchartvars.keySet().iterator();

               /* create a feature map for the mapping */
               FeatureMap buchartMap=Factory.newFeatureMap();
               while(ite.hasNext())
               {
                  feature=(String) ite.next();
                  var=(String)fmbuchartvars.get(feature);
                  if(variables.containsKey(var))
                  {
                     buchartMap.put(feature,(String) variables.get(var));
                  }
                  else
                  {
                     buchartMap.put(feature,"_");
                  }
               }
               buchartMap.putAll((FeatureMap)buchartConstraints.get(index));
               record=new SUPPLERecord((String) buchartMap.get("category"),annStart,annEnd,buchartMap);
               buchartList.add(record);
            }
            else
            {

               // throw new ExecutionException("Restriction not found for annotation type " + type);
            }
         }

         /* sort the list of buchart categories by offset */
         Collections.sort(buchartList,SUPPLERecord.SUPPLERecordComparator());
         /* organise by offset and longest match */
         if(getLongestMatch().booleanValue()) {

           /* keeps one element per valid start offset */
           workingList=keepLongest(buchartList,priorityList);
      }
      else
      {
         workingList=buchartList;
      }


         /* list buchart format of each */
         String cat;
         ArrayList outFeatures;

         for(int c=0;c<workingList.size();c++)
         {
            record=(SUPPLERecord) workingList.get(c);
            charts[s].setNext(record);
            cat=record.getCategory();
            if(buchartCategories.containsKey(cat))
            {
               outFeatures = (ArrayList) buchartCategories.get(cat);
            }
            else
            {
               throw new ExecutionException(cat + " without printable features!");
            }
         }


         charts[s].setFinal(new SUPPLERecord("bottom",endSent,endSent,Factory.newFeatureMap()));


         out.println(charts[s].toSUPPLEFormat(buchartCategories));
         out.flush();
      }

      out.close();

      /** Mark **/
      prolog.parse(InTempFile,OutTempFile, debug.booleanValue());
      /** Horacio **/
      //  callParser(InTempFile,OutTempFile,SemTempFile,"flag");

      /* check instantiation of this resource */
      SynSemTriple parserOutput=extractSynSem();
      ArrayList synOut=parserOutput.getSyntax();

      createSyntacticAnnotations(synOut);

      ArrayList list=parserOutput.getSemnatics();
      ArrayList best=parserOutput.getBestParse();
      SemOutput aux;
      Long semStart, semEnd;
      ArrayList semList;

      if(semanticsSetName != null && semanticsSetName.equals("")) semanticsSetName = null;
      AnnotationSet semAnnotationSet = (semanticsSetName == null) ? document.getAnnotations() : document.getAnnotations(semanticsSetName);

      FeatureMap semfm;
      for(int i=0;i<list.size();i++)
      {


         aux=(SemOutput) list.get(i);
         semStart=aux.getStart();
         semEnd=aux.getEnd();
         semList=aux.getSemantics();
         semfm=Factory.newFeatureMap();
         semfm.put("qlf",semList);

         try
         {
            semAnnotationSet.add(semStart,semEnd,"semantics",semfm);
         }
         catch(InvalidOffsetException ioe)
         {
            ioe.printStackTrace();
         }
      }


      BestParseOutput auxBest;
      String startBest,endBest;
      for(int i=0;i<best.size();i++)
      {

         auxBest=(BestParseOutput) best.get(i);
         startBest=auxBest.getStart();
         endBest=auxBest.getEnd();

         semfm=Factory.newFeatureMap();
         semfm.put("best_parse",auxBest.getBestParse());
         try
         {
            all.add(new Long(startBest),new Long(endBest),"parse",semfm);
         }
         catch(InvalidOffsetException ioe)
         {
            ioe.printStackTrace();
         }
      }

      if(!debug.booleanValue())
      {
         InTempFile.delete();
         OutTempFile.delete();
      }
   }

   /* the list contains pairs (set name,annotation)  and is sorted by annotation offset in ascending order */
   public static ArrayList keepLongest(ArrayList list)
   {
      ArrayList working=new ArrayList();
      Object[] pair;
      Object[] previous;
      Annotation preann;
      Annotation curann;
      long presize;
      long cursize;
      Long prestart, preend, start, end;
      int index=0;
      if(list.size()>0)
      {
         previous=(Object[])list.get(0);
         preann=(Annotation) previous[1];
         prestart=preann.getStartNode().getOffset();
         preend=preann.getEndNode().getOffset();
         presize=preend.longValue()-prestart.longValue();
         working.add(previous);
         for(int e=1;e<list.size();e++)
         {
            pair=(Object[]) list.get(e);
            curann=(Annotation)pair[1];
            start=curann.getStartNode().getOffset();
            end=curann.getEndNode().getOffset();
            cursize=end.longValue()-start.longValue();
            if(start.compareTo(prestart)==0)
            {
               if(cursize>presize)
               {
                  working.remove(working.size()-1);
                  working.add(pair);
               }
            }
            else if(start.compareTo(preend)>0)
            {
               working.add(pair);
            }
            previous=(Object[])working.get(working.size()-1);
            preann=(Annotation) previous[1];
            prestart=preann.getStartNode().getOffset();
            preend=preann.getEndNode().getOffset();
            presize=preend.longValue()-prestart.longValue();
         }
      }

      return working;
   }



   /* the list contains pairs (set name,annotation)  and is sorted by annotation offset in ascending order */
   public static ArrayList keepLongest(ArrayList list,Hashtable priorities) throws ExecutionException
   {
      ArrayList working=new ArrayList();
      SUPPLERecord current;
      SUPPLERecord previous;
      long presize;
      long cursize;
      Long prestart, preend, start, end;
      int index=0;
      int prepriority;
      int curpriority;
      String auxCat;
      if(list.size()>0)
      {
         previous=(SUPPLERecord)list.get(0);
         auxCat=(String)previous.getCategory();
         if(priorities.containsKey(auxCat))
         {
            prepriority = ( (Integer) priorities.get(previous.getCategory())).intValue();
         }
         else
         {
            throw new ExecutionException(auxCat + " not found in feature table");
         }
         prestart=previous.getStart();
         preend=previous.getEnd();
         presize=preend.longValue()-prestart.longValue();
         working.add(previous);
         for(int e=1;e<list.size();e++)
         {
            current=(SUPPLERecord) list.get(e);
            auxCat=(String)current.getCategory();
            if(priorities.containsKey(auxCat))
            {
               curpriority = ( (Integer) priorities.get(current.getCategory())).intValue();
            }
            else
            {
               throw new ExecutionException(auxCat + " not found in feature table");
            }
            start=current.getStart();
            end=current.getEnd();
            cursize=end.longValue()-start.longValue();
            if(start.compareTo(prestart)==0)
            {
               if(cursize>presize)
               {
                  working.remove(working.size()-1);
                  working.add(current);
               }
               else if(cursize==presize)
               {
                  if(curpriority<prepriority)
                  {
                     working.remove(working.size()-1);
                     working.add(current);
                  }
               }
            }
            else if(start.compareTo(preend)>=0)
            {
               working.add(current);
            }
            previous=(SUPPLERecord)working.get(working.size()-1);
            prepriority=((Integer) priorities.get(previous.getCategory())).intValue();
            prestart=previous.getStart();
            preend=previous.getEnd();
            presize=preend.longValue()-prestart.longValue();
         }
      }

      return working;
   }


   public static int getIndex(String annotationSet,Annotation annotation, ArrayList annotations, ArrayList constraints)
   {
      int index=-1;
      String type=annotation.getType();
      FeatureMap fm=annotation.getFeatures();
      //     System.out.println(" >>" + annotationSet + "<<" );
      FeatureMap annfm;
      FeatureMap consfm;
      String auxType;
      String auxSet;
      for(int i=0;i<annotations.size();i++)
      {
         annfm=(FeatureMap)annotations.get(i);
         consfm=(FeatureMap)constraints.get(i);
         auxType=(String)annfm.get("AnnotationType");
         if(annfm.containsKey("AnnotationSet"))
         {
            auxSet = (String) annfm.get("AnnotationSet");
         }
         else
         {
            auxSet = "Default";
         }
         if(auxType.compareTo(type)==0 && auxSet.compareTo(annotationSet)==0)
         {
            if(fm.subsumes(consfm))
            {
               return i;
            }
         }
      }

      return index;
   }

   public static Comparator pairComparator()
   {
      Comparator comp=new Comparator()
      {
         public int compare(Object o1, Object o2)
         {
            Object[] a1 = (Object[]) o1;

            Object[] a2 = (Object[]) o2;

            Annotation an1 = (Annotation) a1[1];
            Annotation an2 = (Annotation) a2[1];

            return an1.getStartNode().getOffset().compareTo(an2.getStartNode().getOffset());
         }
      };
      return comp;
   }

   public static void processGateLine(String line, FeatureMap annotations, FeatureMap constraints, FeatureMap variables)
   {
      /* attribute value pattern */
      Pattern attValPat=Pattern.compile("(.*)=(.*)");

      /* read feature table */
      StringTokenizer tokenizer;

      String attVal;
      String attribute;
      String value;


      tokenizer=new StringTokenizer(line,ATT_DELIMITER);
      /* consume firet element */
      tokenizer.nextElement();
      while(tokenizer.hasMoreElements())
      {
         attVal=(String)tokenizer.nextToken();
         Matcher m=attValPat.matcher(attVal);
         if(m.matches())
         {
            attribute=m.group(1);
            value=m.group(2);
            if(attribute.compareTo("AnnotationSet")==0 || attribute.compareTo("AnnotationType")==0)
            {
               annotations.put(attribute,value);
            }
            else
            {
               /* variable or value */
               if (value.indexOf("&") == 0)
               {
                  variables.put(attribute, value);
               }
               else
               {
                  constraints.put(attribute, value);
               }
            }
         }
      }
   }

   public static void processLine(String line, FeatureMap constraints, FeatureMap variables)
   {
      /* attribute value pattern */
      Pattern attValPat=Pattern.compile("(.*)=(.*)");

      /* read feature table */
      StringTokenizer tokenizer;

      String attVal;
      String attribute;
      String value;


      tokenizer=new StringTokenizer(line,ATT_DELIMITER);
      /* consume firet element */
      tokenizer.nextElement();
      while(tokenizer.hasMoreElements())
      {
         attVal=(String)tokenizer.nextToken();
         Matcher m=attValPat.matcher(attVal);
         if(m.matches())
         {
            attribute=m.group(1);
            value=m.group(2);
            /* variable or value */
            if(value.indexOf("&")==0)
            {
               variables.put(attribute,value);
            }
            else
            {
               constraints.put(attribute,value);
            }
         }
      }
   }

   public SynSemTriple extractSynSem()
   {
      DocumentContent dc=document.getContent();
      /* stack for the best parse */
      SynSemTriple outTriple;
      String start, end, category, constituents;
      Pattern offsets=Pattern.compile("semantics (\\d+) (\\d+)");
      Matcher match;
      ArrayList list=new ArrayList();
      ArrayList outList=new ArrayList();
      ArrayList bestParse=new ArrayList();
      String startSem, endSem;

      startSem="null";
      endSem="null";
      String startBest, endBest;
      startBest=null;
      endBest=null;
      SemOutput singleSem=null;

      ArrayList output = new ArrayList();

      ArrayList bestParseOut= new ArrayList();

      String SYN="syntax";

      StringTokenizer tokeniser;

      boolean first=true;

      int level=0;

      if(OutTempFile.isFile())
      {
         try
         {

            String TempName = OutTempFile.getAbsolutePath();

            BufferedReader in = new BufferedReader(new FileReader(TempName));

            String line;

            while((line = in.readLine()) != null)
            {
               tokeniser = new StringTokenizer(line," ");
               if(tokeniser.hasMoreTokens())
               {
                  String test = tokeniser.nextToken();

                  if(test.compareTo(SYN)==0)
                  {
                     if(first)
                     {
                        first=false;level=0;
                     }
                     else
                     {
                        level++;
                     }

                     /* next two are offsets */

                     start = tokeniser.nextToken();

                     end = tokeniser.nextToken();
                     if(first) { startBest=start; endBest=end;}

                     category = tokeniser.nextToken();

                     /* skip one */

                     tokeniser.nextToken();

                     /* constituents */

                     constituents = tokeniser.nextToken();

                     output.add(new SynOutput(start,end,category,constituents,level));
                     bestParse.add(new SynOutput(start,end,category,constituents,level));

                  }
                  else
                  {
                     match=offsets.matcher(line);

                     if(match.matches())
                     {
                        SynOutput aux;
                        ArrayList stack=new ArrayList();
                        int consti;
                        String categ;
                        String text;
                        for(int b=bestParse.size()-1;b>=0;b--)
                        {
                           aux=(SynOutput)bestParse.get(b);
                           categ=aux.getCategory();
                           consti=(new Integer(aux.getConstituens())).intValue();
                           if(consti==0)
                           {
                              /* to the stack */
                              try
                              {
                                 stack.add(0, "( " + categ + " \"" +
                                              dc.getContent(new Long(aux.getStart()),
                                              new Long(aux.getEnd())) + "\"" +
                                              " )");
                              }
                              catch(InvalidOffsetException ioe)
                              {
                                 ioe.printStackTrace();
                              }
                           //  dc.getContent(new Long(aux.getStart()),new Long(aux.getEnd()));
                           }
                           else
                           {
                              /* create (cat (c1) (c2) .... (cn)) and put it into the stack */
                              String element="";
                              for(int c=0;c<consti;c++)
                              {
                                 element=element + " " + (String) stack.get(0);
                                 stack.remove(0);
                              }
                              element="( " +categ+ " " + element + " )";
                              stack.add(0,element);
                           }
                           // System.out.println(aux.getCategory() + " " + aux.getConstituens());
                        }

                        bestParse=new ArrayList();
                        /*
                        if(first) {
                        first=false;
                        } else {
                        singleSem.setSemantics(list);
                        outList.add(singleSem);
                        }
                        */
                        list=new ArrayList();
                        singleSem=new SemOutput();
                        startSem=match.group(1);
                        endSem=match.group(2);
                        singleSem.setStart(new Long(startSem));
                        singleSem.setEnd(new Long(endSem));
                        bestParseOut.add(new BestParseOutput(startSem,endSem,(String)stack.get(0)));
                        /* loop on each term */
                        boolean inSem=true;
                        while(inSem && (line = in.readLine()) != null)
                        {
                           if(line.length()!=0 && !line.startsWith(" ") && line!="" && !line.startsWith(SYN))
                           {
                              list.add(line);
                           }
                           else
                           {
                              inSem=false;
                           }
                        }
                        singleSem.setSemantics(list);
                        outList.add(singleSem);
                     }

                     /* re start with syntax */

                     first=true;

                  }
               }
            }
         }
         catch(IOException ioe)
         {
            ioe.printStackTrace();
         }
      }

      outTriple=new SynSemTriple(output,outList,bestParseOut);

      return outTriple;
   }

   public ArrayList readSynFile()
   {
      String start, end, category, constituents;

      ArrayList output = new ArrayList();

      String SYN="syntax";

      StringTokenizer tokeniser;

      boolean first=true;

      int level=0;

      if(OutTempFile.isFile())
      {
         //           System.out.println("Trying to read buchart output file!");

         try
         {
            String TempName = OutTempFile.getAbsolutePath();

            BufferedReader in = new BufferedReader(new FileReader(TempName));

            String line;

            while((line = in.readLine()) != null)
            {
               //               System.out.println(line);
               tokeniser = new StringTokenizer(line," ");

               if(tokeniser.hasMoreTokens())
               {
                  String test = tokeniser.nextToken();

                  if(test.compareTo(SYN)==0)
                  {
                     if(first)
                     {
                        first=false;level=0;
                     }
                     else
                     {
                        level++;
                     }

                     /* next two are offsets */

                     start = tokeniser.nextToken();

                     end = tokeniser.nextToken();

                     category = tokeniser.nextToken();

                     /* skip one */

                     tokeniser.nextToken();

                     /* constituents */

                     constituents = tokeniser.nextToken();

                     output.add(new SynOutput(start,end,category,constituents,level));

                  }
                  else
                  {
                     first=true;
                  }
               }
            }
         }
         catch(IOException ioe)
         {
            ioe.printStackTrace();
         }
      }

      return output;
   }

   public static Integer SynTreeBack(ArrayList constituents,AnnotationSet docAnnotations,Integer yourFather)
   {
      //     System.out.println("Syntactic tree back...");
      ArrayList father;
      Integer son;
      Integer id=null;
      STreeNode node=null;
      FeatureMap fm;
      ArrayList components = new ArrayList();
      if(constituents.size()==0) System.out.println("Error!!!");

      SynOutput next = (SynOutput) constituents.get(0);
      String category=next.getCategory();
      Integer NRO=new Integer(next.getConstituens());
      int nro=NRO.intValue();
      String cat=next.getCategory();
      Long start = new Long(next.getStart());
      Long end   = new Long(next.getEnd());

      constituents.remove(0);
      /* for each component, construct a tree recursively */
      if(nro>0)
      {
         fm=Factory.newFeatureMap();
         try
         {
            id=docAnnotations.add(start,end,"SyntaxTreeNode",fm);
            node= new STreeNode(docAnnotations.get(id));
            node.setAllowsChildren(true);
         }
         catch(InvalidOffsetException ioe)
         {
            ioe.printStackTrace();
         }
         for(int i=0; i<nro; i++)
         {
            son=SynTreeBack(constituents,docAnnotations,id);
            // node.add(son);
            components.add(son);
         }


         fm.put("consists",components);
         fm.put("cat",category);
         father=new ArrayList();
         if(!(yourFather.compareTo(new Integer(0))==0))
         {
            father.add(yourFather);
         }
         else
         {
            /* root */
         }
         fm.put("father",father);
      }
      else
      {
         AnnotationSet auxTokens=docAnnotations.get("Token").get(start,end);
         if(auxTokens.size()>1)
         {
            //if(category.compareTo("sem_cat")==0) {
            /* is a semantic category, simulate a 'tree' structure */
            ArrayList neComponents=new ArrayList();
            /* get all tokens spanning the start and end */

            ArrayList auxList=new ArrayList(auxTokens);
            Annotation auxToken;
            String tokenCat;
            FeatureMap tokenfm;
            Long startToken,endToken;
            Integer id1;
            fm=Factory.newFeatureMap();
            father=new ArrayList();
            if(!(yourFather.compareTo(new Integer(0))==0))
            {
               father.add(yourFather);
            }
            else
            {
               /* root */
            }
            fm.put("father",father);
            try
            {
               id=docAnnotations.add(start,end,"SyntaxTreeNode",fm);
               node = new STreeNode(docAnnotations.get(id));
               node.setAllowsChildren(true);
            }
            catch(InvalidOffsetException ioe)
            {
               ioe.printStackTrace();
            }
            STreeNode node1;
            for(int h=0;h<auxList.size();h++)
            {
               auxToken=(Annotation) auxList.get(h);
               tokenfm=auxToken.getFeatures();
               startToken=auxToken.getStartNode().getOffset();
               endToken=auxToken.getEndNode().getOffset();

               FeatureMap fm1=Factory.newFeatureMap();

               if(tokenfm.containsKey("category"))
               {
                  tokenCat=(String) tokenfm.get("category");
                  tokenCat=tokenCat.toLowerCase();
               }
               else
               {
                  tokenCat=(String) tokenfm.get("string");
               }
               father=new ArrayList();
               father.add(id);
               fm1.put("consists",new ArrayList());
               fm1.put("cat",tokenCat);
               fm1.put("father",father);
               try
               {
                  id1=docAnnotations.add(startToken,endToken,"SyntaxTreeNode",fm1);
                  //System.out.println(id1);
                  node1 = new STreeNode(docAnnotations.get(id1));
                  node1.setAllowsChildren(false);

                  neComponents.add(id1);
               }
               catch(InvalidOffsetException ioe)
               {
                  ioe.printStackTrace();
               }
            }

            fm.put("consists",neComponents);
            fm.put("cat",category);

         }
         else
         {
            /* is a token */
            fm=Factory.newFeatureMap();
            fm.put("consists",new ArrayList());
            fm.put("cat",category);
            father=new ArrayList();
            if(!(yourFather.compareTo(new Integer(0))==0))
            {
               father.add(yourFather);
            }
            else
            {
               /* root */
            }
            fm.put("father",father);
            try
            {
               id=docAnnotations.add(start,end,"SyntaxTreeNode",fm);
               node = new STreeNode(docAnnotations.get(id));
               node.setAllowsChildren(false);
            }
            catch(InvalidOffsetException ioe)
            {
               ioe.printStackTrace();
            }
         }
      }

      return id;
   }

   public ArrayList ExtractQLF()
   {
      Pattern offsets=Pattern.compile("semantics (\\d+) (\\d+)");
      Matcher match;
      ArrayList list=new ArrayList();
      ArrayList outList=new ArrayList();
      String start, end;
      start="null";
      end="null";
      SemOutput singleSem=null;
      if(SemTempFile.isFile())
      {
         try
         {
            String TempName = SemTempFile.getAbsolutePath();

            BufferedReader in = new BufferedReader(new FileReader(TempName));

            String line;
            boolean first=true;
            while ( (line = in.readLine()) != null)
            {
               match=offsets.matcher(line);
               if(match.matches())
               {
                  if(first)
                  {
                     first=false;
                  }
                  else
                  {
                     singleSem.setSemantics(list);
                     outList.add(singleSem);
                  }
                  list=new ArrayList();
                  singleSem=new SemOutput();
                  start=match.group(1);
                  end=match.group(2);
                  singleSem.setStart(new Long(start));
                  singleSem.setEnd(new Long(end));
               }
               else
               {
                  list.add(line);
               }
            }
            singleSem.setSemantics(list);
            outList.add(singleSem);

         }
         catch (IOException ioe)
         {
            ioe.printStackTrace();
         }
      }
      else
      {
         System.out.println("Can't read semantic output!!!");
      }
      return outList;
   }

   public void createSyntacticAnnotations()
   {
      AnnotationSet theAnnotationSet=document.getAnnotations();
      AnnotationSet theTokens=theAnnotationSet.get("Token");
      ArrayList tokens=new ArrayList(theTokens);
      Collections.sort(tokens,new OffsetComparator());
      ArrayList synOut = new ArrayList();
      ArrayList synOut1;
      try
      {
         synOut= readSynFile();
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }

      synOut1=new ArrayList(synOut);
      while(synOut.size()>0)
      {
         //          System.out.println("creating the annotations");
         SynTreeBack(synOut,theAnnotationSet,new Integer(0));
      }

      /* for tokens without STreeNode we should create one */

      Annotation auxToken;
      Long tokenStart,tokenEnd;
      AnnotationSet auxSynSet;

      FeatureMap auxfm;
      String tokenCat;
      for(int i=0;i<tokens.size();i++)
      {
         auxToken=(Annotation) tokens.get(i);
         auxfm=auxToken.getFeatures();
         tokenCat=auxfm.get("category").toString();
         FeatureMap fm_token=Factory.newFeatureMap();
         fm_token.put("father",new ArrayList());
         fm_token.put("consists",new ArrayList());
         fm_token.put("cat",tokenCat);
         tokenStart=auxToken.getStartNode().getOffset();
         tokenEnd  =auxToken.getEndNode().getOffset();
         auxSynSet=theAnnotationSet.get("SyntaxTreeNode",tokenStart,tokenEnd);
         if(auxSynSet==null || auxSynSet.isEmpty())
         {
            //    System.out.println("Token in " + tokenStart + " - " + tokenEnd + " without SyntaxTreeNode...");
            try
            {
               theAnnotationSet.add(tokenStart,tokenEnd,"SyntaxTreeNode",fm_token);
            }
            catch(InvalidOffsetException ioe)
            {
               ioe.printStackTrace();
            }
         }
      }

      //      AnnotationSet syntax = document.getAnnotations(getSyntaxSetName());

      Iterator synIte = synOut1.iterator();

      SynOutput auxSyn;

      FeatureMap fm;

      while(synIte.hasNext())
      {
         //             System.out.println("Creating components in 'Syntax' set");

         auxSyn = (SynOutput) synIte.next();


         fm = new SimpleFeatureMapImpl();

         fm.put("constituents",auxSyn.getConstituens());

         fm.put("level",new Integer(auxSyn.getLevel()));

         try
         {
            theAnnotationSet.add(new Long(auxSyn.getStart()), new Long(auxSyn.getEnd()),auxSyn.getCategory(),fm);
         }
         catch(InvalidOffsetException ioe)
         {
            ioe.printStackTrace();
         }
      }
   }

   public void createSyntacticAnnotations(ArrayList synOut)
   {
      AnnotationSet theAnnotationSet=document.getAnnotations();
      AnnotationSet theTokens=theAnnotationSet.get("Token");
      ArrayList tokens=new ArrayList(theTokens);
      Collections.sort(tokens,new OffsetComparator());

      ArrayList synOut1;

      synOut1=new ArrayList(synOut);
      while(synOut.size()>0)
      {
         //          System.out.println("creating the annotations");
         SynTreeBack(synOut,theAnnotationSet,new Integer(0));
      }

      /* for tokens without STreeNode we should create one */

      Annotation auxToken;
      Long tokenStart,tokenEnd;
      AnnotationSet auxSynSet;

      FeatureMap auxfm;
      String tokenCat;
      for(int i=0;i<tokens.size();i++)
      {
         auxToken=(Annotation) tokens.get(i);
         auxfm=auxToken.getFeatures();
         tokenCat=auxfm.get("category").toString();
         FeatureMap fm_token=Factory.newFeatureMap();
         fm_token.put("father",new ArrayList());
         fm_token.put("consists",new ArrayList());
         fm_token.put("cat",tokenCat);
         tokenStart=auxToken.getStartNode().getOffset();
         tokenEnd  =auxToken.getEndNode().getOffset();
         auxSynSet=theAnnotationSet.get("SyntaxTreeNode",tokenStart,tokenEnd);
         if(auxSynSet==null || auxSynSet.isEmpty())
         {
            //    System.out.println("Token in " + tokenStart + " - " + tokenEnd + " without SyntaxTreeNode...");
            try
            {
               theAnnotationSet.add(tokenStart,tokenEnd,"SyntaxTreeNode",fm_token);
            }
            catch(InvalidOffsetException ioe)
            {
               ioe.printStackTrace();
            }
         }
      }
   }
}

