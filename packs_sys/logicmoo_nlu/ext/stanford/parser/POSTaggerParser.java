
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.*;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.ling.CoreAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.LemmaAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.parser.lexparser.EnglishTreebankParserParams;
import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
// import edu.stanford.nlp.parser.lexparser.TreebankLangParserParams;
import edu.stanford.nlp.parser.shiftreduce.ShiftReduceParser;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.process.DocumentPreprocessor;
import edu.stanford.nlp.process.Morphology;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations;
import edu.stanford.nlp.semgraph.SemanticGraphFactory;
import edu.stanford.nlp.semgraph.semgrex.SemgrexMatcher;
import edu.stanford.nlp.semgraph.semgrex.SemgrexPattern;
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.GrammaticalStructureFactory;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.util.CoreMap;
import java.io.StringReader;
import java.util.List;

import edu.stanford.nlp.util.*;
import edu.stanford.nlp.trees.*;
import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.io.*;
import edu.stanford.nlp.ling.*;
import java.io.*;
import java.util.*;

/**
 * Demonstrates how to first use the tagger, then use the
 * ShiftReduceParser.  Note that ShiftReduceParser will not work
 * on untagged text.
 *
 * @author John Bauer
 */
public class POSTaggerParser {
  public static boolean is_DEBUG = true;
  public static StanfordCoreNLP pipeline = null;
  public static LexicalizedParser lp = null;
  public static TreebankLanguagePack tlp = null;
  public static GrammaticalStructureFactory gsf1 = null;
  public static GrammaticalStructureFactory gsf2 = null;
  public static MaxentTagger tagger = null;
  public static ShiftReduceParser model = null;
  public static String taggerPath = "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger";
  //public static String taggerPath = "models/english-left3words-distsim.tagger";
  public static String modelPath = "edu/stanford/nlp/models/srparser/englishSR.ser.gz";
  public static String grammarPath = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz";
  public static String annotators = "tokenize, ssplit, pos, lemma, ner, parse, dcoref depparse";

  public POSTaggerParser() {
  }
  /** This example shows a few more ways of providing input to a parser.
   *
   *  POSTaggerParser(str grammar, str options[])
   */
  synchronized public static void init(String grammar, String[] options) throws IOException {
      grammar = grammar!=null && grammar.length() > 0 ? grammar : grammarPath;
      if (options==null) options = new String[]{ "-maxLength", "8000", "-retainTmpSubcategories" };


      EnglishTreebankParserParams params = new EnglishTreebankParserParams();
      gsf2 = params.treebankLanguagePack().grammaticalStructureFactory(params.treebankLanguagePack().punctuationWordRejectFilter(), params.typedDependencyHeadFinder());

      lp = LexicalizedParser.loadModel(grammar, options);
      tlp = lp.getOp().langpack();
      gsf1 = tlp.grammaticalStructureFactory();
      // gsf2 = gsf1;

      model = ShiftReduceParser.loadModel(modelPath);
      tagger = new MaxentTagger(taggerPath);
      Properties props = new Properties();
      props.put("ner.model", "edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz");
      props.put("ner.applyNumericClassifiers", "false");
      props.setProperty("annotators", annotators);
      pipeline = new StanfordCoreNLP(props);
  }

  public static void init() throws IOException {
      init(null,null);
  }

  /**
   * 
   *  Uses the default tokenizer for this TreebankLanguagePack
   */
  public static Object tagPOS(String sent2) {
	//String sent2 = ("This is a slightly longer and more complex sentence requiring tokenization.");	
	Tokenizer<? extends HasWord> toke = tlp.getTokenizerFactory().getTokenizer(new StringReader(""+sent2));
	List<? extends HasWord> sentence2 = toke.tokenize();
	return tagSentence(sentence2, is_DEBUG);
  }

  public static Object tagPOSTagged(String[] sent3, String[] tag3){ 
      // String[] sent3 = { "It", "can", "can", "it", "." };
      // String[] tag3 = { "PRP", "MD", "VB", "PRP", "." }; 
      // Parser gets second "can" wrong without help
      List<HasWord> sentence3 = new ArrayList<HasWord>();
      for (int i = 0; i < sent3.length; i++) {
	  if (tag3!=null && tag3[i]!=null) {
	      sentence3.add(new TaggedWord(sent3[i], tag3[i]));
	  } else {
	      sentence3.add(new Word(sent3[i]));
	  }
      }
      return tagSentence(sentence3, is_DEBUG);
    }


   public static Object tagSentence(List<? extends HasWord> sentence, boolean debug){ 

      List<TaggedWord> tagged = tagger.tagSentence(sentence);
      //Tree srtree = model.apply(tagged);
      Tree parse = lp.parse(sentence);      
      GrammaticalStructure gs1 = gsf1.newGrammaticalStructure(parse);
      List<TypedDependency> tdl = gs1.typedDependenciesCCprocessed();
      GrammaticalStructure gs2 = gsf2.newGrammaticalStructure(parse);
      SemanticGraph graph = SemanticGraphFactory.generateUncollapsedDependencies(parse);
      /*
      if (debug) {      
	    parse.pennPrint();
	    System.out.println();
	    System.out.println(tdl);
	    System.out.println();
	    
	    System.out.println("The words of the sentence:");
	    for (Label lab : parse.yield()) {
		if (lab instanceof CoreLabel) {
		    System.out.println(((CoreLabel) lab).toString(CoreLabel.OutputFormat.VALUE_MAP));
		} else {
		    System.out.println(lab);
		}
	    }
	    System.out.println();
	    System.out.println(parse.taggedYield());
	    System.out.println();
      }*/
      return new Object[]{tagged,parse,gs1,tdl,gs2,graph};
    }


  public static Object annotateSentence(String text) {

       // create an empty Annotation just with the given text
    Annotation document = new Annotation(text);
    
    // run all Annotators on this text
    pipeline.annotate(document);
    
    // these are all the sentences in this document
    // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
    List<CoreMap> sentences = document.get(SentencesAnnotation.class);

    List<Object> semgraphs = new ArrayList<Object>();

    for(CoreMap sentence: sentences) {
      // traversing the words in the current sentence
      // a CoreLabel is a CoreMap with additional token-specific methods
      for (CoreLabel token: sentence.get(TokensAnnotation.class)) {
        // this is the text of the token
        String word = token.get(TextAnnotation.class);
        // this is the POS tag of the token
        String pos = token.get(PartOfSpeechAnnotation.class);
        // this is the NER label of the token
        String ne = token.get(NamedEntityTagAnnotation.class);       
      }

      // this is the parse tree of the current sentence
      Tree tree = sentence.get(TreeCoreAnnotations.TreeAnnotation.class);

      // this is the Stanford dependency graph of the current sentence
      SemanticGraph dependencies = sentence.get(SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation.class);

      semgraphs.add(new Object[]{tree, dependencies, sentence});
    }

    // This is the coreference link graph
    // Each chain stores a set of mentions that link to each other,
    // along with a method for getting the most representative mention
    // Both sentence and token offsets start at 1!
    Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
    
    return new Object[] { graph, semgraphs, sentences, document };
  }

  public static void main(String[] args) throws IOException {

    for (int argIndex = 0; argIndex < args.length; ) {
      switch (args[argIndex]) {
	case "-tagger":
	  taggerPath = args[argIndex + 1];
	  argIndex += 2;
	  break;
	case "-model":
	  modelPath = args[argIndex + 1];
	  argIndex += 2;
	  break;
	default:
	  throw new RuntimeException("Unknown argument " + args[argIndex]);
      }
    }

    String text = "My dog likes to shake his stuffed chickadee toy.";

    MaxentTagger tagger = new MaxentTagger(taggerPath);
    ShiftReduceParser model = ShiftReduceParser.loadModel(modelPath);

    DocumentPreprocessor tokenizer = new DocumentPreprocessor(new StringReader(text));
    for (List<HasWord> sentence : tokenizer) {
      List<TaggedWord> tagged = tagger.tagSentence(sentence);
      Tree tree = model.apply(tagged);
      System.err.println(tree);
    }
    main2(args);
    main3(args);
  }


   /** Usage: java -cp "*" StanfordCoreNlpDemo [inputFile [outputTextFile [outputXmlFile]]] */
  public static void main2(String[] args) throws IOException {
     // set up optional output files
     PrintWriter out;
     if (args.length > 1) {
       out = new PrintWriter(args[1]);
     } else {
       out = new PrintWriter(System.out);
     }
     PrintWriter xmlOut = null;
     if (args.length > 2) {
       xmlOut = new PrintWriter(args[2]);
     }

     // Create a CoreNLP pipeline. This line just builds the default pipeline.
     // In comments we show how you can build a particular pipeline
     // Properties props = new Properties();
     // StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
     //StanfordCoreNLP pipeline = new StanfordCoreNLP();
      init();

     // Initialize an Annotation with some text to be annotated. The text is the argument to the constructor.
     Annotation annotation;
     if (args.length > 0) {
       annotation = new Annotation(IOUtils.slurpFileNoExceptions(args[0]));
     } else {
       annotation = new Annotation("Kosgi Santosh sent an email to Stanford University. He didn't get a reply.");
     }

     // run all the selected Annotators on this text
     pipeline.annotate(annotation);

     // print the results to file(s)
     pipeline.prettyPrint(annotation, out);
     if (xmlOut != null) {
       pipeline.xmlPrint(annotation, xmlOut);
     }

     // Access the Annotation in code
     // The toString() method on an Annotation just prints the text of the Annotation
     // But you can see what is in it with other methods like toShorterString()
     out.println();
     out.println("The top level annotation");
     out.println(annotation.toShorterString());

     // An Annotation is a Map and you can get and use the various analyses individually.
     // For instance, this gets the parse tree of the first sentence in the text.
     List<CoreMap> sentences = annotation.get(CoreAnnotations.SentencesAnnotation.class);
     if (sentences != null && ! sentences.isEmpty()) {
       CoreMap sentence = sentences.get(0);
       out.println();
       out.println("The first sentence is:");
       out.println(sentence.toShorterString());
       out.println();
       out.println("The first sentence tokens are:");
       for (CoreMap token : sentence.get(CoreAnnotations.TokensAnnotation.class)) {
	 out.println(token.toShorterString());
       }
       Tree tree = sentence.get(TreeCoreAnnotations.TreeAnnotation.class);
       out.println();
       out.println("The first sentence parse tree is:");
       tree.pennPrint(out);
       out.println();
       out.println("The first sentence basic dependencies are:");
       out.println(sentence.get(SemanticGraphCoreAnnotations.BasicDependenciesAnnotation.class).toString(SemanticGraph.OutputFormat.LIST));
       out.println("The first sentence collapsed, CC-processed dependencies are:");
       SemanticGraph graph = sentence.get(SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation.class);
       out.println(graph.toString(SemanticGraph.OutputFormat.LIST));

       // Access coreference. In the coreference link graph,
       // each chain stores a set of mentions that co-refer with each other,
       // along with a method for getting the most representative mention.
       // Both sentence and token offsets start at 1!
       out.println("Coreference information");
       Map<Integer, CorefChain> corefChains =
	   annotation.get(CorefCoreAnnotations.CorefChainAnnotation.class);
       if (corefChains == null) { return; }
       for (Map.Entry<Integer,CorefChain> entry: corefChains.entrySet()) {
	 out.println("Chain " + entry.getKey() + " ");
	 for (CorefChain.CorefMention m : entry.getValue().getMentionsInTextualOrder()) {
	   // We need to subtract one since the indices count from 1 but the Lists start from 0
	   List<CoreLabel> tokens = sentences.get(m.sentNum - 1).get(CoreAnnotations.TokensAnnotation.class);
	   // We subtract two for end: one for 0-based indexing, and one because we want last token of mention not one following.
	   out.println("  " + m + ", i.e., 0-based character offsets [" + tokens.get(m.startIndex - 1).beginPosition() +
		   ", " + tokens.get(m.endIndex - 2).endPosition() + ")");
	 }
       }
     }
     IOUtils.closeIgnoringExceptions(out);
     IOUtils.closeIgnoringExceptions(xmlOut);
   }

  public static void main3(String[] args) {
     String treeString = "(ROOT  (S (NP (PRP$ My) (NN dog)) (ADVP (RB also)) (VP (VBZ likes) (S (VP (VBG eating) (NP (NN sausage))))) (. .)))";
     // Typically the tree is constructed by parsing or reading a
     // treebank.  This is just for example purposes
     Tree tree = Tree.valueOf(treeString);

     // This creates English uncollapsed dependencies as a
     // SemanticGraph.  If you are creating many SemanticGraphs, you
     // should use a GrammaticalStructureFactory and use it to generate
     // the intermediate GrammaticalStructure instead
     SemanticGraph graph = SemanticGraphFactory.generateUncollapsedDependencies(tree);

     // Alternatively, this could have been the Chinese params or any
     // other language supported.  As of 2014, only English and Chinese
     EnglishTreebankParserParams params = new EnglishTreebankParserParams();
     GrammaticalStructureFactory gsf = params.treebankLanguagePack().grammaticalStructureFactory(params.treebankLanguagePack().punctuationWordRejectFilter(), params.typedDependencyHeadFinder());

     GrammaticalStructure gs = gsf.newGrammaticalStructure(tree);

     System.err.println(graph);

     SemgrexPattern semgrex = SemgrexPattern.compile("{}=A <<nsubj {}=B");
     SemgrexMatcher matcher = semgrex.matcher(graph);
     // This will produce two results on the given tree: "likes" is an
     // ancestor of both "dog" and "my" via the nsubj relation
     while (matcher.find()) {
       System.err.println(matcher.getNode("A") + " <<nsubj " + matcher.getNode("B"));
     }
   }

}
