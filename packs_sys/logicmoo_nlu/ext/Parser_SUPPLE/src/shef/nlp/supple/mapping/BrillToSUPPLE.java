package shef.nlp.supple.mapping;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeSet;

import shef.nlp.supple.category.Category;
import shef.nlp.supple.category.FeatureValue;
import shef.nlp.supple.utils.InvalidPosCategory;

/* Brill to Buchart is a Hash Table that is the basis for
conversion of POS tags to Buchart Categories for parsing

Key: String representing the POS tag
Value: Category (as defined by class Category)

*/
public class BrillToSUPPLE
{
	static Hashtable brillSUPPLETable = new Hashtable();

	static TreeSet POS = new TreeSet();

	static
	{
		POS.add("NN");
		POS.add("NNP");
		POS.add("NNPS");
		POS.add("NNS");
		POS.add("NP");
		POS.add("NPS");
		POS.add("JJ");
		POS.add("JJR");
		POS.add("JJS");
		POS.add("RB");
		POS.add("RBR");
		POS.add("RBS");
		POS.add("VB");
		POS.add("VBD");
		POS.add("VBG");
		POS.add("VBN");
		POS.add("VBZ");
		POS.add("VBP");
		POS.add("FW");
		POS.add("CD");
		POS.add("CC");
		POS.add("DT");
		POS.add("EX");
		POS.add("IN");
		POS.add("LS");
		POS.add("MD");
		POS.add("PDT");
		POS.add("POS");
		POS.add("PP");
		POS.add("PRP");
		POS.add("PRP$");
		POS.add("PRPR$");
		POS.add("RP");
		POS.add("TO");
		POS.add("UH");
		POS.add("WDT");
		POS.add("WP");
		POS.add("WP$");
		POS.add("WRB");
		POS.add("SYM");
		POS.add("PERIOD");
		POS.add("COMMA");
		POS.add("TOP");
		POS.add("BOTTOM");

		Category c;

		/* CONTENT OF THE TABLE FOR MAPPING Brown POS Tags to Buchart Categories */

		/* NN */
		c = new Category("n",Category.getDefaultFeatureMap("n"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","sing"));
		brillSUPPLETable.put("NN",c) ;

		/* NNS */
		c = new Category("n",Category.getDefaultFeatureMap("n"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","plural"));
		brillSUPPLETable.put("NNS",c) ;

		/* NNP */
		c = new Category("pn",Category.getDefaultFeatureMap("pn"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","sing"));
		brillSUPPLETable.put("NNP",c) ;

		/* NNPS */
		c = new Category("pn",Category.getDefaultFeatureMap("pn"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","plural"));
		brillSUPPLETable.put("NNPS",c) ;

		/* NP */
		c = new Category("pn",Category.getDefaultFeatureMap("pn"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","sing"));
		brillSUPPLETable.put("NP",c) ;

		/* NPS */
		c = new Category("pn",Category.getDefaultFeatureMap("pn"));
		c.setFeatureValue(new FeatureValue("person","3"));
		c.setFeatureValue(new FeatureValue("number","plural"));
		brillSUPPLETable.put("NPS",c) ;

		/* VB */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"none"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"base"));
		brillSUPPLETable.put("VB",c) ;

		/* VBD */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"past"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"dform"));
		brillSUPPLETable.put("VBD",c) ;

		/* VBG */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"present"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"gform"));
		brillSUPPLETable.put("VBG",c) ;

		/* VBN */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"past"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"nform"));
		brillSUPPLETable.put("VBN",c) ;

		/* VBP */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"present"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"sform"));
		brillSUPPLETable.put("VBP",c) ;

		/* VBZ */
		c = new Category("v",Category.getDefaultFeatureMap("v"));
		c.setFeatureValue(new FeatureValue(Category.PERSON,"3"));
		c.setFeatureValue(new FeatureValue(Category.NUMBER,"sing"));
		c.setFeatureValue(new FeatureValue(Category.TENSE,"present"));
		c.setFeatureValue(new FeatureValue(Category.VFORM,"sform"));
		brillSUPPLETable.put("VBZ",c) ;

		/* JJ */
		c = new Category("jj",Category.getDefaultFeatureMap("jj"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"base"));
		brillSUPPLETable.put("JJ",c) ;

		/* JJR */
		c = new Category("jj",Category.getDefaultFeatureMap("jj"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"comp"));
		brillSUPPLETable.put("JJR",c) ;

		/* JJS */
		c = new Category("jj",Category.getDefaultFeatureMap("jj"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"sup"));
		brillSUPPLETable.put("JJS",c) ;

		/* RB */
		c = new Category("rb",Category.getDefaultFeatureMap("rb"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"base"));
		brillSUPPLETable.put("RB",c) ;

		/* RBR */
		c = new Category("rb",Category.getDefaultFeatureMap("rb"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"comp"));
		brillSUPPLETable.put("RBR",c) ;

		/* RBS */
		c = new Category("rb",Category.getDefaultFeatureMap("rb"));
		c.setFeatureValue(new FeatureValue(Category.DEGREE,"sup"));
		brillSUPPLETable.put("RBS",c) ;

		/* PRP */
		c = new Category("pps",Category.getDefaultFeatureMap("pps"));
		brillSUPPLETable.put("PRP",c) ;

		/* PP$ */
		c = new Category("pps",Category.getDefaultFeatureMap("pps"));
		brillSUPPLETable.put("PP$",c) ;

		/* PRP$ */
		c = new Category("pps",Category.getDefaultFeatureMap("pps"));
		brillSUPPLETable.put("PRP$",c) ;

		/* PRPR$ */
		c = new Category("pps",Category.getDefaultFeatureMap("pps"));
		brillSUPPLETable.put("PRPR$",c) ;

		/* WP$ */
		c = new Category("wp",Category.getDefaultFeatureMap("wp"));
		brillSUPPLETable.put("WP$",c) ;

		/* TOP */
		c = new Category("top",Category.getDefaultFeatureMap("top"));
		brillSUPPLETable.put("TOP",c);

		/* BOTTOM */
		c = new Category("bottom",Category.getDefaultFeatureMap("bottom"));
		brillSUPPLETable.put("BOTTOM",c);

		/* PERIOD (.) */
		c = new Category("period",Category.getDefaultFeatureMap("period"));
		brillSUPPLETable.put("PERIOD",c);

		/* COMMA (,) */
		c = new Category("comma",Category.getDefaultFeatureMap("comma"));
		brillSUPPLETable.put("COMMA",c);

		/* SYM (other symbols) */
		c = new Category("sym",Category.getDefaultFeatureMap("sym"));
		brillSUPPLETable.put("SYM",c);

		/* CATEGORIES WITHOUT PARTICULAR VALUES */
		Iterator ite = POS.iterator();
		String pos;
		while(ite.hasNext())
		{
			pos = (String) ite.next();

			if (!brillSUPPLETable.containsKey(pos))
			{
			  c = new Category(pos.toLowerCase(),Category.getDefaultFeatureMap(pos.toLowerCase()));
			  brillSUPPLETable.put(pos,c);
			}
			else
			{
				c = (Category) brillSUPPLETable.get(pos);
			}
		}
	}

	public static void showBrillSUPPLETable()
	{
		Enumeration keys = brillSUPPLETable.keys();
		String key;
		Category c;
		while(keys.hasMoreElements())
		{
			key = (String) keys.nextElement();
			System.out.println(key);
			c = (Category) brillSUPPLETable.get(key);
			c.show();
		}
	}

	public static Category convertBrillToSUPPLE(String pos) throws InvalidPosCategory
	{
		Category category;
		String converted;

		if (pos.compareTo(".")==0)
			converted = "PERIOD";
		else  if (pos.compareTo(",")==0)
			converted = "COMMA";
		else if (!POS.contains(pos))
			converted = "SYM";
		else
			converted = pos;

		category = (Category) brillSUPPLETable.get(converted);

		return new Category(category.getCategory(), category.getFeatures());
	}
}