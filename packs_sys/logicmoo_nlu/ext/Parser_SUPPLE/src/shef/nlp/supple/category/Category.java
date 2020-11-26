package shef.nlp.supple.category;

import gate.FeatureMap;
import gate.creole.ExecutionException;
import gate.util.Err;
import gate.util.SimpleFeatureMapImpl;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeSet;

import shef.nlp.supple.utils.EmptyFeatureMap;
import shef.nlp.supple.utils.IllegalCategoryName;

public class Category
{
	static TreeSet ALL_CAT = new TreeSet();
	static TreeSet NE_CAT = new TreeSet();
	static TreeSet POS_CAT = new TreeSet();
	static Hashtable DEF_CAT = new Hashtable();

	/* DEFAULT FEATURE MAPS FOR POS AND SYNTACTIC CATEGORIES */
	static FeatureMap DEF_POS = new SimpleFeatureMapImpl();
	static FeatureMap N_POS = new SimpleFeatureMapImpl();
	static FeatureMap A_POS = new SimpleFeatureMapImpl();
	static FeatureMap V_POS = new SimpleFeatureMapImpl();
	static FeatureMap TOP_POS = new SimpleFeatureMapImpl();
	static FeatureMap BOTTOM_POS = new SimpleFeatureMapImpl();

	/* CONSTANT FEATURE NAMES VALID FOR BUCHART PARSING */
	public static String S_FORM = "s_form";
	public static String M_ROOT = "m_root";
	public static String M_AFFIX = "m_affix";
	public static String TEXT = "text";
	public static String EDGE = "edge";
	public static String SEM = "sem";
	public static String HEAD = "head";
	public static String SOURCE = "source";
	public static String PERSON = "person";
	public static String NUMBER = "number";
	public static String GENDER = "gender";
	public static String TENSE = "tense";
	public static String ASPECT = "aspect";
	public static String VOICE = "voice";
	public static String VFORM = "vform";
	public static String DEGREE = "degree";
	public static String NE_TAG = "ne_tag";
	public static String NE_TYPE = "ne_type";

	/* ANY VALUE */
	static String ANY = "_";
	static String EMPTY = "";
	static String BODY = "body";

	String name;
	public String getCategory() { return name; }
	public void  setCategory(String Name) { name = Name; }

	FeatureMap features = new SimpleFeatureMapImpl();
	public FeatureMap getFeatures() { return features; }
	public void setFeatures(FeatureMap Features) { features = Features; }

	static boolean is_ne(String type) { return NE_CAT.contains(type); }

	public boolean equals(Object o)
	{
		if (!(o instanceof Category)) return false;

		Category c = (Category)o;

		return name.equals(c.name) && features.equals(c.features);
	}

	static
	{
		/* FEATURES FOR DEFAULT POS CATEGORIES */
		DEF_POS.put(S_FORM,ANY);
		DEF_POS.put(M_ROOT,EMPTY);
		DEF_POS.put(M_AFFIX,EMPTY);
		DEF_POS.put(TEXT,"body");
		/* FEATURES FOR NOUN POS CATEGORIES */

		N_POS.put(S_FORM,ANY);
		N_POS.put(M_ROOT,EMPTY);
		N_POS.put(M_AFFIX,EMPTY);
		N_POS.put(TEXT,"body");
		N_POS.put(PERSON,ANY);
		N_POS.put(NUMBER,ANY);

		/* FEATURES FOR ADJ AND ADV CATEGORIES */
		A_POS.put(S_FORM,ANY);
		A_POS.put(M_ROOT,EMPTY);
		A_POS.put(M_AFFIX,EMPTY);
		A_POS.put(TEXT,"body");
		A_POS.put(DEGREE,ANY);

		/* FEATURES FOR VERB CATEGORIES */
		V_POS.put(S_FORM,ANY);
		V_POS.put(M_ROOT,EMPTY);
		V_POS.put(M_AFFIX,EMPTY);
		V_POS.put(TEXT,"body");
		V_POS.put(PERSON,ANY);
		V_POS.put(NUMBER,ANY);
		V_POS.put(TENSE,ANY);
		V_POS.put(VFORM,ANY);

		/* TOP */
		TOP_POS.put(S_FORM,"top");
		TOP_POS.put(M_ROOT,"top");
		TOP_POS.put(M_AFFIX,EMPTY);
		TOP_POS.put(TEXT,"body");

		/* BOTTOM */
		BOTTOM_POS.put(S_FORM,"bottom");
		BOTTOM_POS.put(M_ROOT,"bottom");
		BOTTOM_POS.put(M_AFFIX,EMPTY);
		BOTTOM_POS.put(TEXT,"body");

		/* SET OF POS CATS FOR BUCHART */
		POS_CAT.add("n");
		POS_CAT.add("pn");
		POS_CAT.add("v");
		POS_CAT.add("jj");
		POS_CAT.add("rb");
		POS_CAT.add("fw");
		POS_CAT.add("cd");
		POS_CAT.add("cc");
		POS_CAT.add("dt");
		POS_CAT.add("ex");
		POS_CAT.add("in");
		POS_CAT.add("ls");
		POS_CAT.add("md");
		POS_CAT.add("pdt");
		POS_CAT.add("pos");
		POS_CAT.add("pp");
		POS_CAT.add("pps");
		POS_CAT.add("rp");
		POS_CAT.add("to");
		POS_CAT.add("uh");
		POS_CAT.add("wdt");
		POS_CAT.add("wp");
		POS_CAT.add("wrb");
		POS_CAT.add("sym");
		POS_CAT.add("period");
		POS_CAT.add("comma");
		POS_CAT.add("top");
		POS_CAT.add("bottom");

		// NON POS CATS
		NE_CAT.add("list_np");

		/* ALL VALID CATEGORIES */
		ALL_CAT.addAll(NE_CAT);
		ALL_CAT.addAll(POS_CAT);

		Iterator ite_pos = POS_CAT.iterator();
		while(ite_pos.hasNext())
		{
			DEF_CAT.put(ite_pos.next(),DEF_POS);
		}

		/* DEFAULT SET OF CATEGORIES AND THEIR FEATURES */
		DEF_CAT.put("n",N_POS);
		DEF_CAT.put("v",V_POS);
		DEF_CAT.put("jj",A_POS);
		DEF_CAT.put("pn",N_POS);
		DEF_CAT.put("rb",A_POS);
		DEF_CAT.put("top",TOP_POS);
		DEF_CAT.put("bottom",BOTTOM_POS);
		DEF_CAT.put("list_np",DEF_POS);
	}

	public Category() {}

	public Category(String Name, FeatureMap Features)
	{
		name = Name;
		features = new SimpleFeatureMapImpl();
		features.putAll(Features);
	}

	public Category(String Name) throws IllegalCategoryName
	{
		if(!ALL_CAT.contains(Name))
		{
			throw new IllegalCategoryName(Name+" is invalid");
		}
		name = Name;
	}

	public static Category getDefaultCategory(String Name)
	{
		return new Category(Name,(FeatureMap) DEF_CAT.get(Name));
	}

	public static String quoteValue(String value)
	{
		String output = "";
		output = "\'";
		int len = value.length();
		for(int i=0; i<len; i++)
		{
			if(value.charAt(i)=='\'')
			{
				output += "''";
			}
			else if(value.charAt(i)=='\n')
			{
				output += " ";
			}
			else
				output +=  value.charAt(i);
		}

		output += '\'';
		return output;
	}

	public String toSUPPLEFormat() throws EmptyFeatureMap, ExecutionException
	{
		String cat;
		FeatureMap features;
		String output= "";

		cat = this.getCategory();
		features = this.getFeatures();

		/* FOR POS CATEGORIES */

		if (POS_CAT.contains(cat) )
		{
			output += cat;
			output += "(";

			output += S_FORM+":"+quoteValue((String) features.get(S_FORM))+",";
			output += M_ROOT+":"+quoteValue((String) features.get(M_ROOT))+",";
			output += M_AFFIX+":"+quoteValue((String) features.get(M_AFFIX))+",";
			output += TEXT+":"+quoteValue((String) features.get(TEXT));

			if (cat == "n" || cat == "pn")
			{
				output += ",";
				output += PERSON+":"+features.get(PERSON)+",";
				output += NUMBER+":"+features.get(NUMBER);
			}
			else if (cat == "v")
			{
				output += ",";
				output += PERSON+":"+features.get(PERSON)+",";
				output += NUMBER+":"+features.get(NUMBER);
				output += ",";
				output += TENSE+":"+features.get(TENSE)+",";
				output += VFORM+":"+features.get(VFORM);
			}
			else if (cat == "jj" | cat == "rb" )
			{
				output += ",";
				output += DEGREE+":"+features.get(DEGREE);
			}
			else
			{
				/* nothing else */
			}
		}
		else if (NE_CAT.contains(cat))
		{
			if (cat=="list_np")
			{
				output += cat;
				output += "(";

				output += S_FORM+":"+quoteValue((String) features.get(S_FORM))+",";
				output += M_ROOT+":"+quoteValue((String) features.get(M_ROOT))+",";
				output += M_AFFIX+":"+quoteValue((String) features.get(M_AFFIX))+",";
				output += TEXT+":"+quoteValue((String) features.get(TEXT))+",";
				output += NE_TAG+":"+quoteValue((String) features.get("ne_tag"))+",";

				if (features.get("ne_type") != null)
				{
					output += NE_TYPE+":"+quoteValue((String) features.get("ne_type"))+",";
				}

				output += GENDER+":"+quoteValue((String) features.get("gender"));
			}
			else
			{
				Err.println("There is no category "+cat);
			}
		}
		output += ")";

		return output;
	}

	public static FeatureMap getDefaultFeatureMap(String Name)
	{
		return (FeatureMap) DEF_CAT.get(Name);
	}

	public void emptyCategory(String Name) throws IllegalCategoryName
	{
		if (Name=="n")
		{
			this.setCategory("n");
			this.setFeatures(N_POS);
			return;
		}
		if (Name=="v")
		{
			this.setCategory("v");
			this.setFeatures(V_POS);
			return;
		}

		throw new IllegalCategoryName(Name+" is invalid");
	}

	public void setFeatureValue(FeatureValue fv)
	{
		this.features.put(fv.feature,fv.value);
	}

	public void show()
	{
		System.out.println("CAT: "+name);
		System.out.println("FEATURES: "+features);
	}

	public void showFeatureMap()
	{
		System.out.println("Features "+features);
	}
}


