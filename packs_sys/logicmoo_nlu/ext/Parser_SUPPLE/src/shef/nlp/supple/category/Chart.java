package shef.nlp.supple.category;

import gate.FeatureMap;
import gate.util.Err;
import gate.util.SimpleFeatureMapImpl;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.ListIterator;

import shef.nlp.supple.SUPPLERecord;
import shef.nlp.supple.mapping.BrillToSUPPLE;
import shef.nlp.supple.utils.InvalidPosCategory;

public class Chart
{
	public Long sentence_ID;
	public ArrayList  edge_List;
	public Long next_edge_ID;
	public Long next_ID;

	public Long beginOffset;
	public Long getBeginOffset() { return beginOffset; }

	public Long endOffset;
	public Long getEndOffset() { return endOffset; }

	public Chart(Long id, Long begin, Long end)
	{
		Category buchartCategory;
		InitialEdge edge;
		long value;

		try
		{
			sentence_ID = id;
			beginOffset = begin;
			endOffset = end;
			edge_List = new ArrayList();
			next_edge_ID = new Long(0);
			next_ID = new Long(1);
			value = next_edge_ID.longValue();
			buchartCategory = BrillToSUPPLE.convertBrillToSUPPLE("TOP");
			edge = new InitialEdge(new Long(value++), new Long(value),
								   buchartCategory, begin, begin, getNextId(), 1);
			edge_List.add(edge);
			next_edge_ID = new Long(value);
		}
		catch(InvalidPosCategory ipc)
		{
			Err.println("TOP IS INVALID CATEGORY!");
		}
	}

	private Long getNextEdgeId()
	{
		Long aux = next_edge_ID;
		next_edge_ID = new Long(aux.longValue()+1);
		return aux;
	}

	private Long getNextId()
	{
		Long aux = next_ID;
		next_ID = new Long(aux.longValue()+1);
		return aux;
	}

	private void releaseNextId()
	{
		next_ID = new Long(next_ID.longValue()-1);
	}

	public void setNextListNP(Long beginPos, Long endPos, long lookupLength, String stringValue, String root, String affix, String text, String major, String minor)
	{
		InitialEdge edge;
		Category buchartCategory;
		FeatureMap buchartFeatures = new SimpleFeatureMapImpl();

		buchartFeatures.put("s_form",stringValue);

		buchartFeatures.put(Category.M_ROOT,root);
		buchartFeatures.put(Category.M_AFFIX,affix);
		buchartFeatures.put(Category.TEXT,text);

		String gender = "_";

		if (minor != null)
		{
			if (minor.indexOf("male") != -1)
			{
				gender = "masc";
				minor = "person_first";
				major = "person";
			}
			else if (minor.indexOf("female") != -1)
			{
				gender = "fem";
				minor = "person_first";
				major = "person";
			}
			else if (minor.equals("person_ambig"))
			{
				minor = "person_first";
				major = "person";
			}
		}

		buchartFeatures.put("ne_tag",major);
		buchartFeatures.put("gender",gender);

		if (minor != null)
			buchartFeatures.put("ne_type",minor);

		Long begin = new Long(next_edge_ID.longValue()-1);
		Long end = new Long(begin.longValue()+lookupLength);

		edge = new InitialEdge(begin, end, new Category("list_np", buchartFeatures), beginPos, endPos, getNextId(), 1);

		if (!edge_List.contains(edge))
			edge_List.add(edge);
		else
		{
			releaseNextId();
		}
	}

	public void setNextPos(String brillCat, Long beginPos, Long endPos, String stringValue, String root, String affix, String text )
	{
		InitialEdge edge;
		Category buchartCategory;
		FeatureMap buchartFeatures;
		String buchartCat;

		/* generation edge */

		try
		{
			buchartCategory = BrillToSUPPLE.convertBrillToSUPPLE(brillCat);
			buchartCategory.setFeatureValue(new FeatureValue("s_form",stringValue));

			/* updating root  */
			buchartCategory.setFeatureValue(new
											FeatureValue(Category.M_ROOT,root));

			/* updating affix */
			buchartCategory.setFeatureValue(new
											FeatureValue(Category.M_AFFIX,affix));

			/* updating text */
			buchartCategory.setFeatureValue(new
											FeatureValue(Category.TEXT,text));

			buchartCat = buchartCategory.getCategory();
			buchartFeatures = buchartCategory.getFeatures();

			/* Initial Edge */
			Long begin = getNextEdgeId();
			Long end = new Long(begin.longValue()+1);
			edge = new InitialEdge(begin, end, new Category(buchartCat, buchartFeatures), beginPos, endPos, getNextId(), 1);

			/* put it on chart */
			edge_List.add(edge);

		}
		catch(InvalidPosCategory ipc)
		{
			Err.println(brillCat+" is not a valid POS tag");
		}
	}

	public void setFinal()
	{
		Category buchartCategory;
		InitialEdge edge;
		long value = next_edge_ID.longValue();
		long id = next_ID.longValue();

		try
		{
			buchartCategory = BrillToSUPPLE.convertBrillToSUPPLE("BOTTOM");
			edge = new InitialEdge(new Long(value++), new Long(value),
								   buchartCategory, endOffset, endOffset, new Long(id), 1);
			next_edge_ID = new Long(value);
			next_ID = new Long(id);
			edge_List.add(edge);
		}
		catch(InvalidPosCategory ipc)
		{
			Err.println("BOTTOM IS INVALID CATEGORY!");
		}
	}

	public Chart(long id, long begin, long end)
	{
		this(new Long(id),new Long(begin), new Long(end));
	}

	public void setEdge(Edge edge)
	{
		edge_List.add(edge);
	}

	public void setNext(Long next)
	{
		next_edge_ID = next;
	}

	/*public static String toSUPPLEFormat(ArrayList list)
	{
		String output = "";
		InitialEdge edge;

		ListIterator ite = list.listIterator();
		int edge_count = list.size();
		if (!(edge_count==0))
		{
			for(int i=0; i < edge_count-1;i++)
			{
				edge = (InitialEdge) ite.next();
				output += edge.toBuchartFormat()+",\n\n";

			}
			edge = (InitialEdge) ite.next();
			try
			{
				output += edge.toBuchartFormat();
			}
			catch(Exception e)
			{
				Err.println("problems with edge "+edge);
			}
		}
		return output;
	}*/

	public String toSUPPLEFormat()
	{
		InitialEdge edge;
		String output = "chart(sentence_n:";
		output += sentence_ID.toString()+",";

		/* the edges */
		output += "edges:[";

		ListIterator ite = edge_List.listIterator();
		int edge_count = edge_List.size();
		if (!(edge_count==0))
		{
			for(int i=0; i < edge_count-1;i++)
			{
				edge = (InitialEdge) ite.next();
				output += edge.toSUPPLEFormat()+",\n\n";
			}
			edge = (InitialEdge) ite.next();
			try
			{
				output += edge.toSUPPLEFormat();
			}
			catch(Exception e)
			{
				Err.println("problems with edge "+edge);
			}
		}

		output += "],";
		output += "next_edge_number:"+(next_ID.longValue()+1)+").";

		return output;
	}

	public Chart(Long id, Long begin, Long end, SUPPLERecord topCategory) {
		InitialEdge edge;
		long value;
		sentence_ID = id;
		beginOffset = begin;
		endOffset = end;
		edge_List = new ArrayList();
		next_edge_ID = new Long(0);
		value = next_edge_ID.longValue();

		edge = new InitialEdge(new Long(value++), new Long(value),
							   topCategory, begin, begin, new Long(value), 1);
		edge_List.add(edge);
		next_edge_ID = new Long(value);






	}


	public void setFinal(SUPPLERecord bottomCategory) {

		Category buchartCategory;
		InitialEdge edge;
		long value = next_edge_ID.longValue();
		edge = new InitialEdge(new Long(value++), new Long(value),
							   bottomCategory, endOffset, endOffset, new Long(value++), 1);
		next_edge_ID = new Long(value);
		edge_List.add(edge);

	}


	/**
	 * updates the chart with a new initial edge associated with a category
	 * @param record
	 */

	public void setNext(SUPPLERecord record) {


		InitialEdge edge;


		/* take the begin and end of the annotation (these two exists in the annotation) */

		Long beginAN, endAN;
		beginAN=record.getStart();
		endAN=record.getEnd();

		/* Set an Initial Edge */

		Long begin = getNextEdgeId();
		Long end = new Long(begin.longValue()+1);
		edge = new InitialEdge(begin, end,
							   record,
							   beginAN, endAN, end, 1);


		/* put it on chart */

		edge_List.add(edge);

	} // setNext()


	/**
	 * Prints the chart using the featureTable to determine what features and values and the order
	 * in which they should be printed out
	 * @param featureTable
	 * @return
	 */
	public String toSUPPLEFormat(Hashtable featureTable) {
		InitialEdge edge;
		String output = "chart(sentence_n:";
		output += sentence_ID.toString()+",";


		/* the edges */

		output += "edges:[";

		ListIterator ite = edge_List.listIterator();
		int edge_count = edge_List.size();
		if (!(edge_count==0)) {
  for(int i=0; i < edge_count-1;i++)
  {
				edge = (InitialEdge) ite.next();
				output += edge.toSUPPLEFormat(featureTable)+",\n\n";
//          System.out.println("partial output "+output);
			}
			edge = (InitialEdge) ite.next();
			try {
				output += edge.toSUPPLEFormat(featureTable);
			} catch (Exception e) {
				System.out.println("problems with edge "+edge);
			}
		}

		/* try {
		output += cat.toBuchartFormat()+",";
		}
		catch(EmptyFeatureMap efm) {
		 System.out.println("Empty FM in Category");
	  
		}
	  */
		output += "],";
		output += "next_edge_number:"+next_edge_ID+").";

		return output;

	}



}
