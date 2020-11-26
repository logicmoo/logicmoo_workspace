/*	wsdlcpp.java

The contents of this file are subject to the MPL 1.1 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/wsdllicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developers of the Original Code are Kiran Kaja and Robert A. van Engelen.
Copyright (C) 2000-2002 Kiran Kaja and Robert A. van Engelen. All Rights Reserved.

*/


import java.util.*;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class NsNodeSearch
{

    Node startNode=null;
    String searchString=null;
    Hashtable searchAttributes=null;
    Hashtable namespace=null;
    

    Vector nodeList = new Vector();
    int currentPosition=0;

    boolean firsttime=true;

    public NsNodeSearch(String search)
    {
	searchString = search;
    }

    public NsNodeSearch(Node node)
    {
	startNode = node;
    }
    
    public NsNodeSearch(Node node,String search, Hashtable ns)
    {
	startNode = node;
	searchString = search;
	namespace = ns;
    }

    public NsNodeSearch(Node node,String search,Hashtable attributes,Hashtable ns)
    {
	startNode = node;
	searchString = search;
	searchAttributes = attributes;
	namespace = ns;
    }
    
    public void setElementNode(Node node)
    {
	startNode = node;
    }

    public void setSearchString(String search)
    {
	searchString = search;
    }

    public void setSearchAttributes(Hashtable attributes)
    {
	searchAttributes = attributes;
    }
    

    public int getTotalMatches()
    {
	return nodeList.size();
    }

    public NsNode getNextPartialMatch()
    {
	if(firsttime)
	    {
		firsttime=false;
		
		if(startNode==null||searchString==null) return null;

		if(searchAttributes==null)
		    {
			//System.out.println("called no attributes one");
			findAllElementPartialMatchNode(startNode,searchString,new Hashtable(namespace));
		    }
		else
		    {
			//			System.out.println("called attributes one");
			findAllElementPartialMatchNode(startNode,searchString,searchAttributes,new Hashtable(namespace));
		    }

		//System.out.println("the vector is intitlized to size "+nodeList.size());
	    }

	if(nodeList.size() > currentPosition)
	    return (NsNode) nodeList.elementAt(currentPosition++);
	else
	    return null;
    }
    

    
    public static Hashtable setNameSpaces(Node node,Hashtable ns)
    {
	String definitionNS=null;
	String wsdlNS=null;
	String xsdNS=null;
	String soapNS=null;
	String docNS=null;
	String encodingNS=null;
	String tns=null;
	String defaultNS=null;
	String defans=null;
	
	NamedNodeMap attributes = node.getAttributes();

	/*	System.out.println("in setname space");*/
	
	for (int i=0; (attributes!=null)&&(i<attributes.getLength()); i++)
	    {
		Node current = attributes.item(i);

		/*System.out.println(current.getNodeName());*/
		
		if(current.getNodeName().compareToIgnoreCase("targetNamespace")==0)
		    {
			tns =  current.getNodeValue();
			continue;
		    }

		if(current.getNodeName().compareToIgnoreCase("xmlns")==0)
		    {
			defans =  current.getNodeValue();
			continue;
		    }

		if(current.getNodeName().startsWith("xmlns:"))
		    {
			//put all name space def in the hash table

			ns.put(current.getNodeName(),current.getNodeValue());
			
			if(current.getNodeValue().endsWith("/wsdl/"))
			    {
				StringTokenizer st = new StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					wsdlNS =new String(st.nextToken()+":");
					/*System.out.println("wsdl found" + wsdlNS);*/
				    }
				else
				    {
					//					System.out.println("Found wsdl name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
			
			if(current.getNodeValue().endsWith("/soap/"))
			    {
				StringTokenizer st = new StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					soapNS =new String(st.nextToken()+":");
					/*System.out.println("soap found"+ soapNS);*/
				    }
				else
				    {
					//System.out.println("Found soap name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
			
			if(current.getNodeValue().endsWith("/soap/encoding/"))
			    {
				StringTokenizer st = new
				    StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					encodingNS =new String(st.nextToken()+":");
					/*System.out.println("encoding found"+encodingNS);*/
				    }
				else
				    {
					//System.out.println("Found encoding name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
		
			if(current.getNodeValue().endsWith("/XMLSchema"))
			    {
				StringTokenizer st = new
				    StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					xsdNS =new String(st.nextToken()+":");
					/*System.out.println("xml schema"+xsdNS);*/
				    }
				else
				    {
					//System.out.println("Found xsd name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
		    }
	    }




	if(wsdlNS!=null)
	    ns.put(new String("wsdlNS"),wsdlNS);
	
	if(xsdNS!=null)
	    ns.put("xsdNS",xsdNS);
	
	if(soapNS!=null)
	    ns.put("soapNS",soapNS);
	
	if(docNS!=null)
	    ns.put("docNS",docNS);
	
	if(encodingNS!=null)
	    ns.put("encodingNS",encodingNS);
	
	if(tns!=null)
	    ns.put("tns",tns);

	
	
	if(tns!=null)
	    {
		for (Enumeration e = ns.keys() ; e.hasMoreElements() ;)
		    {
			String curkey=(String)e.nextElement();
			String namesp = (String)ns.get(curkey);

			if(namesp.equals(tns))
			    {
				StringTokenizer st = new StringTokenizer(curkey,":");

				/*				System.out.println("found TNS"+*/
				if(st.countTokens()==2)
				    {
					st.nextToken();
					definitionNS =new String(st.nextToken()+":");
					break;
				    }
				else
				    {
					//System.out.println("Found default name space but name has more than 2 elements on tokenizer :");
					
				    }

			    }
		    }
 

	    }
	else
	    {
		//System.out.println("TNS not found");
	    }
    
	if(definitionNS!=null)
	    ns.put(new String("definitionNS"),definitionNS);
	

	if(defans!=null)
	    {
		for (Enumeration e = ns.keys() ; e.hasMoreElements() ;)
		    {
			String curkey=(String)e.nextElement();
			String namesp = (String)ns.get(curkey);

			if(namesp.equals(defans))
			    {
				StringTokenizer st = new StringTokenizer(curkey,":");

				/*				System.out.println("found TNS"+*/
				if(st.countTokens()==2)
				    {
					st.nextToken();
					defaultNS =new String(st.nextToken()+":");
					break;
				    }
				else
				    {
					//System.out.println("Found default name space but name has more than 2 elements on tokenizer :");
					
				    }

			    }
		    }
 

	    }
	else
	    {
		//System.out.println("TNS not found");
	    }

	
	if(defaultNS!=null)
	    ns.put(new String("defaultNS"),defans);
	
	return ns;
    }
    

    public void findAllElementPartialMatchNode(Node node, String
					       name, Hashtable ns)
    {
	if(node==null||name==null) return ;
	
	Node rnode=null;

	ns = setNameSpaces(node,ns);

	//	System.out.println("name - "+node.getNodeName());

	if(node.getNodeType()==Node.ELEMENT_NODE)
	    {
		if(node.getNodeName().endsWith(name))
		    {
			nodeList.add(new NsNode(node,ns));
		    }
	    }
	
	NodeList nodes = node.getChildNodes();
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementPartialMatchNode(nodes.item(i),name,new
			    Hashtable(ns));
	    }
    }


    public void findAllElementPartialMatchNode(Node node,String
					       name,Hashtable
					       attributes,Hashtable ns)
    //vector will contain attribute in even position and values in odd position
    {
	String value,key,gotValue;
	Node rnode=null;
	Enumeration keyList;

	
	if(node==null||name==null||attributes==null) return;

	ns = setNameSpaces(node,ns);
	
	keyList = attributes.keys();
	
	if(node.getNodeName().endsWith(name))
	    {
		boolean fail=false;
		
		while(keyList.hasMoreElements()) //check if the attribute values match 
		    {
			key = (String)keyList.nextElement();
			value = (String)attributes.get(key);
			gotValue = getAttrValue(node,key);
			if(gotValue!=null)
			    {
				if(gotValue.compareTo(value)!=0)
				    {
					fail = true;
					break;
				    }
			    }
			else
			    {
				fail = true;
				break;
			    }
		    }
		
		if(!fail)
		    {
			nodeList.add(new NsNode(node,ns));
		    }
	    }

	NodeList nodes = node.getChildNodes();
	
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementPartialMatchNode(nodes.item(i),name,attributes,new Hashtable(ns));
	    }
    
    }

    

    
    public NsNode getNextNode()
    {
	if(firsttime)
	    {
		firsttime=false;
		
		if(startNode==null||searchString==null) return null;
		if(searchAttributes==null)
		    {
			//System.out.println("called no attributes one");
			findAllElementNode(startNode,searchString,new Hashtable(namespace));
		    }
		else
		    {
			//			System.out.println("called attributes one");
			findAllElementNode(startNode,searchString,searchAttributes,new Hashtable(namespace));
		    }

		//System.out.println("the vector is intitlized to size "+nodeList.size());
	    }
	if(nodeList.size() > currentPosition)
	    return (NsNode) nodeList.elementAt(currentPosition++);
	else
	    return null;

    }

    public void findAllElementNode(Node node,String name,Hashtable ns)
    {
	if(node==null||name==null) return ;
	
	Node rnode=null;

	//	System.out.println("name - "+node.getNodeName());

	ns = setNameSpaces(node,ns);
	
	if(node.getNodeType()==Node.ELEMENT_NODE)
	    {
		if(node.getNodeName().compareToIgnoreCase(name)==0)
		    {
			nodeList.add(new NsNode(node,ns));
		    }
	    }
	
	NodeList nodes = node.getChildNodes();
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementNode(nodes.item(i),name,new Hashtable(ns));
	    }
    }
    
    public void findAllElementNode(Node node,String name,Hashtable
				   attributes,Hashtable ns)
    //vector will contain attribute in even position and values in odd position
    {
	String value,key,gotValue;
	Node rnode=null;
	Enumeration keyList;

	if(node==null||name==null||attributes==null) return;


	ns = setNameSpaces(node,ns);
	
	keyList = attributes.keys();
	
	if(node.getNodeName().compareToIgnoreCase(name)==0)
	    {
		boolean fail=false;
		
		while(keyList.hasMoreElements()) //check if the attribute values match 
		    {
			key = (String)keyList.nextElement();
			value = (String)attributes.get(key);
			gotValue = getAttrValue(node,key);
			if(gotValue!=null)
			    {
				if(gotValue.compareTo(value)!=0)
				    {
					fail = true;
					break;
				    }
			    }
			else
			    {
				fail = true;
				break;
			    }
		    }
		
		if(!fail)
		    {
			nodeList.add(new NsNode(node,ns));
		    }
	    }

	NodeList nodes = node.getChildNodes();
	
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementNode(nodes.item(i),name,attributes,new Hashtable(ns));
	    }
    
    }




    public String getAttrValue(Node node, String attribute)
    {
	NamedNodeMap attributes = node.getAttributes();
	for (int i=0; i<attributes.getLength(); i++)
	    {
		Node current = attributes.item(i);
		if(current.getNodeName().compareToIgnoreCase(attribute)==0)
		    {
			return current.getNodeValue();
		    }
		}
	return null;
    }
}
