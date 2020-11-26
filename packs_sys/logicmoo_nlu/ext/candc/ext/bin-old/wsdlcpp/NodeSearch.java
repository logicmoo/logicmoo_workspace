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




public class NodeSearch
{

    Node startNode=null;
    String searchString=null;
    Hashtable searchAttributes=null;
    
    

    Vector nodeList = new Vector();
    int currentPosition=0;

    boolean firsttime=true;

    public NodeSearch(String search)
    {
	searchString = search;
    }

    public NodeSearch(Node node)
    {
	startNode = node;
    }
    
    public NodeSearch(Node node,String search)
    {
	startNode = node;
	searchString = search;
    }

    public NodeSearch(Node node,String search,Hashtable attributes)
    {
	startNode = node;
	searchString = search;
	searchAttributes = attributes;
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

    public Node getNextPartialMatch()
    {
	if(firsttime)
	    {
		firsttime=false;
		
		if(startNode==null||searchString==null) return null;

		if(searchAttributes==null)
		    {
			//System.out.println("called no attributes one");
			findAllElementPartialMatchNode(startNode,searchString);
		    }
		else
		    {
			//System.out.println("called attributes one");
			findAllElementPartialMatchNode(startNode,searchString,searchAttributes);
		    }

		//System.out.println("the vector is intitlized to size "+nodeList.size());
	    }

	if(nodeList.size() > currentPosition)
	    return (Node) nodeList.elementAt(currentPosition++);
	else
	    return null;
    }


    public void findAllElementPartialMatchNode(Node node,String name)
    {
	if(node==null||name==null) return ;
	
	Node rnode=null;

	if(node.getNodeType()==Node.ELEMENT_NODE)
	    {
		if(node.getNodeName().endsWith(name))
		    {
			nodeList.add(node);
		    }
	    }
	
	NodeList nodes = node.getChildNodes();
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementPartialMatchNode(nodes.item(i),name);
	    }
    }


    public void findAllElementPartialMatchNode(Node node,String name,Hashtable attributes)
    //vector will contain attribute in even position and values in odd position
    {
	String value,key,gotValue;
	Node rnode=null;
	Enumeration keyList;

	if(node==null||name==null||attributes==null) return;
	
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
			nodeList.add(node);
		    }
	    }

	NodeList nodes = node.getChildNodes();
	
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementPartialMatchNode(nodes.item(i),name,attributes);
	    }
    
    }

    
    public Node getNextNode()
    {
	if(firsttime)
	    {
		firsttime=false;
		
		if(startNode==null||searchString==null) return null;
		if(searchAttributes==null)
		    {
			//System.out.println("called no attributes one");
			findAllElementNode(startNode,searchString);
		    }
		else
		    {
			//			System.out.println("called attributes one");
			findAllElementNode(startNode,searchString,searchAttributes);
		    }

		//System.out.println("the vector is intitlized to size "+nodeList.size());
	    }

	if(nodeList.size() > currentPosition)
	    return (Node) nodeList.elementAt(currentPosition++);
	else
	    return null;

    }

    public void findAllElementNode(Node node,String name)
    {
	if(node==null||name==null) return ;
	
	Node rnode=null;

	//	System.out.println("name - "+node.getNodeName());

	if(node.getNodeType()==Node.ELEMENT_NODE)
	    {
		if(node.getNodeName().compareToIgnoreCase(name)==0)
		    {
			nodeList.add(node);
		    }
	    }
	
	NodeList nodes = node.getChildNodes();
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementNode(nodes.item(i),name);
	    }
    }
    
    public void findAllElementNode(Node node,String name,Hashtable attributes)
    //vector will contain attribute in even position and values in odd position
    {
	String value,key,gotValue;
	Node rnode=null;
	Enumeration keyList;

	if(node==null||name==null||attributes==null) return;
	
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
			nodeList.add(node);
		    }
	    }

	NodeList nodes = node.getChildNodes();
	
	if (nodes != null)
	    {
		for (int i=0; i<nodes.getLength(); i++)
		    if(nodes.item(i).getNodeType()==Node.ELEMENT_NODE)
			findAllElementNode(nodes.item(i),name,attributes);
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
