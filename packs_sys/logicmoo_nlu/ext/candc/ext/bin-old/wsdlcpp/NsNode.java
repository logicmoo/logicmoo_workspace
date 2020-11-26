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
import org.w3c.dom.Node;

public class NsNode 
{
    Node node;
    Hashtable namespaces;
    
    public NsNode(Node n)
    {
	node = n;
	namespaces = new Hashtable();
    }

    public NsNode(Node n, Hashtable ht)
    {
	node = n;
	namespaces = ht;
    }
    
    public void setNamespaces(Hashtable ht)
    {
	namespaces = ht;
    }

    public Node getNode()
    {
	return node;
    }

    public Hashtable getNameSpace()
    {
	return namespaces;
    }
}
