/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import org.jdom.Element;

/**
 *
 * @author santi
 */
public class A4ObjectClass {
    String m_type, m_name;
    List<String> m_super = new ArrayList<>();
    Element m_xml;
 
    public A4ObjectClass(Element xml) {
        m_type = xml.getAttributeValue("class");
        m_name = xml.getAttributeValue("name");
        String superString = xml.getAttributeValue("super");
        if (superString!=null) {
            StringTokenizer st = new StringTokenizer(superString,", ");
            while(st.hasMoreTokens()) {
                m_super.add(st.nextToken());
            }
        }
        m_xml = xml;
    }
    
    
    public String getName() {
        return m_name;
    }
    
    public String toString() {
        return m_name + ": "+m_super;
    }
}
