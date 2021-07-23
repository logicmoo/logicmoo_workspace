/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

import java.util.ArrayList;
import java.util.List;
import org.jdom.Element;

/**
 *
 * @author santi
 */
public class A4Object {
    int m_x, m_y, m_width, m_height;
    String m_class;
    Element m_xml;
    
    public A4Object(Element xml) {
        setXML(xml);
    }
    
    public String toString() {
        return m_class + ":(" + m_x + "," + m_y + "),(" + m_width + "," + m_height+ ")";
    }
    
    public int getX() {
        return m_x;
    }
    
    public int getY() {
        return m_y;
    }
    
    public int getWidth() {
        return m_width;
    }
    
    public int getHeight() {
        return m_height;
    }
    
    public Element getXML() {
        return m_xml;
    }
    
    public void setXML(Element xml) {
        m_xml = xml;
        m_class = xml.getAttributeValue("class");
        m_x = Integer.parseInt(xml.getAttributeValue("x"));
        m_y = Integer.parseInt(xml.getAttributeValue("y"));
        m_width = Integer.parseInt(xml.getAttributeValue("width"));
        m_height = Integer.parseInt(xml.getAttributeValue("height"));
    }
    
    public Element getDefaultAnimation() {
        String preferred[] = {"idle","closed"};
        List<Element> animations = getAnimations();
        if (animations.isEmpty()) return null;
        for(String id:preferred) {
            for(Element animation:animations) {
                String animation_id = animation.getAttributeValue("name");
                if (animation_id.equals(id)) return animation;
            }
        }
        return animations.get(0);
    }
    
    public List<Element> getAnimations() {
        List animations_xml = m_xml.getChildren("animation");
        List<Element> l = new ArrayList<>();
        for(Object o:animations_xml) {
            l.add((Element)o);
        }
        return l;
    }
}
