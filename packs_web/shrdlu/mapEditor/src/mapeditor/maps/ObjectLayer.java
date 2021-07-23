/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author santi
 */
public class ObjectLayer {
    String m_name;
    boolean m_show;     // whether to show this in the editor, or hide it
    List<A4Object> m_objects;
    
    public ObjectLayer(String name) {
        m_name = name;
        m_show = true;
        m_objects = new ArrayList<>();
    }
    
    public String toString() {
        return getName() + (m_show ? "":" (hidden)");
    }

    public String getName() {
        return m_name;
    }
    
    public void addObject(A4Object o) {
        m_objects.add(o);
    }
    
    public void removeObject(A4Object o) {
        m_objects.remove(o);
    }
    
    public List<A4Object> getObjects() {
        return m_objects;
    }
    
    public boolean getShow() {
        return m_show;
    }
    
    public void setShow(boolean show) {
        m_show = show;
    }
    
}
