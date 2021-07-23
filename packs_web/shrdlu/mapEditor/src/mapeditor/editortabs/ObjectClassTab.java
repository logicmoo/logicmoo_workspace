/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import mapeditor.A4MapEditor;
import mapeditor.maps.A4ObjectClass;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 *
 * @author santi
 */
public class ObjectClassTab extends EditorTab {

    List<A4ObjectClass> m_baseClasses = new ArrayList<>();
    List<A4ObjectClass> m_objectClasses = new ArrayList<>();
    JList objectTypeListcomponent = null;
    DefaultListModel objectTypeList = new DefaultListModel();
    
    public ObjectClassTab(A4MapEditor editor) {
        super(editor);
        
        // add the base types:
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Item");
            c.setAttribute("name","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","CoinPurse");
            c.setAttribute("name","CoinPurse");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Potion");
            c.setAttribute("name","Potion");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","HPPotion");
            c.setAttribute("name","HPPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","MPPotion");
            c.setAttribute("name","MPPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","XPPotion");
            c.setAttribute("name","XPPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","StrengthPotion");
            c.setAttribute("name","StrengthPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","ConstitutionPotion");
            c.setAttribute("name","ConstitutionPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","LifePotion");
            c.setAttribute("name","LifePotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","PowerPotion");
            c.setAttribute("name","PowerPotion");
            c.setAttribute("super","Potion");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Key");
            c.setAttribute("name","Key");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Door");
            c.setAttribute("name","Door");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","EquipableItem");
            c.setAttribute("name","EquipableItem");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Food");
            c.setAttribute("name","Food");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Container");
            c.setAttribute("name","Container");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Spade");
            c.setAttribute("name","Spade");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Scroll");
            c.setAttribute("name","Scroll");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Wand");
            c.setAttribute("name","Wand");
            c.setAttribute("super","Item");
            m_baseClasses.add(new A4ObjectClass(c));
        }

        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","PushableWall");
            c.setAttribute("name","PushableWall");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","PressurePlate");
            c.setAttribute("name","PressurePlate");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Lever");
            c.setAttribute("name","Lever");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Trigger");
            c.setAttribute("name","Trigger");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","WalkingObject");
            c.setAttribute("name","WalkingObject");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Character");
            c.setAttribute("name","Character");
            c.setAttribute("super","WalkingObject");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Vehicle");
            c.setAttribute("name","Vehicle");
            c.setAttribute("super","WalkingObject");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","Bridge");
            c.setAttribute("name","Bridge");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        {
            Element c = new Element("ObjectClass");
            c.setAttribute("class","BridgeDestination");
            c.setAttribute("name","BridgeDestination");
            m_baseClasses.add(new A4ObjectClass(c));
        }
        m_objectClasses.addAll(m_baseClasses);
        
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        
        JPanel buttonColumn = new JPanel();
        buttonColumn.setLayout(new BoxLayout(buttonColumn, BoxLayout.Y_AXIS));
        
        JButton add = new JButton("Add Object Definition File");
        add.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                    JFileChooser chooser = new JFileChooser();
                    File pathFile = new File(m_editor.getMap().getPath());
                    chooser.setCurrentDirectory(pathFile);
                    FileNameExtensionFilter filter = new FileNameExtensionFilter(
                        "XML files", "xml");
                    chooser.setFileFilter(filter);
                    int returnVal = chooser.showOpenDialog(buttonColumn);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            SAXBuilder builder = new SAXBuilder();
                            Element root = builder.build(chooser.getSelectedFile().getAbsolutePath()).getRootElement();
                            
                            for(Object o:root.getChildren()) {
                                Element e = (Element)o;
                                if (e.getName().equals("ObjectClass") ||
                                    e.getName().equals("CharacterClass")) {
                                    // overwrite previous types (to ensure there are no duplicates):
                                    A4ObjectClass new_ot = new A4ObjectClass(e);
                                    A4ObjectClass ot = getObjectType(new_ot.getName());
                                    if (ot!=null) m_objectClasses.remove(ot);
                                    m_objectClasses.add(new_ot);
                                } else {
                                    throw new Exception("Unknown xml tag `" + e.getName() + "'");
                                }
                            }
                            
                            updateFromMap();
                        }catch(Exception e) {
                            JOptionPane.showMessageDialog(buttonColumn, "Could not load the file!\n" + e.getMessage());                            
                        }
                    }
            }
        });
        buttonColumn.add(add);    
        
        JButton remove = new JButton("Remove Object Type");
        remove.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                A4ObjectClass ot = (A4ObjectClass)objectTypeListcomponent.getSelectedValue();
                if (ot!=null) {
                    if (m_baseClasses.contains(ot)) {
                        JOptionPane.showMessageDialog(remove, "You cannot remove a base type."); 
                    } else {
                        Object[] options = {"Ok",
                                            "Cancel"};
                        int n = JOptionPane.showOptionDialog(remove,
                            "Delete the object type losing all of its content?",
                            "Confirmation",
                            JOptionPane.OK_CANCEL_OPTION,
                            JOptionPane.QUESTION_MESSAGE,
                            null,
                            options,
                            options[1]);
                        if (n==0) {
                            m_objectClasses.remove(ot);
                            m_editor.refreshAllPanes();
                        }
                    }
                }
            }
        });
        buttonColumn.add(remove);        
        add(buttonColumn);
        
        objectTypeListcomponent = new JList(objectTypeList);
        JScrollPane scroll = new JScrollPane(objectTypeListcomponent);
        scroll.setPreferredSize(new Dimension(400,400));
        add(scroll);
        
        
        updateFromMap();
    }    
    
    
    public A4ObjectClass getSelectedObjectType() {
        return (A4ObjectClass)objectTypeListcomponent.getSelectedValue();
    }
    
    
    public void updateFromMap() {
        A4ObjectClass selected_ot = (A4ObjectClass)objectTypeListcomponent.getSelectedValue();        
        objectTypeList.clear();
        for(A4ObjectClass ot:m_objectClasses) {
            objectTypeList.addElement(ot);
            if (ot == selected_ot) {
                objectTypeListcomponent.setSelectedValue(ot, true);
            }
        }        
    }
    
    
    public A4ObjectClass getObjectType(String name) {
        for(A4ObjectClass ot:m_objectClasses) {
            if (ot.getName().equals(name)) return ot;
        }
        return null;
    }
    
}
