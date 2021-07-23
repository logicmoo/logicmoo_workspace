/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mapeditor.A4MapEditor;
import mapeditor.maps.A4Object;
import mapeditor.maps.ObjectLayer;

/**
 *
 * @author santi
 */
public class ObjectLayersTab extends EditorTab {

    JList objectLayerListcomponent = null;
    DefaultListModel objectLayerList = new DefaultListModel();
    
    JList objectListcomponent = null;
    DefaultListModel objectList = new DefaultListModel();


    public ObjectLayersTab(A4MapEditor editor) {
        super(editor);
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        
        objectLayerListcomponent = new JList(objectLayerList);
        objectLayerListcomponent.setPreferredSize(new Dimension(240,200));
        objectLayerListcomponent.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    updateObjectList((ObjectLayer)objectLayerListcomponent.getSelectedValue());
                }
        });            
        leftPanel.add(objectLayerListcomponent);

        JPanel buttonRow = new JPanel();
        buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
        JButton add = new JButton("Add");
        add.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int n = 1;
                do{
                    String name = "Object Layer " + n;
                    if (m_editor.getMap().getObjectLayer(name)==null) {
                        m_editor.getMap().addObjectLayer(new ObjectLayer(name));
                        m_editor.refreshAllPanes();
                        break;
                    }
                    n++;
                }while(true);
            }
        });
        buttonRow.add(add);
        
        JButton remove = new JButton("Remove");
        remove.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                Object[] options = {"Ok",
                                    "Cancel"};
                int n = JOptionPane.showOptionDialog(remove,
                    "Delete the layer losing all of its content?",
                    "Confirmation",
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    options,
                    options[1]);
                if (n==0) {
                    ObjectLayer tl = (ObjectLayer)objectLayerListcomponent.getSelectedValue();
                    if (tl!=null) {
                        m_editor.getMap().removeObjectLayer(tl);
                        m_editor.refreshAllPanes();
                    }
                }
            }
        });
        buttonRow.add(remove);

        JButton hideshow = new JButton("Hide/Show");
        hideshow.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                ObjectLayer tl = (ObjectLayer)objectLayerListcomponent.getSelectedValue();
                if (tl!=null) {
                    tl.setShow(!tl.getShow());
                    m_editor.refreshAllPanes();
                }
            }
        });
        buttonRow.add(hideshow);
        
        leftPanel.add(buttonRow);
        add(leftPanel);

        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        
        objectListcomponent = new JList(objectList);
        objectListcomponent.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    m_editor.setTool(A4MapEditor.TOOL_SELECT_OBJECTS);
                    m_editor.refreshMapViz();
                }
        });            
        JScrollPane scroll = new JScrollPane(objectListcomponent);
        scroll.setPreferredSize(new Dimension(400,400));
        rightPanel.add(scroll);

        {
            JPanel objectButtonRow = new JPanel();
            objectButtonRow.setLayout(new BoxLayout(objectButtonRow, BoxLayout.X_AXIS));
            JButton editObject = new JButton("Edit");
            editObject.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    A4Object mo = getSelectedObject();
                    if (mo!=null) {
                        XMLTextEditor editor = new XMLTextEditor(mo.getXML(), m_editor, XMLTextEditor.OBJECT_XML, mo);
                        editor.setVisible(true);
                    }
                }
            });
            objectButtonRow.add(editObject);
            JButton removeObject = new JButton("Remove");
            removeObject.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    A4Object mo = getSelectedObject();
                    if (mo!=null) {
                        getSelectedLayer().removeObject(mo);
                        m_editor.refreshAllPanes();
                    }
                }
            });
            objectButtonRow.add(removeObject);
            rightPanel.add(objectButtonRow);
        }
        
        add(rightPanel);
        
        updateFromMap();
    }    
    
    
    public ObjectLayer getSelectedLayer() {
        return (ObjectLayer)objectLayerListcomponent.getSelectedValue();
    }
    
    
    public A4Object getSelectedObject() {
        return (A4Object)objectListcomponent.getSelectedValue();
    }
    
    
    public void selectObject(ObjectLayer ol, A4Object mo) {
        objectLayerListcomponent.setSelectedValue(ol, true);
        updateObjectList(ol);
        if (mo!=null) objectListcomponent.setSelectedValue(mo, true);
    }
    
    
    public void updateFromMap() {
        ObjectLayer selected_tl = (ObjectLayer)objectLayerListcomponent.getSelectedValue();
        boolean found = false;
        objectLayerList.clear();
        for(ObjectLayer tl:m_editor.getMap().getObjectLayers()) {
            objectLayerList.addElement(tl);
            if (tl == selected_tl) {
                objectLayerListcomponent.setSelectedValue(tl, true);
                found = true;
            }
        }
        if (!found) selected_tl = null;
        updateObjectList(selected_tl);
    }
    
    public void updateObjectList(ObjectLayer l) {
        objectList.clear();
        if (l!=null) {
            for(A4Object mo:l.getObjects()) {
                objectList.addElement(mo);
            }
        }
    }
    
}
