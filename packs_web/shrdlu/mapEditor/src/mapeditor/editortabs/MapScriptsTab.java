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
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import mapeditor.A4MapEditor;
import org.jdom.Element;

/**
 *
 * @author santi
 */
public class MapScriptsTab extends EditorTab {
    
    JList onStartListcomponent = null;
    DefaultListModel onStartList = new DefaultListModel();    

    JList storyStateRulesListcomponent = null;
    DefaultListModel storyStateRulesList = new DefaultListModel();    

    JList eventRulesListcomponent = null;
    DefaultListModel eventRulesList = new DefaultListModel();    
    
    public MapScriptsTab(A4MapEditor editor) {
        super(editor);
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        {
            JPanel onStartPanel = new JPanel();
            onStartPanel.setLayout(new BoxLayout(onStartPanel, BoxLayout.Y_AXIS));
            onStartPanel.add(new JLabel("On Start Scripts:"));
            onStartListcomponent = new JList(onStartList);
            onStartListcomponent.setPreferredSize(new Dimension(240,200));
            onStartPanel.add(onStartListcomponent);

            JPanel buttonRow = new JPanel();
            buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
            JButton add = new JButton("Add");
            add.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = new Element("onStart");
                    m_editor.getMap().addOnStart(e);
                    m_editor.refreshAllPanes();
                }
            });
            buttonRow.add(add);
        
            JButton remove = new JButton("Remove");
            remove.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedOnStart();
                    if (e!=null) {
                        m_editor.getMap().removeOnStart(e);
                        m_editor.refreshAllPanes();
                    }
                }
            });
            buttonRow.add(remove);

            JButton edit = new JButton("Edit");
            edit.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedOnStart();
                    if (e!=null) {
                        XMLTextEditor editor = new XMLTextEditor(e, m_editor, XMLTextEditor.ON_START_XML, m_editor.getMap());
                        editor.setVisible(true);
                    }
                }
            });
            buttonRow.add(edit);
            onStartPanel.add(buttonRow);
            add(onStartPanel);
        }
        {
            JPanel storyStateRulesPanel = new JPanel();
            storyStateRulesPanel.setLayout(new BoxLayout(storyStateRulesPanel, BoxLayout.Y_AXIS));
            storyStateRulesPanel.add(new JLabel("Story State Rules:"));
            storyStateRulesListcomponent = new JList(storyStateRulesList);
            storyStateRulesListcomponent.setPreferredSize(new Dimension(240,200));
            storyStateRulesPanel.add(storyStateRulesListcomponent);

            JPanel buttonRow = new JPanel();
            buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
            JButton add = new JButton("Add");
            add.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = new Element("storyStateRule");
                    e.setAttribute("scope", "map");
                    e.setAttribute("variable", "v");
                    e.setAttribute("value","1");
                    e.setAttribute("once","true");
                    m_editor.getMap().addStoryStateRule(e);
                    m_editor.refreshAllPanes();
                }
            });
            buttonRow.add(add);
        
            JButton remove = new JButton("Remove");
            remove.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedStoryStateRule();
                    if (e!=null) {
                        m_editor.getMap().removeStoryStateRule(e);
                        m_editor.refreshAllPanes();
                    }
                }
            });
            buttonRow.add(remove);

            JButton edit = new JButton("Edit");
            edit.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedStoryStateRule();
                    if (e!=null) {
                        XMLTextEditor editor = new XMLTextEditor(e, m_editor, XMLTextEditor.STORY_RULE_XML, m_editor.getMap());
                        editor.setVisible(true);
                    }
                }
            });
            buttonRow.add(edit);
            storyStateRulesPanel.add(buttonRow);
            add(storyStateRulesPanel);
        }
        {
            JPanel eventRulesPanel = new JPanel();
            eventRulesPanel.setLayout(new BoxLayout(eventRulesPanel, BoxLayout.Y_AXIS));
            eventRulesPanel.add(new JLabel("Event Rules:"));
            eventRulesListcomponent = new JList(eventRulesList);
            eventRulesListcomponent.setPreferredSize(new Dimension(240,200));
            eventRulesPanel.add(eventRulesListcomponent);

            JPanel buttonRow = new JPanel();
            buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
            JButton add = new JButton("Add");
            add.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = new Element("eventRule");
                    e.setAttribute("event", "timer");
                    e.setAttribute("time","0");
                    e.setAttribute("period","50");
                    m_editor.getMap().addEventRule(e);
                    m_editor.refreshAllPanes();
                }
            });
            buttonRow.add(add);
        
            JButton remove = new JButton("Remove");
            remove.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedEventRule();
                    if (e!=null) {
                        m_editor.getMap().removeEventRule(e);
                        m_editor.refreshAllPanes();
                    }
                }
            });
            buttonRow.add(remove);

            JButton edit = new JButton("Edit");
            edit.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    Element e = getSelectedEventRule();
                    if (e!=null) {
                        XMLTextEditor editor = new XMLTextEditor(e, m_editor, XMLTextEditor.EVENT_RULE_XML, m_editor.getMap());
                        editor.setVisible(true);
                    }
                }
            });
            buttonRow.add(edit);
            eventRulesPanel.add(buttonRow);
            add(eventRulesPanel);
        }

        updateFromMap();
    }
    
    
    public Element getSelectedOnStart() {
        return (Element)onStartListcomponent.getSelectedValue();
    }
    
    public Element getSelectedStoryStateRule() {
        return (Element)storyStateRulesListcomponent.getSelectedValue();
    }

    public Element getSelectedEventRule() {
        return (Element)eventRulesListcomponent.getSelectedValue();
    }
    
    public void updateFromMap() {
        Element selected = getSelectedOnStart();
        onStartList.clear();
        for(Element e:m_editor.getMap().getOnStart()) {
            onStartList.addElement(e);
            if (e == selected) {
                onStartListcomponent.setSelectedValue(e, true);
            }
        }
        
        selected = getSelectedStoryStateRule();
        storyStateRulesList.clear();
        for(Element e:m_editor.getMap().getStoryStateRules()) {
            storyStateRulesList.addElement(e);
            if (e == selected) {
                storyStateRulesListcomponent.setSelectedValue(e, true);
            }
        }
                
        selected = getSelectedEventRule();
        eventRulesList.clear();
        for(Element e:m_editor.getMap().getEventRules()) {
            eventRulesList.addElement(e);
            if (e == selected) {
                eventRulesListcomponent.setSelectedValue(e, true);
            }
        }
                
    }
    
}
