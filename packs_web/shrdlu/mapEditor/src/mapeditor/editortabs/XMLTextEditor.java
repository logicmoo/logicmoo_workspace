/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.io.StringReader;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import mapeditor.A4MapEditor;
import mapeditor.maps.A4Map;
import mapeditor.maps.A4Object;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 *
 * @author santi
 */
public class XMLTextEditor extends JFrame {
    public static final int OBJECT_XML = 1;
    public static final int ON_START_XML = 2;
    public static final int STORY_RULE_XML = 3;
    public static final int EVENT_RULE_XML = 4;
    
    Element m_originalXML = null;
    Element m_savedXML = null;
    SAXBuilder m_builder = null;
    JTextArea m_textArea = null;
    
    // objects to save the xml back to:
    int m_type;
    A4Object m_originalObject = null;
    A4Map m_originalMap = null;
    A4MapEditor m_editor = null;
            
    public XMLTextEditor(Element xml, A4MapEditor editor, int type, Object o) {
        m_originalXML = xml;
        m_editor = editor;
        m_type = type;
        switch(m_type) {
            case OBJECT_XML: m_originalObject = (A4Object)o; break;
            case ON_START_XML: m_originalMap = (A4Map)o; break;
            case STORY_RULE_XML: m_originalMap = (A4Map)o; break;
            case EVENT_RULE_XML: m_originalMap = (A4Map)o; break;
        }
        m_savedXML = null;
        m_builder = new SAXBuilder();
        
        XMLOutputter outp = new XMLOutputter();
        outp.getFormat().setTextMode(Format.TextMode.TRIM);
        outp.getFormat().setIndent("  ");
        outp.getFormat().setLineSeparator("\n");
        String xmlAsText = outp.outputString(m_originalXML);
        
        getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
        
        m_textArea = new JTextArea(xmlAsText, 16, 40);
        JScrollPane scrollingArea = new JScrollPane(m_textArea);
        add(scrollingArea);
        
        JPanel buttonRow = new JPanel();
        buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
        JButton ok = new JButton("Save");
        ok.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                try {
                    saveXML();
                }catch(Exception exception) {
                    JOptionPane.showMessageDialog(getContentPane(), "Error parsing XML!\n" + 
                                                                    exception.getClass().getSimpleName() + ": " + exception.getMessage());
                }
            }
        });
        buttonRow.add(ok);
        JButton close = new JButton("Save'n'Close");
        JFrame frame = this;
        close.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                try {
                    saveXML();
                    dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
                }catch(Exception exception) {
                    JOptionPane.showMessageDialog(getContentPane(), "Error parsing XML!\n" + 
                                                                    exception.getClass().getSimpleName() + ": " + exception.getMessage());
                }
            }
        });
        buttonRow.add(close);
        JButton cancel = new JButton("Close without saving");
        cancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
            }
        });
        buttonRow.add(cancel);
        add(buttonRow);
        
        pack();
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);         
    }
    
    
    public void saveXML() throws Exception {
        m_savedXML = m_builder.build(new StringReader(m_textArea.getText())).getRootElement();
        switch(m_type) {
            case OBJECT_XML: 
                if (!m_savedXML.getName().equals("object")) throw new Exception("top tag is not 'object'");
                if (m_savedXML.getAttributeValue("x")==null) throw new Exception("missing 'x' attribute in the top 'object' tag");
                if (m_savedXML.getAttributeValue("y")==null) throw new Exception("missing 'y' attribute in the top 'object' tag");
                if (m_savedXML.getAttributeValue("width")==null) throw new Exception("missing 'width' attribute in the top 'object' tag");
                if (m_savedXML.getAttributeValue("height")==null) throw new Exception("missing 'height' attribute in the top 'object' tag");
                m_originalObject.setXML(m_savedXML);
                m_originalXML = m_savedXML;
                break;
            case ON_START_XML: 
                if (!m_savedXML.getName().equals("onStart")) throw new Exception("top tag is not 'onStart'");
                {
                    List<Element> l = m_originalMap.getOnStart();
                    int idx = l.indexOf(m_originalXML);
                    l.set(idx, m_savedXML);
                    m_originalXML = m_savedXML;
                }
                break;
            case STORY_RULE_XML: 
                if (!m_savedXML.getName().equals("storyStateRule")) throw new Exception("top tag is not 'storyStateRule'");
                {
                    List<Element> l = m_originalMap.getStoryStateRules();
                    int idx = l.indexOf(m_originalXML);
                    l.set(idx, m_savedXML);
                    m_originalXML = m_savedXML;
                }
                break;
            case EVENT_RULE_XML: 
                if (!m_savedXML.getName().equals("eventRule")) throw new Exception("top tag is not 'eventRule'");
                {
                    List<Element> l = m_originalMap.getEventRules();
                    int idx = l.indexOf(m_originalXML);
                    l.set(idx, m_savedXML);
                    m_originalXML = m_savedXML;
                }
                break;
        }
        m_editor.refreshAllPanes();        
    }
}
