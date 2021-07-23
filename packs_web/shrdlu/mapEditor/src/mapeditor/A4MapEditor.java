/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor;

import mapeditor.editortabs.MapVizPanel;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileWriter;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import mapeditor.editortabs.EditorTab;
import mapeditor.maps.A4Map;
import mapeditor.editortabs.MapPropertiesTab;
import mapeditor.editortabs.MapScriptsTab;
import mapeditor.editortabs.ObjectLayersTab;
import mapeditor.editortabs.ObjectClassTab;
import mapeditor.editortabs.TileLayersTab;
import mapeditor.editortabs.TileSetTab;
import mapeditor.maps.A4Object;
import mapeditor.maps.A4ObjectClass;
import mapeditor.maps.ObjectLayer;
import mapeditor.maps.TileLayer;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import util.XMLWriter;

/**
 *
 * @author santi
 */
public class A4MapEditor extends JPanel {
    public static final int N_TOOLS = 4;
    
    public static final int TOOL_TILES = 0;
    public static final int TOOL_REMOVE_TILES = 1;
    public static final int TOOL_ADD_OBJECT = 2;
    public static final int TOOL_SELECT_OBJECTS = 3;
    
    public static final int INITIAL_WINDOW_WIDTH = 800;
    public static final int INITIAL_MAP_AREA_HEIGHT = 480;
    public static final int INITIAL_TAB_AREA_HEIGHT = 240;
    public static final int INITIAL_WINDOW_HEIGHT = INITIAL_MAP_AREA_HEIGHT + INITIAL_TAB_AREA_HEIGHT;
    
    
    A4Map m_map = null;
    
    JRadioButton m_tools[] = {null,null,null,null};
    JTabbedPane m_tabbedPane = null;
    MapVizPanel m_mapViz = null;
    JScrollPane m_mapVizScroll = null;
    TileSetTab m_tileSetTab = null;
    TileLayersTab m_tileLayersTab = null;
    ObjectClassTab m_objectTypeTab = null;
    ObjectLayersTab m_objectLayersTab = null;
    MapScriptsTab m_scriptsTab = null;
    
    JFileChooser m_chooser = new JFileChooser();
    
    public static void main(String args[]) {
        JFrame frame = new JFrame("A4 Engine Map Editor (beta)");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);         
        frame.add(new A4MapEditor(), BorderLayout.CENTER);
        frame.pack();
        frame.setVisible(true);
    }       
    
    public A4MapEditor() {
        m_map = new A4Map("a map", 32, 20, 32, 32);
        
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setPreferredSize(new Dimension(INITIAL_WINDOW_WIDTH,INITIAL_WINDOW_HEIGHT));
        add(generateMapComponent());
        add(generatePropertiesTabbedComponent());        
    }
    
    public JComponent generateMapComponent() {
        JPanel component = new JPanel();
        component.setLayout(new BoxLayout(component, BoxLayout.Y_AXIS));
        component.setPreferredSize(new Dimension(INITIAL_WINDOW_WIDTH,INITIAL_MAP_AREA_HEIGHT));
        
        JPanel buttonRow = new JPanel();
        buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
        
        JButton button1 = new JButton("New");
        button1.setAlignmentX(Component.CENTER_ALIGNMENT);
        button1.setAlignmentY(Component.TOP_ALIGNMENT);
        button1.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Object[] options = {"Ok",
                                    "Cancel"};
                int n = JOptionPane.showOptionDialog(component,
                    "Create a new map, discarding the current map?",
                    "Confirmation",
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    options,
                    options[1]);
                if (n==0) {
                    m_map = new A4Map("a map", 32, 20, 32, 32);
                    refreshAllPanes();
                }
            }
        });
        buttonRow.add(button1);

        JButton button2 = new JButton("Open");
        button2.setAlignmentX(Component.CENTER_ALIGNMENT);
        button2.setAlignmentY(Component.TOP_ALIGNMENT);
        button2.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Object[] options = {"Ok",
                                    "Cancel"};
                int n = JOptionPane.showOptionDialog(component,
                    "Load a map, discarding the current map?",
                    "Confirmation",
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    options,
                    options[1]);
                if (n==0) {
                    FileNameExtensionFilter filter = new FileNameExtensionFilter(
                        "Map files", "tmx", "xml");
                    m_chooser.setFileFilter(filter);
                    int returnVal = m_chooser.showOpenDialog(component);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            SAXBuilder builder = new SAXBuilder();
                            Element root = builder.build(m_chooser.getSelectedFile().getAbsolutePath()).getRootElement();
                            String path = m_chooser.getSelectedFile().getPath();
                            path = path.substring(0,path.lastIndexOf(File.separator));
                            A4Map map = new A4Map(root, path);
                            m_map = map;
                            refreshAllPanes();
                        } catch (Exception exception) {
                            JOptionPane.showMessageDialog(component, "Could not load the file!\n" + 
                                                                     exception.getClass().getSimpleName() + ": " + exception.getMessage());
                        }
                    }
                }
            }
        });
        buttonRow.add(button2);
        
        JButton button3 = new JButton("Save");
        button3.setAlignmentX(Component.CENTER_ALIGNMENT);
        button3.setAlignmentY(Component.TOP_ALIGNMENT);
        button3.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                FileNameExtensionFilter filter = new FileNameExtensionFilter(
                    "Map files", "tmx", "xml");
                m_chooser.setFileFilter(filter);
                int returnVal = m_chooser.showSaveDialog(component);
                if(returnVal == JFileChooser.APPROVE_OPTION) {
                    String oldPath = m_map.getPath();
                    try {
                        String name = m_chooser.getSelectedFile().getCanonicalPath();
                        String path = name.substring(0, name.lastIndexOf(File.separator));
                        XMLWriter writer = new XMLWriter(new FileWriter(name));
                        m_map.setPath(path);
                        m_map.saveToXML(writer);
                        writer.close();
                    } catch(Exception exception) {
                        m_map.setPath(oldPath);
                        JOptionPane.showMessageDialog(component, "Could not save the file!\n" + exception.getMessage());
                    }
                }
            }
        });
        buttonRow.add(button3);
        
        {
            String tools[] = {"Tiles", "Erase", "Add Object", "Select Objects"};
            ButtonGroup group = new ButtonGroup();
            for(int i = 0;i<4;i++) {
                JRadioButton b = new JRadioButton(tools[i]);
                m_tools[i] = b;
                b.setActionCommand(tools[i]);
                b.setSelected(false);
                group.add(b);
                buttonRow.add(b);
            }
        }        

        component.add(buttonRow);
        
        m_mapViz = new MapVizPanel(this);
        m_mapVizScroll = new JScrollPane(m_mapViz, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        component.add(m_mapVizScroll);        
        
        return component;
    }
    
    
    public JComponent generatePropertiesTabbedComponent() {
        m_tabbedPane = new JTabbedPane();
        m_tabbedPane.setPreferredSize(new Dimension(INITIAL_WINDOW_WIDTH,INITIAL_TAB_AREA_HEIGHT));
        
        EditorTab panel1 = new MapPropertiesTab(this);
        m_tabbedPane.addTab("Properties", null, panel1, "Edit the map global properties.");
        m_tileSetTab = new TileSetTab(this);         
        m_tabbedPane.addTab("Tileset", null, m_tileSetTab, "Add/remove graphic files to the tileset.");
        m_tileLayersTab = new TileLayersTab(this);         
        m_tabbedPane.addTab("Tile Layers", null, m_tileLayersTab, "Add/remove/select tile layers from the map.");
        m_objectTypeTab = new ObjectClassTab(this);         
        m_tabbedPane.addTab("Object Types", null, m_objectTypeTab, "Add/remove/select object types.");
        m_objectLayersTab = new ObjectLayersTab(this);         
        m_tabbedPane.addTab("Object Layers", null, m_objectLayersTab, "Add/remove/select object layers from the map.");
        m_scriptsTab = new MapScriptsTab(this);
        m_tabbedPane.addTab("Scripts", null, m_scriptsTab, "Add/remove/edit scripts from the map.");
        
        
        add(m_tabbedPane);        
        m_tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);   
        
        m_tabbedPane.addChangeListener(new ChangeListener() {
              public void stateChanged(ChangeEvent changeEvent) {
                JTabbedPane sourceTabbedPane = (JTabbedPane) changeEvent.getSource();
                EditorTab tab = (EditorTab)sourceTabbedPane.getSelectedComponent();
                tab.updateFromMap();
              }
            });        
        return m_tabbedPane;
    }
        
    
    public void refreshMapViz() {
        m_mapViz.updateFromMap();
        m_mapViz.revalidate();
        m_mapViz.repaint();
        m_mapVizScroll.revalidate();
        m_mapVizScroll.repaint();
    }
    
    
    public void refreshAllPanes() {
        for(Component c:m_tabbedPane.getComponents()) {
            EditorTab tab = (EditorTab)c;
            tab.updateFromMap();
        }
        refreshMapViz();
    }
    
    
    public A4Map getMap() {
        return m_map;
    }
    
    
    public int selectedTool() {
        for(int i = 0;i<N_TOOLS;i++) {
            if (m_tools[i].isSelected()) return i;
        }
        return -1;
    }
    
    public void setTool(int tool) {
        for(int i = 0;i<N_TOOLS;i++) {
            if (i==tool) {
                m_tools[i].setSelected(true);
            } else {
                m_tools[i].setSelected(false);
            }
        }
    }
    
    public TileSetTab getTileSetTab() {
        return m_tileSetTab;
    }

    public TileLayersTab getTileLayersTab() {
        return m_tileLayersTab;
    }    
    
    public MapVizPanel getMapVizTab() {
        return m_mapViz;
    }
    
    public A4Object getSelectedObject() {
        return m_objectLayersTab.getSelectedObject();
    }
    
    public void selectObject(ObjectLayer ol, A4Object mo) {
        m_objectLayersTab.selectObject(ol, mo);
    }
    
    public ObjectLayer getSelectedObjectLayer() {
        return m_objectLayersTab.getSelectedLayer();
    }
    
    public A4ObjectClass getSelectedObjectType() {
        return m_objectTypeTab.getSelectedObjectType();
    }

    public TileLayer getSelectedTileLayer() {
        return m_tileLayersTab.getSelectedLayer();
    }

    public int getSelectedTile() {
        return m_tileSetTab.getSelectedTile();
    }

}
