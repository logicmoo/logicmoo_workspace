/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mapeditor.A4MapEditor;

/**
 *
 * @author santi
 */
public class MapPropertiesTab extends EditorTab {
    
    JTextField m_mapNameField = null;
    JTextField m_mapWidthField = null;
    JTextField m_mapHeightField = null;
    JTextField m_mapTileWidthField = null;
    JTextField m_mapTileHeightField = null;
        
    public MapPropertiesTab(A4MapEditor editor) {
        super(editor);
        SpringLayout layout = new SpringLayout();
        setLayout(layout);

        JLabel label = new JLabel("Map Name:");
        add(label);
        layout.putConstraint(SpringLayout.WEST, label, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, label, 5, SpringLayout.NORTH, this);
        JLabel previous = label;
        m_mapNameField = new JTextField(m_editor.getMap().getName(), 32);
        add(m_mapNameField);
        layout.putConstraint(SpringLayout.WEST, m_mapNameField, 5, SpringLayout.EAST, label);
        layout.putConstraint(SpringLayout.VERTICAL_CENTER, m_mapNameField, 0, SpringLayout.VERTICAL_CENTER, label);        

        label = new JLabel("Width:");
        add(label);
        layout.putConstraint(SpringLayout.WEST, label, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, label, 5, SpringLayout.SOUTH, previous);
        m_mapWidthField = new JTextField("" + m_editor.getMap().getWidth(), 8);
        add(m_mapWidthField);
        layout.putConstraint(SpringLayout.WEST, m_mapWidthField, 5, SpringLayout.EAST, label);
        layout.putConstraint(SpringLayout.VERTICAL_CENTER, m_mapWidthField, 0, SpringLayout.VERTICAL_CENTER, label);        
        previous = label;
        
        label = new JLabel("Height:");
        add(label);
        layout.putConstraint(SpringLayout.WEST, label, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, label, 5, SpringLayout.SOUTH, previous);
        m_mapHeightField = new JTextField("" + m_editor.getMap().getHeight(), 8);
        add(m_mapHeightField);
        layout.putConstraint(SpringLayout.WEST, m_mapHeightField, 5, SpringLayout.EAST, label);
        layout.putConstraint(SpringLayout.VERTICAL_CENTER, m_mapHeightField, 0, SpringLayout.VERTICAL_CENTER, label);        
        previous = label;

        label = new JLabel("Tile Width:");
        add(label);
        layout.putConstraint(SpringLayout.WEST, label, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, label, 5, SpringLayout.SOUTH, previous);
        m_mapTileWidthField = new JTextField("" + m_editor.getMap().getTileWidth(), 8);
        add(m_mapTileWidthField);
        layout.putConstraint(SpringLayout.WEST, m_mapTileWidthField, 5, SpringLayout.EAST, label);
        layout.putConstraint(SpringLayout.VERTICAL_CENTER, m_mapTileWidthField, 0, SpringLayout.VERTICAL_CENTER, label);        
        previous = label;

        label = new JLabel("Tile Height:");
        add(label);
        layout.putConstraint(SpringLayout.WEST, label, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, label, 5, SpringLayout.SOUTH, previous);
        m_mapTileHeightField = new JTextField("" + m_editor.getMap().getTileHeight(), 8);
        add(m_mapTileHeightField);
        layout.putConstraint(SpringLayout.WEST, m_mapTileHeightField, 5, SpringLayout.EAST, label);
        layout.putConstraint(SpringLayout.VERTICAL_CENTER, m_mapTileHeightField, 0, SpringLayout.VERTICAL_CENTER, label);        
        previous = label;
        
        JButton apply = new JButton("Apply");
        apply.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                editor.getMap().setName(m_mapNameField.getText());
                try{
                    editor.getMap().setWidth(Integer.parseInt(m_mapWidthField.getText()));
                }catch(Exception e) {
                }
                try{
                    editor.getMap().setHeight(Integer.parseInt(m_mapHeightField.getText()));
                }catch(Exception e) {
                }
                try{
                    editor.getMap().setTileWidth(Integer.parseInt(m_mapTileWidthField.getText()));
                }catch(Exception e) {
                }
                try{
                    editor.getMap().setTileHeight(Integer.parseInt(m_mapTileHeightField.getText()));
                }catch(Exception e) {
                }
                m_editor.refreshAllPanes();
            }
        });
        add(apply);
        layout.putConstraint(SpringLayout.WEST, apply, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.NORTH, apply, 5, SpringLayout.SOUTH, previous);

        updateFromMap();
    }
    
    
    public void updateFromMap() {
        m_mapNameField.setText(m_editor.getMap().getName());
        m_mapWidthField.setText("" + m_editor.getMap().getWidth());
        m_mapHeightField.setText("" + m_editor.getMap().getHeight());
        m_mapTileWidthField.setText("" + m_editor.getMap().getTileWidth());
        m_mapTileHeightField.setText("" + m_editor.getMap().getTileHeight());
    }
    
}
